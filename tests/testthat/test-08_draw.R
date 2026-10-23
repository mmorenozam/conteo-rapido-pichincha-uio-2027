# Tests for R/08_draw.R.

# A small frame: 30 recintos in Quito urbano (one of them very large) and 10 in Mejía,
# with consecutive junta numbers per parroquia and sex, as in the distributivo.
fake_frame_8 <- function() {
  set.seed(8)
  f <- tibble::tibble(
    recinto_codigo = 1:40,
    estrato = factor(rep(c("Quito urbano", "Mejía"), c(30, 10)), levels = ESTRATOS),
    canton = rep(c(CANTON_QUITO, 70L), c(30, 10)),
    canton_nombre = rep(c("QUITO", "MEJIA"), c(30, 10)),
    parroquia_codigo = c(rep(1:3, each = 10), rep(4L, 10)),
    zona_codigo = 0L, zona = "ZONA",
    juntas_f = c(30L, sample(2:7, 39, replace = TRUE)),
    juntas_m = c(30L, sample(2:7, 39, replace = TRUE))
  ) |>
    dplyr::mutate(parroquia = paste("PARROQUIA", parroquia_codigo), recinto = paste("RECINTO", recinto_codigo),
                  juntas = juntas_f + juntas_m) |>
    dplyr::group_by(parroquia_codigo) |>
    dplyr::mutate(jun_finf = cumsum(juntas_f), jun_inif = jun_finf - juntas_f + 1L,
                  jun_finm = cumsum(juntas_m), jun_inim = jun_finm - juntas_m + 1L) |>
    dplyr::ungroup()
  f
}

test_that("frame_juntas gives one row per junta, in the sort order of the draw", {
  f <- fake_frame_8()
  j <- frame_juntas(f)
  expect_equal(nrow(j), sum(f$juntas))
  expect_equal(anyDuplicated(j[c("recinto_codigo", "sexo", "junta_num")]), 0L)
  expect_equal(unique(j$estrato), c("Quito urbano", "Mejía"))
  expect_true(all(j$quito == (j$estrato == "Quito urbano")))
  expect_false(is.unsorted(j$recinto_codigo[j$estrato == "Quito urbano"]))
})

test_that("frame_juntas stops when the junta ranges do not match the declared juntas", {
  f <- fake_frame_8()
  f$juntas[2] <- f$juntas[2] + 1L
  expect_error(frame_juntas(f), "junta ranges")
})

test_that("draw_list draws n_h juntas per stratum, at most 4 per recinto, reproducibly", {
  j <- frame_juntas(fake_frame_8())
  n_h <- c("Quito urbano" = 40L, "Mejía" = 10L)
  l1 <- draw_list(j, n_h, seed = 1L)
  l2 <- draw_list(j, n_h, seed = 1L)
  expect_identical(l1, l2)
  expect_equal(as.vector(table(l1$estrato)[names(n_h)]), unname(n_h))
  expect_lte(max(table(l1$recinto_codigo)), 4L)
  expect_equal(l1$numero, seq_len(nrow(l1)))
  # the weights add up to the juntas of each stratum
  expect_equal(as.vector(tapply(l1$peso, l1$estrato, sum)[names(n_h)]),
               as.vector(table(j$estrato)[names(n_h)]))
  # the large recinto is its own sub-stratum with exactly 4 juntas
  expect_equal(sum(l1$recinto_codigo == 1L), 4L)
  expect_true(all(l1$subestrato[l1$recinto_codigo == 1L] == "Quito urbano|R1"))
})

test_that("draw_lists adds the buffer and the waves of option A only", {
  j <- frame_juntas(fake_frame_8())
  disenos <- list(A_99 = list(n_h = c("Quito urbano" = 30L, "Mejía" = 8L)),
                  B_99 = list(n_h = c("Quito urbano" = 20L, "Mejía" = 6L)))
  olas <- list(A_99 = list(n_h = c("Quito urbano" = 15L, "Mejía" = 4L)))
  perd <- list(junta = stats::setNames(rep(0.10, length(ESTRATOS)), ESTRATOS), recinto = 0.03)
  l <- draw_lists(j, disenos, olas, perd, seed = 2L)
  colchon <- buffer_allocation(disenos$A_99$n_h, perd$junta, perd$recinto)
  expect_equal(as.vector(table(l$A_99$estrato)[names(colchon)]), unname(colchon))
  expect_true(all(l$A_99$ola %in% 1:2))
  expect_gt(sum(l$A_99$ola == 1L), 0L)
  expect_lt(sum(l$A_99$ola == 1L), nrow(l$A_99))
  expect_false("ola" %in% names(l$B_99))
  r <- summarise_lists(l, j)
  expect_equal(r$juntas[r$lista == "A_99" & r$estrato == "Mejía"], unname(colchon[["Mejía"]]))
  expect_equal(r$actas_alcaldia[r$lista == "A_99" & r$estrato == "Mejía"], 0L)
  expect_true(all(is.na(r$ola_1[r$opcion == "B"])))
})

test_that("write_list_workbooks writes one workbook per option", {
  skip_if_not_installed("openxlsx")
  j <- frame_juntas(fake_frame_8())
  disenos <- list(A_99 = list(n_h = c("Quito urbano" = 30L, "Mejía" = 8L)),
                  A_95 = list(n_h = c("Quito urbano" = 20L, "Mejía" = 6L)),
                  B_99 = list(n_h = c("Quito urbano" = 20L, "Mejía" = 6L)))
  olas <- list(A_99 = list(n_h = c("Quito urbano" = 15L, "Mejía" = 4L)),
               A_95 = list(n_h = c("Quito urbano" = 10L, "Mejía" = 4L)))
  perd <- list(junta = stats::setNames(rep(0.10, length(ESTRATOS)), ESTRATOS), recinto = 0.03)
  l <- draw_lists(j, disenos, olas, perd, seed = 3L)
  dir <- withr::local_tempdir()
  p <- write_list_workbooks(l, summarise_lists(l, j), dir, 3L, perd)
  expect_equal(basename(p), c("conteo_rapido_opcion_A_juntas.xlsx", "conteo_rapido_opcion_B_juntas.xlsx"))
  expect_equal(openxlsx::getSheetNames(p[1]), c("Léame", "Resumen", "Juntas 99 %", "Juntas 95 %"))
  a99 <- openxlsx::read.xlsx(p[1], sheet = "Juntas 99 %", sep.names = " ")
  expect_equal(nrow(a99), nrow(l$A_99))
  expect_true("Ola" %in% names(a99))
})
