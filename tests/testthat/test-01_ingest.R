# Tests for R/01_ingest.R.
# Part 1 uses small hand-made tables; part 2 checks the real 2023 data against
# the numbers established when the data were extracted (README, section 6).

# ---- part 1: hand-made tables ------------------------------------------------------

mini_dist <- tibble::tibble(
  parroquia_codigo = c(1L, 1L, 2L), zona_codigo = c(0L, 0L, 5L), recinto_codigo = c(10L, 11L, 20L),
  recinto = c("A", "B", "C"), parroquia = c("P1", "P1", "P2"),
  juntas_f = c(2L, 1L, 1L), juntas_m = c(2L, 1L, 0L),
  jun_inif = c(1L, 3L, 1L), jun_finf = c(2L, 3L, 1L),
  jun_inim = c(1L, 3L, 0L), jun_finm = c(2L, 3L, 0L)
)

test_that("expand_junta_ranges gives one row per junta and skips 0-0 ranges", {
  r <- expand_junta_ranges(mini_dist)
  expect_equal(nrow(r), 7) # A: 2F + 2M, B: 1F + 1M, C: 1F (men's range 0-0 skipped)
  expect_equal(sort(unique(r$recinto_codigo)), c(10L, 11L, 20L))
  expect_false(any(r$recinto_codigo == 20L & r$sexo == "M"))
})

test_that("invalid_junta_ranges flags declared juntas without a valid range", {
  d <- mini_dist
  d$juntas_m[3] <- 1L # declares one men's junta but the range is 0-0
  expect_equal(invalid_junta_ranges(d)$recinto_codigo, 20L)
  expect_equal(nrow(invalid_junta_ranges(mini_dist)), 0)
})

mini_juntas <- tibble::tibble(
  parroquia_codigo = 1L, parroquia = "P1", zona_codigo = 0L, zona = "Z",
  junta_id = rep(1:3, each = 2), junta = rep(c("1F", "2F", "1M"), each = 2),
  junta_num = rep(c(1L, 2L, 1L), each = 2), sexo = rep(c("F", "F", "M"), each = 2),
  recinto_codigo = 10L, sufragantes = rep(c(10L, 12L, 9L), each = 2),
  blancos = 1L, nulos = 1L, candidato = rep(c("X", "Y"), 3), votos = c(5L, 3L, 6L, 4L, 4L, 3L)
)
mini_spss <- tibble::tibble(
  canton = 1L, parroquia_codigo = 1L, sexo = c("F", "F", "M", "M"), candidato = c("X", "Y", "X", "Y"),
  sufragantes = c(22L, 22L, 9L, 9L), blancos = c(2L, 2L, 1L, 1L), nulos = c(2L, 2L, 1L, 1L),
  votos = c(11L, 7L, 4L, 3L)
)

test_that("compare_with_spss finds no difference when totals agree", {
  cmp <- compare_with_spss(mini_juntas, mini_spss)
  expect_equal(cmp$n_celdas, 4)
  expect_equal(cmp$n_celdas_distintas, 0)
  expect_equal(cmp$n_cabecera_distintas, 0)
  expect_silent(assert_matches_spss(cmp, "mini"))
})

test_that("compare_with_spss catches a changed vote and a missing candidate", {
  bad <- mini_juntas
  bad$votos[1] <- 6L
  expect_error(assert_matches_spss(compare_with_spss(bad, mini_spss), "mini"), "differ")
  bad2 <- dplyr::filter(mini_juntas, !(sexo == "M" & candidato == "Y"))
  expect_equal(compare_with_spss(bad2, mini_spss)$n_celdas_distintas, 1)
})

test_that("assert_juntas_ok rejects duplicates and bad sex codes", {
  expect_equal(assert_juntas_ok(mini_juntas, "mini")$juntas, 3)
  expect_error(assert_juntas_ok(dplyr::bind_rows(mini_juntas, mini_juntas[1, ]), "mini"), "structural")
  bad <- mini_juntas
  bad$sexo[1:2] <- "X"
  expect_error(assert_juntas_ok(bad, "mini"), "structural")
})

test_that("check_recinto_link links juntas and spots disagreements", {
  r <- expand_junta_ranges(mini_dist)
  lk <- check_recinto_link(mini_juntas, r)
  expect_equal(lk$juntas_con_recinto, 3) # 1F, 2F and 1M all fall in recinto 10 (A)
  expect_equal(lk$distinto_del_archivo, 0)
  expect_equal(nrow(lk$sin_resultado), 3) # 2M (recinto A) and 3F, 3M (recinto B) have no result
  expect_error(assert_recinto_link(lk, "mini"), "without result")

  bad <- mini_juntas
  bad$recinto_codigo[bad$junta == "1M"] <- 11L # the file says 11, the range says 10
  expect_equal(check_recinto_link(bad, r)$distinto_del_archivo, 1)
})

# ---- part 2: the real 2023 data (skipped if the raw files are not present) ---------

have_2023 <- file.exists(raw_path("cne2023_prefecto_pichincha_juntas.csv")) &&
  file.exists(raw_path("seccionales-2023", "Resultados-electorales-2023.sav"))

test_that("2023 Prefecto: size, totals and exact match with the SPSS file", {
  skip_if_not(have_2023, "2023 raw files not found")
  j <- read_juntas_web(raw_path("cne2023_prefecto_pichincha_juntas.csv"))
  ck <- assert_juntas_ok(j, "Prefecto 2023")
  expect_equal(ck$filas, 82836)
  expect_equal(ck$juntas, 6903)
  expect_equal(ck$parroquias, 96)
  expect_equal(ck$votos_validos, 1489812)
  expect_equal(c(ck$sufragantes, ck$blancos, ck$nulos), c(1945156, 105012, 349775))
  sp <- spss_dignidad(read_resultados_spss(raw_path("seccionales-2023", "Resultados-electorales-2023.sav")), 11L)
  cmp <- compare_with_spss(j, sp)
  expect_equal(cmp$n_celdas, 2304)
  expect_equal(cmp$n_celdas_distintas, 0)
  expect_equal(cmp$n_cabecera_distintas, 0)
})

test_that("2023 Alcalde Quito: size, totals and exact match with the SPSS file", {
  skip_if_not(have_2023, "2023 raw files not found")
  j <- read_juntas_web(raw_path("cne2023_alcalde_quito_juntas.csv"))
  ck <- assert_juntas_ok(j, "Alcalde Quito 2023")
  expect_equal(ck$filas, 71400)
  expect_equal(ck$juntas, 5950)
  expect_equal(ck$parroquias, 65)
  expect_equal(ck$votos_validos, 1381728)
  expect_equal(c(ck$sufragantes, ck$blancos, ck$nulos), c(1665124, 53579, 229236))
  sp <- spss_dignidad(read_resultados_spss(raw_path("seccionales-2023", "Resultados-electorales-2023.sav")), 13L, canton = 60L)
  cmp <- compare_with_spss(j, sp)
  expect_equal(cmp$n_celdas, 1560)
  expect_equal(cmp$n_celdas_distintas, 0)
})

test_that("2023 and 2027 distributivos and the recinto link", {
  skip_if_not(have_2023, "2023 raw files not found")
  d23 <- read_distributivo(raw_path("seccionales-2023", "Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx"),
                           sheet = 3, skip = 7, anio = 2023L)
  expect_equal(nrow(d23), 450)
  expect_equal(sum(d23$juntas), 6892)
  expect_equal(sum(d23$canton == 60L), 379)
  r23 <- expand_junta_ranges(d23)
  expect_equal(nrow(r23), 6892)

  lk <- check_recinto_link(read_juntas_web(raw_path("cne2023_prefecto_pichincha_juntas.csv")), r23)
  expect_equal(lk$juntas_con_recinto, 6892)
  expect_equal(lk$recintos, 450)
  expect_equal(nrow(lk$sin_recinto), 11)
  expect_true(all(lk$sin_recinto$zona_codigo %in% c(61L, 89L)))
  expect_equal(nrow(lk$sin_resultado), 0)
  expect_equal(lk$distinto_del_archivo, 0)

  f27 <- raw_path("seccionales-2026", "Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx")
  skip_if_not(file.exists(f27), "2027 distributivo not found")
  d27 <- read_distributivo(f27, sheet = "Consolidado", skip = 7, anio = 2027L)
  expect_equal(nrow(d27), 469)
  expect_equal(sum(d27$juntas), 7121)
  expect_equal(sum(d27$juntas[d27$canton == 60L]), 6096)
  expect_equal(sum(d27$juntas > 32), 34)
  expect_equal(nrow(expand_junta_ranges(d27)), 7121)
  expect_equal(sum(d27$electores), 2408731)
})

test_that("parroquias: Quito has 32 urban and 33 rural", {
  f <- raw_path("seccionales-2023", "parroquias.sav")
  skip_if_not(file.exists(f), "parroquias.sav not found")
  p <- read_parroquias(f)
  expect_equal(nrow(p), 96)
  q <- dplyr::count(dplyr::filter(p, canton == 60L), estado)
  expect_equal(q$n[q$estado == "U"], 32)
  expect_equal(q$n[q$estado == "R"], 33)
})
