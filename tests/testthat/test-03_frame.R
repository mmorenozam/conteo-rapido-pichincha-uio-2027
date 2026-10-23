# Tests for R/03_frame.R.

mini <- tibble::tibble(
  anio = 2027L, canton = c(60L, 60L, 60L, 80L, 70L), canton_nombre = "X",
  parroquia_codigo = c(1L, 1L, 2L, 3L, 4L), parroquia = c("U1", "U1", "R1", "S", "M"),
  zona_codigo = 0L, zona = "Z", recinto_codigo = c(10L, 11L, 12L, 13L, 14L), recinto = "R",
  juntas_f = c(30L, 2L, 3L, 4L, 1L), juntas_m = c(30L, 2L, 3L, 4L, 1L), juntas = c(60L, 4L, 6L, 8L, 2L),
  electores = 100L, cda = "SI", dificil_acceso = "NO", sin_conectividad = "NO"
)
mini_parr <- tibble::tibble(canton = c(60L, 60L, 80L, 70L), parroquia_codigo = 1:4,
                            parroquia = c("U1", "R1", "S", "M"), estado = c("U", "R", "U", "R"))
mini_23 <- dplyr::mutate(mini, juntas = c(60L, 3L, 6L, 8L, 2L))[-5, ]

test_that("assign_strata uses the parroquia flag in Quito and cantons elsewhere", {
  s <- assign_strata(mini, mini_parr)
  expect_equal(as.character(s$estrato), c("Quito urbano", "Quito urbano", "Quito rural", ESTRATOS[3], ESTRATOS[4]))
  expect_error(assign_strata(mini, mini_parr[-2, ]), "without stratum")
})

test_that("link_history classifies recintos", {
  f <- build_frame(mini, mini_23, mini_parr)
  h <- f$historia[match(10:14, f$recinto_codigo)]
  expect_equal(h, c("igual", "cambia_juntas", "igual", "igual", "nuevo"))
  expect_silent(assert_frame_ok(f, mini))
})

test_that("crosswalk maps Juan Montalvo to its 2027 code", {
  p <- parroquias_frame(tibble::tibble(canton = 65L, parroquia_codigo = 5495L, parroquia = "JUAN MONTALVO", estado = "U"))
  expect_true(4765L %in% p$parroquia_codigo)
  expect_equal(nrow(p), 2)
})

test_that("allocation adds up and respects the minimum", {
  f <- build_frame(mini, mini_23, mini_parr)
  a <- allocate_proportional(f, 10L)
  expect_equal(sum(a$n_recintos), 10L)
  expect_true(all(a$n_recintos >= 2L))
})

test_that("a recinto larger than the interval becomes certainty", {
  mini2 <- dplyr::bind_rows(mini, dplyr::mutate(mini[1, ], recinto_codigo = 15L, juntas_f = 3L, juntas_m = 3L, juntas = 6L))
  qu <- dplyr::filter(build_frame(mini2, mini_23, mini_parr), estrato == "Quito urbano") # 60, 4 and 6 juntas
  a <- tibble::tibble(estrato = factor("Quito urbano", levels = ESTRATOS), n_recintos = 2L)
  f1 <- flag_certainty(qu, a)
  # 70 juntas, 2 to draw: interval 35, so recinto 10 (60) is certainty; then 10 / 1 = 10, nobody else
  expect_equal(f1$certeza[match(c(10L, 11L, 15L), f1$recinto_codigo)], c(TRUE, FALSE, FALSE))
  expect_equal(f1$intervalo[1], 10)
  # 3 to draw from 3 recintos: every recinto ends up certainty, which is an error
  expect_error(flag_certainty(qu, dplyr::mutate(a, n_recintos = 3L)), "certainty")
})

test_that("2027 frame: strata, history and provisional certainty match the checked numbers", {
  f27 <- raw_path("seccionales-2026", "Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx")
  f23 <- raw_path("seccionales-2023", "Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx")
  skip_if_not(file.exists(f27) && file.exists(f23), "distributivos not found")
  d27 <- read_distributivo(f27, sheet = "Consolidado", skip = 7, anio = 2027L)
  d23 <- read_distributivo(f23, sheet = 3, skip = 7, anio = 2023L)
  p <- read_parroquias(raw_path("seccionales-2023", "parroquias.sav"))
  m <- assert_frame_ok(build_frame(d27, d23, p), d27)

  s <- frame_summary(m)
  expect_equal(s$recintos[match(ESTRATOS, s$estrato)], c(266L, 120L, 22L, 16L, 26L, 19L))
  expect_equal(s$juntas[match(ESTRATOS, s$estrato)], c(4273L, 1823L, 298L, 231L, 355L, 141L))
  expect_equal(s$electores[match(ESTRATOS, s$estrato)], c(1451670L, 616121L, 100671L, 77712L, 118474L, 44083L))
  expect_equal(as.integer(table(m$historia)[c("igual", "cambia_juntas", "nuevo")]), c(229L, 206L, 34L))
  expect_equal(sum(m$historia == "cambia_parroquia"), 0L) # Juan Montalvo resolved by the crosswalk
  r <- recintos_retirados(m, d23)
  expect_equal(nrow(r), 15)
  expect_equal(sum(r$juntas), 125)

  a <- allocate_proportional(m, 188L)
  expect_equal(a$n_recintos, c(113L, 48L, 8L, 6L, 9L, 4L))
  cs <- certainty_summary(flag_certainty(m, a), a)
  expect_equal(cs$recintos_certeza, c(15L, 10L, 2L, 0L, 0L, 0L))
})
