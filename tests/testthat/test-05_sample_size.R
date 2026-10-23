# Tests for R/05_sample_size.R.

mk_strata <- function() list(
  A = list(estrato = "A", J = c(40L, rep(10L, 39)), xbar = 200, S = c(x = 0.08, MARGEN = 0.10), rho = c(x = 0.6, MARGEN = 0.3)),
  B = list(estrato = "B", J = rep(8L, 20), xbar = 180, S = c(x = 0.05, MARGEN = 0.12), rho = c(x = 0.5, MARGEN = 0.6))
)

test_that("certainty_vector follows the iterative rule and enumerates when exhausted", {
  expect_equal(which(certainty_vector(c(40L, rep(10L, 39)), 10L)), integer(0)) # 430 / 10 = 43 > 40: none
  expect_equal(which(certainty_vector(c(40L, rep(10L, 39)), 12L)), 1L) # 430 / 12 = 35.8: the 40 is certainty; then 390 / 11 = 35.5
  expect_true(all(certainty_vector(c(50L, 50L, 1L), 3L))) # every recinto ends certainty: the stratum is enumerated
})

test_that("variance falls with more recintos and with more juntas per recinto", {
  st <- mk_strata()
  z <- stats::qnorm(0.995)
  v <- function(n, m) max(evaluate_design(st, c(A = n, B = 6L), m, z)$semiamplitud)
  expect_lt(v(20L, 1L), v(10L, 1L))
  expect_lt(v(10L, 3L), v(10L, 1L))
  # with rho = 1 a second junta adds nothing (only the PPS part counts)
  st1 <- lapply(st, function(s) { s$rho[] <- 1; s })
  e1 <- evaluate_design(st1, c(A = 10L, B = 6L), 1L, z)$semiamplitud
  e2 <- evaluate_design(st1, c(A = 10L, B = 6L), 2L, z)$semiamplitud
  expect_equal(e1, e2)
})

test_that("allocation respects total, minimum and stratum size", {
  st <- mk_strata()
  a <- allocate_recintos(st, 30L, 2L, "neyman", minimo = 4L)
  expect_equal(sum(a), 30L)
  expect_true(all(a >= 4L))
  expect_true(all(a <= c(40L, 20L)))
  p <- allocate_recintos(st, 30L, 2L, "proporcional", minimo = 4L)
  expect_equal(unname(p), c(22L, 8L)) # juntas 430 vs 160: 21.9 and 8.1
})

test_that("size_design returns the smallest feasible size", {
  st <- mk_strata()
  r <- size_design(st, E = 2, conf = 0.95, m = 2L, asignacion = "neyman", minimo = 2L)
  expect_lte(max(r$ev$semiamplitud), 0.02)
  expect_equal(sum(r$n_h), 35L)
  z <- stats::qnorm(0.975)
  smaller <- allocate_recintos(st, sum(r$n_h) - 1L, 2L, "neyman", minimo = 2L)
  expect_gt(max(evaluate_design(st, smaller, 2L, z)$semiamplitud), 0.02)
})

test_that("2023 / 2027: sizes match the independent computation", {
  f <- raw_path("cne2023_prefecto_pichincha_juntas.csv")
  f27 <- raw_path("seccionales-2026", "Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx")
  skip_if_not(file.exists(f) && file.exists(f27), "raw files not found")
  p23 <- read_parroquias(raw_path("seccionales-2023", "parroquias.sav"))
  d23 <- read_distributivo(raw_path("seccionales-2023", "Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx"), 3, 7, 2023L)
  marco <- build_frame(read_distributivo(f27, "Consolidado", 7, 2027L), d23, p23)
  par <- dplyr::bind_rows(
    estimate_parameters(junta_matrix(clean_juntas(read_juntas_web(f), "P"), p23), "Prefecto 2023", B = 0L),
    estimate_parameters(junta_matrix(clean_juntas(read_juntas_web(raw_path("cne2023_alcalde_quito_juntas.csv")), "A"), p23),
                        "Alcalde Quito 2023", B = 0L)
  )
  sz <- function(dig, obj, E, conf, m) {
    r <- size_design(design_strata(par, marco, dig, obj, "punto"), E, conf, m, "neyman", 4L)
    sum(r$n_h)
  }
  expect_equal(sz("Prefecto 2023", "cuotas", 1, 0.99, 3L), 235L)
  expect_equal(sz("Prefecto 2023", "margen", 1, 0.99, 3L), 191L)
  expect_equal(sz("Prefecto 2023", "ambos", 1, 0.99, 3L), 245L)
  expect_equal(sz("Prefecto 2023", "ambos", 1, 0.99, 2L), 250L)
  expect_equal(sz("Alcalde Quito 2023", "ambos", 1, 0.99, 3L), 179L)

  it <- integrated_size(par, marco, "Prefecto 2023", "Alcalde Quito 2023", 1, 0.99, 3L, "neyman", "ambos", "punto")
  expect_equal(it$recintos, 245L)
  expect_equal(it$actas, 735L)
  expect_equal(it$recintos_certeza, 57L)
  expect_lte(it$semiamplitud_prefecto, 1)
  expect_lte(it$semiamplitud_alcalde, 1)
})
