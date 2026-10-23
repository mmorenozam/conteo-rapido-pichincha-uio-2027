# Tests for R/04_parameters.R.

test_that("param_core agrees with a direct computation", {
  set.seed(1)
  g <- rep(1:6, times = c(2, 3, 2, 4, 1, 3))
  x <- sample(150:250, length(g), replace = TRUE)
  y <- round(x * (0.3 + 0.1 * (g %% 3) + stats::rnorm(length(g), 0, 0.02)))
  p <- param_core(cbind(a = y, b = x - y), x, g)

  R <- sum(y) / sum(x)
  e <- y - R * x
  expect_equal(unname(p$R[1]), R)
  expect_equal(unname(p$S[1]), stats::sd(e) / mean(x))
  # one-way ANOVA estimator with unequal group sizes
  N <- length(e); k <- 6; m <- tabulate(g)
  gm <- tapply(e, g, mean)
  msb <- sum(m * (gm - mean(e))^2) / (k - 1)
  msw <- sum((e - gm[g])^2) / (N - k)
  m0 <- (N - sum(m^2) / N) / (k - 1)
  expect_equal(unname(p$rho[1]), (msb - msw) / (msb + (m0 - 1) * msw))
  # with two candidates, residuals are mirror images: same S and rho
  expect_equal(unname(p$S[1]), unname(p$S[2]))
  expect_equal(unname(p$rho[1]), unname(p$rho[2]))
})

test_that("rho is near 1 when recintos are homogeneous and near 0 when they are not", {
  set.seed(2)
  g <- rep(1:40, each = 2)
  x <- rep(200, 80)
  share <- stats::runif(40, 0.1, 0.5)
  y_hom <- round(x * share[g])
  y_ind <- round(x * stats::runif(80, 0.1, 0.5))
  expect_gt(param_core(cbind(y_hom), x, g)$rho, 0.95)
  expect_lt(abs(param_core(cbind(y_ind), x, g)$rho), 0.3)
  expect_equal(deff_cluster(0.5, 2), 1.5)
})

test_that("bootstrap is reproducible with a seed and brackets the estimate", {
  set.seed(3)
  g <- rep(1:30, each = 2)
  x <- rep(200, 60)
  y <- round(x * stats::runif(30, 0.2, 0.4)[g] + stats::rnorm(60, 0, 5))
  b1 <- boot_params(cbind(a = y), x, g, B = 200, seed = 11)
  b2 <- boot_params(cbind(a = y), x, g, B = 200, seed = 11)
  expect_identical(b1, b2)
  p <- param_core(cbind(a = y), x, g)
  expect_true(b1$S_inf < p$S && p$S < b1$S_sup)
  expect_true(b1$rho_inf < p$rho && p$rho < b1$rho_sup)
})

test_that("2023: S and rho reproduce the values checked independently", {
  f <- raw_path("cne2023_prefecto_pichincha_juntas.csv")
  skip_if_not(file.exists(f), "2023 raw files not found")
  p23 <- read_parroquias(raw_path("seccionales-2023", "parroquias.sav"))
  jm <- junta_matrix(clean_juntas(read_juntas_web(f), "Prefecto 2023"), p23)
  expect_equal(colnames(jm$Y)[1:3], c("PAOLA PABON", "GUILLERMO CHURUCHUMBI", "EDUARDO DEL POZO"))
  pr <- estimate_parameters(jm, "Prefecto 2023", B = 0L)
  v <- function(est, cand, col) pr[[col]][pr$estrato == est & pr$candidato == cand]
  expect_equal(v("Quito urbano", "PAOLA PABON", "pct"), 27.701, tolerance = 1e-4)
  expect_equal(v("Quito urbano", "PAOLA PABON", "S"), 0.0709, tolerance = 2e-3)
  expect_equal(v("Quito urbano", "PAOLA PABON", "rho"), 0.6703, tolerance = 1e-3)
  expect_equal(v("Quito rural", "GUILLERMO CHURUCHUMBI", "S"), 0.1080, tolerance = 2e-3)
  expect_equal(v("Quito urbano", MARGEN, "rho"), 0.2210, tolerance = 2e-3)
  expect_equal(v("Total", "PAOLA PABON", "pct"), 27.957, tolerance = 1e-4)
  expect_equal(v("Cayambe y Pedro Moncayo", "GUILLERMO CHURUCHUMBI", "pct"), 47.923, tolerance = 1e-4)
  expect_equal(sort(unique(pr$estrato)), sort(c(ESTRATOS, "Total")))

  ja <- junta_matrix(clean_juntas(read_juntas_web(raw_path("cne2023_alcalde_quito_juntas.csv")), "Alcalde"), p23)
  pa <- estimate_parameters(ja, "Alcalde Quito 2023", B = 0L)
  expect_equal(sort(unique(pa$estrato)), c("Quito rural", "Quito urbano", "Total"))
  expect_equal(pa$S[pa$estrato == "Quito urbano" & pa$candidato == "JORGE YUNDA"], 0.0838, tolerance = 2e-3)
  expect_equal(pa$rho[pa$estrato == "Quito urbano" & pa$candidato == MARGEN], 0.3643, tolerance = 2e-3)
})
