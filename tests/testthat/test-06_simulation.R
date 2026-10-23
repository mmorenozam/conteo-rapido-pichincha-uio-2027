# Tests for R/06_simulation.R.

fake_jm <- function(seed = 1, quito_only = FALSE) {
  set.seed(seed)
  rec <- tibble::tibble(recinto_codigo = 1:30, M = c(40L, sample(4:14, 29, replace = TRUE)),
                        estrato = factor(rep(c("Quito urbano", "Mejía"), c(20, 10)), levels = ESTRATOS),
                        canton = rep(c(60L, 70L), c(20, 10)))
  if (quito_only) rec <- rec[rec$canton == 60L, ]
  j <- rec[rep(seq_len(nrow(rec)), rec$M), ] |>
    dplyr::mutate(junta_id = dplyr::row_number(), parroquia_codigo = recinto_codigo %/% 5L, zona_codigo = 0L,
                  sexo = rep(c("F", "M"), length.out = dplyr::n()))
  x <- rep(200, nrow(j))
  share <- stats::runif(nrow(rec), 0.2, 0.4)[match(j$recinto_codigo, rec$recinto_codigo)]
  a <- round(x * share)
  b <- round(x * 0.3)
  Y <- cbind(A = a, B = b, C = x - a - b, a - b)
  colnames(Y)[4] <- MARGEN # the pseudo-candidate name used by the pipeline ("MARGEN 1-2")
  list(juntas = j, Y = Y, x = x)
}

test_that("pick_juntas balances sex and gives correct inclusion probabilities", {
  set.seed(1)
  p <- pick_juntas(1:6, 7:10, 2L)
  expect_equal(length(p$filas), 2)
  expect_true(any(p$filas <= 6) && any(p$filas > 6))
  expect_equal(sort(p$p), sort(c(1 / 6, 1 / 4)))
  all <- pick_juntas(1:2, 3L, 5L)
  expect_equal(sort(all$filas), 1:3)
  expect_equal(all$p, rep(1, 3))
})

test_that("full enumeration reproduces the truth with zero corrected variance", {
  jm <- fake_jm()
  pop <- sim_population(jm, fake_jm(quito_only = TRUE))
  n_all <- c("Quito urbano" = 20L, "Mejía" = 10L)
  s <- draw_sample(pop, n_all, m = 100L)
  expect_equal(nrow(s), nrow(jm$juntas))
  e <- estimate_sample(s, pop$prov, s$fila, stats::qnorm(0.995))
  expect_equal(unname(e$R), unname(pop$prov$verdad))
  expect_equal(unname(e$semi_uc_fpc), rep(0, 4))
})

test_that("weights are unbiased for the number of juntas and the estimator is nearly unbiased", {
  pop <- sim_population(fake_jm(), fake_jm(quito_only = TRUE))
  n_h <- c("Quito urbano" = 8L, "Mejía" = 4L)
  set.seed(5)
  tot <- replicate(300, sum(draw_sample(pop, n_h, 2L)$w))
  expect_equal(mean(tot), nrow(fake_jm()$juntas), tolerance = 0.02)
  sim <- simulate_design(pop, n_h, 2L, n_sim = 300, seed = 7)
  r <- summarise_simulation(sim)
  expect_true(all(abs(r$sesgo) < 0.5)) # pp
  expect_true(all(r$cobertura_uc >= 0.9))
})

test_that("simulation is reproducible with a seed", {
  pop <- sim_population(fake_jm(), fake_jm(quito_only = TRUE))
  n_h <- c("Quito urbano" = 6L, "Mejía" = 4L)
  expect_identical(simulate_design(pop, n_h, 2L, 20, seed = 3), simulate_design(pop, n_h, 2L, 20, seed = 3))
})

test_that("coverage is computed on the replicate half-widths, not on their mean", {
  sim <- tibble::tibble(escenario = "x", dignidad = "d", cantidad = "c", replica = 1:4, recintos = 10, actas = 20,
                        error = c(0.02, -0.02, 0.001, 0), semi_uc = 0.01, semi_uc_fpc = 0.01)
  r <- summarise_simulation(sim)
  expect_equal(r$cobertura_uc, 0.5)
  expect_equal(r$semi_uc, 1) # reported in pp
})

test_that("certainty recintos observed with one junta still give a variance", {
  pop <- sim_population(fake_jm(), fake_jm(quito_only = TRUE))
  n_h <- c("Quito urbano" = 15L, "Mejía" = 4L) # most Quito recintos become certainty
  set.seed(9)
  s <- draw_sample(pop, n_h, 1L)
  expect_true(any(grepl("\\|C1$", s$vstrat)))
  e <- estimate_sample(s, pop$prov, s$fila, stats::qnorm(0.995))
  expect_true(all(e$semi_uc[c(1, 3, 4)] > 0)) # column B has no variance by construction
})
