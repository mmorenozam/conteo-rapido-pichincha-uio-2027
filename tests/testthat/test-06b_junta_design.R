# Tests for R/06b_junta_design.R.

fake_jm_j <- function(quito_only = FALSE) {
  set.seed(1)
  rec <- tibble::tibble(recinto_codigo = 1:30, M = c(40L, sample(4:14, 29, replace = TRUE)),
                        estrato = factor(rep(c("Quito urbano", "Mejía"), c(20, 10)), levels = ESTRATOS),
                        canton = rep(c(60L, 70L), c(20, 10)))
  if (quito_only) rec <- rec[rec$canton == 60L, ]
  j <- rec[rep(seq_len(nrow(rec)), rec$M), ] |>
    dplyr::mutate(junta_id = dplyr::row_number(), parroquia_codigo = recinto_codigo %/% 5L, zona_codigo = 0L,
                  sexo = rep(c("F", "M"), length.out = dplyr::n()), junta_num = dplyr::row_number())
  x <- rep(200, nrow(j))
  share <- stats::runif(nrow(rec), 0.2, 0.4)[match(j$recinto_codigo, rec$recinto_codigo)]
  a <- round(x * share)
  b <- round(x * 0.3)
  Y <- cbind(A = a, B = b, C = x - a - b, a - b)
  colnames(Y)[4] <- MARGEN
  list(juntas = j, Y = Y, x = x)
}

test_that("systematic draw gives n distinct juntas per stratum with weights N / n", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  set.seed(2)
  s <- draw_juntas(pop, c("Quito urbano" = 25L, "Mejía" = 7L))
  expect_equal(as.vector(table(s$estrato)[c("Quito urbano", "Mejía")]), c(25L, 7L))
  expect_equal(anyDuplicated(s$fila), 0L)
  N <- lengths(pop$filas_estrato)
  expect_equal(sum(s$w), sum(N))
})

test_that("taking every junta gives the truth and zero variance", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  N <- lengths(pop$filas_estrato)
  s <- draw_juntas(pop, N)
  e <- estimate_juntas(s, pop$prov, s$fila, stats::qnorm(0.995))
  expect_equal(unname(e$R), unname(pop$prov$verdad))
  expect_equal(unname(e$semi_srs), rep(0, 4))
  expect_equal(unname(e$semi_sd), rep(0, 4))
})

test_that("successive differences credit the sort order (smaller than SRS when recintos differ)", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  set.seed(3)
  r <- summarise_junta_sim(simulate_junta_design(pop, c("Quito urbano" = 30L, "Mejía" = 10L), 200, seed = 4))
  a <- dplyr::filter(r, cantidad == "A")
  expect_true(all(a$semi_sd < a$semi_srs))
  expect_true(all(abs(r$sesgo) < 0.5))
  expect_true(all(a$cobertura_sd >= 0.9))
})

test_that("one sample and two samples of the same size give the same precision", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  n_h <- c("Quito urbano" = 30L, "Mejía" = 10L)
  one <- summarise_junta_sim(simulate_junta_design(pop, n_h, 300, seed = 5))
  two <- summarise_junta_sim(simulate_junta_design(pop, n_h, 300, seed = 6, n_h_alcalde = n_h["Quito urbano"]))
  expect_equal(unique(one$actas), unique(two$actas))
  expect_gt(unique(two$juntas), unique(one$juntas)) # two samples: more distinct juntas to visit
  a1 <- one$rmse[one$dignidad == "Alcalde Quito 2023" & one$cantidad == "A"]
  a2 <- two$rmse[two$dignidad == "Alcalde Quito 2023" & two$cantidad == "A"]
  expect_lt(abs(a1 / a2 - 1), 0.25)
})

test_that("formula sizes respect the target and the allocation sums up", {
  st <- list(
    A = list(estrato = "A", J = rep(10L, 100), xbar = 200, S = c(x = 0.08, MARGEN = 0.10), rho = c(x = 0.6, MARGEN = 0.3)),
    B = list(estrato = "B", J = rep(8L, 20), xbar = 180, S = c(x = 0.05, MARGEN = 0.12), rho = c(x = 0.5, MARGEN = 0.6))
  )
  n_h <- junta_formula_size(st, E = 1, conf = 0.99)
  z <- stats::qnorm(0.995)
  expect_lte(max(evaluate_junta_design(st, n_h, z)), 0.01)
  expect_true(all(n_h <= c(1000L, 160L)))
  # independent check: stratified SRS formula by hand for the margin
  W <- c(A = 1000 * 200, B = 160 * 180); W <- W / sum(W)
  V <- sum(W^2 * c(0.10, 0.12)^2 / n_h * (1 - n_h / c(1000, 160)))
  expect_equal(unname(evaluate_junta_design(st, n_h, z)["MARGEN"]), z * sqrt(V))
})

test_that("the summary takes the quantile at each scenario's own confidence level", {
  sim <- tibble::tibble(escenario = rep(c("a", "b"), each = 100), dignidad = "d", cantidad = "c",
                        conf = rep(c(0.5, 0.99), each = 100), replica = 1:200, juntas = 10, actas = 10,
                        recintos = 5, max_por_recinto = 2L, error = rep(seq(0.001, 0.1, length.out = 100), 2),
                        semi_srs = 0.05, semi_sd = 0.05)
  r <- summarise_junta_sim(sim)
  expect_equal(r$error_cuantil[r$escenario == "a"], 100 * stats::quantile(seq(0.001, 0.1, length.out = 100), 0.5, names = FALSE))
  expect_gt(r$error_cuantil[r$escenario == "b"], r$error_cuantil[r$escenario == "a"])
})

test_that("without losses nothing changes and no random numbers are used", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  set.seed(7)
  s <- draw_juntas(pop, c("Quito urbano" = 25L, "Mejía" = 7L))
  set.seed(8); a <- stats::runif(1)
  set.seed(8); r <- apply_losses(s, pop, 0, fallen_recintos(pop$recinto[s$fila], 0)); b <- stats::runif(1)
  expect_identical(r, s)
  expect_identical(a, b)
})

test_that("losses drop whole recintos and reweight within stratum", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  set.seed(9)
  s <- draw_juntas(pop, c("Quito urbano" = 60L, "Mejía" = 20L))
  caido <- pop$recinto[s$fila[1]]
  r <- apply_losses(s, pop, c("Quito urbano" = 0.2, "Mejía" = 0.1), caido)
  expect_false(any(pop$recinto[r$fila] == caido))
  expect_lt(length(r$fila), length(s$fila))
  N <- lengths(pop$filas_estrato)
  for (h in unique(r$estrato)) expect_equal(sum(r$w[r$estrato == h]), unname(N[[h]]))
})

test_that("buffer inflates by the expected losses and is capped by the population", {
  n <- c("Quito urbano" = 90L, "Mejía" = 20L)
  expect_equal(unname(buffer_allocation(n, c("Quito urbano" = 0.1, "Mejía" = 0.2))), c(100L, 25L))
  expect_equal(unname(buffer_allocation(n, 0.5, N = c("Quito urbano" = 150L, "Mejía" = 30L))), c(150L, 30L))
  expect_equal(sum(scale_allocation(c(a = 50L, b = 50L), 60L)), 60L)
})

test_that("with losses fewer juntas report, bias stays small, and the buffer restores the count", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  n_h <- c("Quito urbano" = 40L, "Mejía" = 12L)
  pj <- c("Quito urbano" = 0.1, "Mejía" = 0.2)
  sin <- simulate_junta_design(pop, n_h, 300, seed = 10, perdida = pj, perdida_recinto = 0.05)
  expect_true(all(sin$juntas <= sin$juntas_plan))
  expect_lt(stats::median(sin$juntas), sum(n_h))
  r <- summarise_junta_sim(sin)
  expect_true(all(abs(r$sesgo) < 0.5))
  con <- simulate_junta_design(pop, buffer_allocation(n_h, pj, 0.05), 300, seed = 11, perdida = pj, perdida_recinto = 0.05)
  expect_gt(stats::median(con$juntas), stats::median(sin$juntas))
  o <- field_options(dplyr::bind_rows(dplyr::mutate(sin, escenario = "sin"), dplyr::mutate(con, escenario = "con")))
  expect_equal(nrow(o), 2L)
  expect_true(all(c("juntas_muestra", "juntas_reportan", "cumple") %in% names(o)))
})

test_that("field scenarios give three options at any confidence level", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  sim <- field_scenarios(pop, c("Quito urbano" = 30L, "Mejía" = 10L), c("Quito urbano" = 0.1, "Mejía" = 0.1), 0.03,
                         n_sim = 50, seed = 12, conf = 0.95)
  o <- field_options(sim, E = 1, conf = 0.95)
  expect_equal(nrow(o), 3L)
  expect_identical(substr(o$escenario, 1, 2), c("A1", "B1", "B2"))
  expect_true(all(o$juntas_reportan <= o$juntas_muestra))
})

test_that("the cap of juntas per recinto holds, keeps the weights, and changes nothing when absent", {
  pop <- junta_population(fake_jm_j(), fake_jm_j(quito_only = TRUE))
  n_h <- c("Quito urbano" = 25L, "Mejía" = 7L)
  set.seed(21); a <- draw_juntas(pop, n_h)
  set.seed(21); b <- draw_juntas(pop, n_h, max_recinto = Inf)
  expect_identical(a, b)
  # recinto 1 has 40 of about 200 Quito juntas: about 5 of 25 juntas without a cap
  set.seed(22); s <- draw_juntas(pop, n_h, max_recinto = 4)
  expect_lte(max(table(pop$recinto[s$fila])), 4L)
  expect_equal(length(s$fila), sum(n_h))
  expect_equal(anyDuplicated(s$fila), 0L)
  N <- lengths(pop$filas_estrato)
  expect_equal(sum(s$w[startsWith(s$estrato, "Quito urbano")]), unname(N[["Quito urbano"]]))
  expect_true(any(grepl("\\|R", s$estrato)))
  r <- summarise_junta_sim(simulate_junta_design(pop, c("Quito urbano" = 40L, "Mejía" = 10L), 200, seed = 23, max_recinto = 4))
  expect_true(all(abs(r$sesgo) < 0.5))
  expect_lte(max(r$max_por_recinto), 4L)
})
