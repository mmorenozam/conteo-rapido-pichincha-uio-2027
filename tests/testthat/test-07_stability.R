# Tests for R/07_stability.R.

# Same fake junta matrix as in test-06b (test files do not share helpers).
fake_jm_7 <- function(quito_only = FALSE) {
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


fake_params <- function(dignidad, S_scale = 1, estratos = ESTRATOS) {
  purrr::map_dfr(estratos, function(h) {
    tibble::tibble(dignidad = dignidad, estrato = h, candidato = c("A", "B", "C", MARGEN), orden = 1:4,
                   pct = c(40, 30, 20, 10), S = S_scale * c(0.08, 0.07, 0.05, 0.12), rho = 0.5,
                   juntas = 100L, recintos = 20L, votos_validos_por_junta = 250)
  })
}

test_that("historical files are read with surrogate recinto ids turned into integers", {
  f <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(dignidad = 2, provincia = 17, canton = 60, parroquia_codigo = 30, parroquia = "X",
                                  zona_codigo = 1, zona = "X", recinto_codigo = c("R19_2640", NA), recinto = "R",
                                  junta_id = 1:2, junta = "1F", junta_num = 1, sexo = "F", sufragantes = 10,
                                  ausentismo = 1, blancos = 1, nulos = 1, op_orden = 1, op_nombre = "P",
                                  candidato = " A  B ", votos = 8), f)
  x <- read_juntas_hist(f)
  expect_identical(x$recinto_codigo, c(2640L, NA))
  expect_type(x$votos, "integer")
  expect_identical(x$candidato[1], "A B")
})

test_that("the stability table gives ratio 1 for the reference races", {
  p <- dplyr::bind_rows(fake_params("Prefecto 2023"), fake_params("Prefecto 2019", 0.8),
                        fake_params("Alcalde Quito 2023", estratos = ESTRATOS[1:2]))
  st <- stability_table(p)
  expect_true(all(abs(st$razon_vs_2023[st$dignidad == "Prefecto 2023"] - 1) < 1e-12))
  expect_true(all(abs(st$razon_vs_2023[st$dignidad == "Prefecto 2019"] - 0.8) < 1e-12))
  expect_true(all(st$candidato_S_max == MARGEN))
})

test_that("the Gibbs sampler recovers stratum levels and the spread between races", {
  set.seed(5)
  a <- log(c(0.12, 0.10, 0.08, 0.09, 0.11, 0.13))
  b <- stats::rnorm(8, 0, 0.25)
  d <- expand.grid(h = 1:6, r = 1:8)
  d$S_max <- exp(a[d$h] + b[d$r] + stats::rnorm(nrow(d), 0, 0.05))
  inputs <- tibble::tibble(dignidad = paste("carrera", d$r), estrato = ESTRATOS[d$h], S_max = d$S_max)
  fit <- fit_s_model(inputs, n_iter = 4000L, burn = 1000L, seed = 1L)
  expect_equal(dim(fit$a), c(3000L, 6L))
  # differences between strata are identified sharply (the race effects cancel)
  dif <- apply(fit$a - fit$a[, 1], 2, stats::median)
  expect_lt(max(abs(dif - (a - a[1]))), 0.06)
  expect_lt(abs(stats::median(sqrt(fit$sigma2)) - 0.05), 0.04)
  expect_gt(stats::median(sqrt(fit$tau2)), 0.1)
  pred <- predict_s(fit, ESTRATOS[1:2], seed = 2L)
  expect_equal(dim(pred), c(3000L, 2L))
  expect_true(all(pred > 0))
})

test_that("assurance grows with the sample and the calibration factor is 1 when formula = simulation", {
  p23 <- dplyr::bind_rows(fake_params("Prefecto 2023"), fake_params("Alcalde Quito 2023", estratos = ESTRATOS[1:2]))
  frame <- tibble::tibble(estrato = factor(rep(ESTRATOS, each = 50), levels = ESTRATOS), juntas = 10L)
  n_h <- stats::setNames(c(150L, 60L, 10L, 10L, 20L, 8L), ESTRATOS)
  z <- stats::qnorm(0.995)
  st <- design_strata(p23, frame, "Prefecto 2023", "ambos")
  hw <- junta_half_width(st, n_h, stats::setNames(rep(0.12, 6), ESTRATOS), z)
  sq <- design_strata(p23, frame, "Alcalde Quito 2023", "ambos")
  hq <- junta_half_width(sq, n_h, stats::setNames(rep(0.12, 2), ESTRATOS[1:2]), z)
  sim <- tibble::tibble(escenario = "A1", dignidad = rep(c("Prefecto 2023", "Alcalde Quito 2023"), each = 2),
                        cantidad = "x", semi_sd = c(hw, hw, hq, hq))
  cal <- calibration_factors(sim, "A1", n_h, p23, frame, conf = 0.99)
  expect_equal(cal$c, c(1, 1))
  inputs <- dplyr::bind_rows(design_inputs(p23), dplyr::mutate(design_inputs(fake_params("Prefecto 2019", 0.9)), juntas = 100L))
  fit <- fit_s_model(inputs, n_iter = 1500L, burn = 500L)
  cv <- assurance_curve(fit, n_h, p23, frame, cal, E = 1, conf = 0.99, totales = c(100L, 300L, 450L))
  expect_true(all(diff(cv$prob_cumple) >= 0))
  expect_true(all(cv$prob_cumple <= pmin(cv$prob_prefecto, cv$prob_alcalde) + 1e-12))
  sz <- assurance_sizes(cv, n_h, 0.1, 0, N = stats::setNames(rep(500L, 6), ESTRATOS), niveles = 0.5)
  expect_true(is.na(sz$juntas) || sz$juntas_con_colchon >= sz$juntas)
})

test_that("weighted parameters average the variances by stratum and rank", {
  p23 <- fake_params("Prefecto 2023")
  p19 <- fake_params("Prefecto 2019", 0.5) |> dplyr::mutate(candidato = c("X", "Y", "Z", MARGEN)[orden], orden = orden)
  pp <- pool_parameters(p23, p19, "Prefecto ponderado", w = 2 / 3)
  expect_equal(nrow(pp), 4L * length(ESTRATOS))
  expect_true(all(pp$dignidad == "Prefecto ponderado"))
  expect_equal(pp$S, sqrt(2 / 3 * pp$S_nueva^2 + 1 / 3 * (0.5 * pp$S_nueva)^2))
  expect_true(all(pp$S < pp$S_nueva & pp$S > 0.5 * pp$S_nueva))
  expect_identical(pp$candidato[pp$orden == 1][1], "A") # names of the newer race
  expect_error(pool_parameters(p23, dplyr::filter(p19, estrato != "Mejía"), "x"), "missing")
})

test_that("the size choice over several elections complies only if every election complies", {
  pop <- junta_population(fake_jm_7(), fake_jm_7(quito_only = TRUE))
  sim <- simulate_scaling_elections(list(a = pop, b = pop), c("Quito urbano" = 30L, "Mejía" = 10L),
                                    factores = c(0.5, 1), n_sim = 40, seed = 3L, conf = 0.95, E = 50) |>
    dplyr::mutate(semi_sd = 0.4) # reported margin 40 pp: covers every error, within E = 50 pp
  ch <- size_choice_elections(sim)
  expect_equal(nrow(ch), 2L)
  expect_true(all(ch$cumple)) # E = 50 pp: always met
  sim2 <- dplyr::mutate(sim, semi_sd = dplyr::if_else(eleccion == "b", 1, semi_sd))
  expect_false(any(size_choice_elections(sim2)$cumple))
  q <- quito_subset(list(juntas = tibble::tibble(canton = c(60L, 70L)), Y = matrix(1:4, 2), x = c(5, 6)))
  expect_equal(q$x, 5)
})

test_that("the reported margin is the larger of sd and SRS, and option B ignores the margin 1-2", {
  sim <- tibble::tibble(escenario = "A1", dignidad = rep(c("Prefecto 2019", "Alcalde Quito 2019"), each = 2),
                        cantidad = rep(c("A", MARGEN), 2), error = c(0.004, 0.012, 0.004, 0.012),
                        semi_sd = c(0.003, 0.009, 0.003, 0.009), semi_srs = c(0.006, 0.015, 0.006, 0.015),
                        juntas = 100, actas = 150)
  r <- with_reported_margin(sim)
  expect_equal(r$semi_rep, sim$semi_srs)
  expect_equal(with_reported_margin(sim, "sd")$semi_rep, sim$semi_sd)
  a <- field_options(sim, E = 1, conf = 0.95)
  b <- field_options(sim, E = 1, conf = 0.95, cantidades = "cuotas")
  expect_equal(a$peor_semi, 1.5)  # the margin 1-2 binds option A
  expect_equal(b$peor_semi, 0.6)  # option B only looks at the shares
  expect_equal(b$semi_margen, 1.5) # but still reports the margin's margin of error
  expect_false(a$cumple)
  expect_true(b$cumple)
  ca <- dplyr::mutate(a, escenario = "A1", conf = 0.99, eleccion = "2023")
  s <- options_summary(ca, dplyr::mutate(b, escenario = "A1", conf = 0.99, eleccion = "2023"))
  expect_equal(nrow(s), 2L)
  expect_equal(s$cumple_en_ambas, c(FALSE, TRUE))
})

test_that("the focused rule uses sd for the two leaders and the margin, the larger margin for the rest", {
  sim <- tibble::tibble(dignidad = rep(c("Prefecto 2023", "Alcalde Quito 2023"), each = 4),
                        cantidad = rep(c("A", "B", "C", MARGEN), 2), error = 0.001,
                        semi_sd = 0.01, semi_srs = 0.02)
  r <- with_reported_margin(sim, "focalizado")
  expect_equal(r$rango, rep(1:4, 2))
  expect_equal(r$semi_rep, rep(c(0.01, 0.01, 0.02, 0.01), 2))
})

test_that("only the first two shares (and the margin in option A) are targets; the 3rd is reported", {
  sim <- tibble::tibble(escenario = "A1", dignidad = rep(c("Prefecto 2023", "Alcalde Quito 2023"), each = 4),
                        cantidad = rep(c("A", "B", "C", MARGEN), 2), error = rep(c(0.002, 0.002, 0.02, 0.003), 2),
                        semi_sd = 0.009, semi_srs = rep(c(0.009, 0.009, 0.03, 0.009), 2), juntas = 100, actas = 150)
  r <- with_reported_margin(sim, "focalizado")
  expect_equal(r$objetivo, rep(c(TRUE, TRUE, FALSE, TRUE), 2))
  expect_equal(with_reported_margin(sim, "focalizado", "cuotas")$objetivo, rep(c(TRUE, TRUE, FALSE, FALSE), 2))
  o <- field_options(sim, E = 1, conf = 0.95, margen = "focalizado")
  expect_true(o$cumple)              # the 3rd candidate's 2 pp error does not count
  expect_equal(o$error_otros, 2)     # but it is reported
  expect_equal(o$semi_otros, 3)      # with the cautious margin
})

test_that("wave 1 is a share of the sample in each stratum, with weights that add up to N", {
  pop <- junta_population(fake_jm_7(), fake_jm_7(quito_only = TRUE))
  set.seed(31)
  s <- draw_juntas(pop, c("Quito urbano" = 40L, "Mejía" = 12L), max_recinto = 4)
  w1 <- subsample_wave(s, c("Quito urbano" = 0.5, "Mejía" = 0.5))
  expect_true(all(w1$fila %in% s$fila))
  expect_equal(length(w1$fila), sum(round(table(s$estrato) * 0.5)))
  for (lab in unique(w1$estrato)) expect_equal(sum(w1$w[w1$estrato == lab]), w1$N[w1$estrato == lab][1])
  sim <- simulate_waves(pop, c("Quito urbano" = 40L, "Mejía" = 12L), c("Quito urbano" = 0.5, "Mejía" = 0.5),
                        n_sim = 30, seed = 32, conf = 0.95, max_recinto = 4, etiqueta = "x")
  expect_setequal(unique(sim$ola), c("Ola 1", "Olas 1 y 2"))
  j <- dplyr::summarise(sim, juntas = dplyr::first(juntas), .by = c(replica, ola))
  expect_true(all(j$juntas[j$ola == "Ola 1"] < j$juntas[j$ola == "Olas 1 y 2"]))
})
