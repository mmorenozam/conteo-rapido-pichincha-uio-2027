# Phase 7: stability of the design parameters across elections, and Bayesian
# assurance for the sample size.
#
# Races with junta-level results (all validated against the CNE SPSS files):
#   Prefecto 2019, Prefecto 2023, Presidente 2025 (first round): all Pichincha;
#   Alcalde Quito 2019, Alcalde Quito 2023: Quito.
# For each race, S is estimated by stratum exactly as for 2023 (phase 4), and the
# design input is S_max: the largest S among the first three candidates and the
# margin between the first two ([design_inputs()]).
#
# Bayesian model (on the log scale, one observation per race x stratum):
#   log S_max[r, h] = a_h + b_r + e_rh,  b_r ~ N(0, tau^2),  e_rh ~ N(0, sigma^2)
#   a_h: stratum level (flat prior); b_r: how much more (or less) variable a whole
#   race is than a typical one; e_rh: stratum-specific departures.
#   tau^2, sigma^2 ~ Inverse-Gamma(a0, b0); default a0 = 2, b0 = 0.04 (prior mean
#   0.04, a log-scale sd of 0.2, i.e. races typically within about +/-20% of each other).
# Fitted by Gibbs sampling (all conditionals are conjugate). The predictive for a
# new race (2027) draws a new b and new e: S_h = exp(a_h + b_new + e_new,h).
# Prefecto 2027 and Alcalde 2027 get independent race effects.
#
# Assurance: for an allocation n_h of juntas, the probability (over the predictive)
# that the margin of error reported on the night is within E for both dignidades.
# The margin of error for given S comes from the textbook formula (stratified SRS of
# juntas), multiplied by a calibration factor c per dignidad, so that with the 2023
# S the formula reproduces the reported margin of the simulation of the chosen
# design (phase 6b): c = simulated / formula, at the chosen design.

# ---- reading the 2019 and 2025 files ------------------------------------------------------

#' Read a junta-level results file of any year (2019, 2023 or 2025 format).
#'
#' The 2019 files have surrogate recinto ids ("R19_<row of the 2019 distributivo>",
#' no CNE codes); they are turned into integers (the row number). Columns that only
#' some years have are kept as read.
read_juntas_hist <- function(path) {
  x <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                       locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  probs <- readr::problems(x)
  if (nrow(probs) > 0) cli::cli_abort("{.file {path}}: {nrow(probs)} parsing problem{?s}.")
  enteros <- intersect(c("dignidad", "provincia", "canton", "parroquia_codigo", "zona_codigo", "junta_id",
                         "junta_num", "sufragantes", "ausentismo", "blancos", "nulos", "votos", "op_codigo"),
                       names(x))
  x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(enteros), as.integer),
      # "R19_2640" -> 2640 (2019 surrogate id); "1402.0" -> 1402 (2023 / 2025 codes)
      recinto_codigo = as.integer(as.numeric(sub("^R[0-9]+_", "", recinto_codigo))),
      candidato = stringr::str_squish(candidato)
    )
}

#' Read, check, clean and build the vote matrix of one historical race.
historic_matrix <- function(path, label, parroquias) {
  j <- read_juntas_hist(path)
  assert_juntas_ok(j, label)
  cl <- assert_clean_ok(clean_juntas(j, label), label)
  junta_matrix(cl, parroquias)
}

# ---- stability table ----------------------------------------------------------------------

#' S_max by race and stratum, with the ratio to the 2023 race of the same dignidad
#' (Presidente 2025 is compared with Prefecto 2023, the other province-wide race).
stability_table <- function(params) {
  ref <- c("Prefecto 2019" = "Prefecto 2023", "Prefecto 2023" = "Prefecto 2023",
           "Presidente 2025" = "Prefecto 2023",
           "Alcalde Quito 2019" = "Alcalde Quito 2023", "Alcalde Quito 2023" = "Alcalde Quito 2023")
  d <- design_inputs(params)
  base <- dplyr::select(d, referencia = dignidad, estrato, S_max_2023 = S_max)
  d |>
    dplyr::mutate(referencia = unname(ref[dignidad])) |>
    dplyr::left_join(base, by = c("referencia", "estrato")) |>
    dplyr::transmute(dignidad, estrato = factor(estrato, levels = ESTRATOS), S_max, candidato_S_max,
                     razon_vs_2023 = S_max / S_max_2023, juntas) |>
    dplyr::arrange(estrato, dignidad)
}

# ---- Bayesian model for S -----------------------------------------------------------------

#' Gibbs sampler for log S_max = a_h + b_r + e (see the header of this file).
#'
#' @param inputs Output of [design_inputs()] for several races (columns dignidad, estrato, S_max).
#' @return List: a (draws x strata), b (draws x races), tau2, sigma2, estratos, carreras.
fit_s_model <- function(inputs, n_iter = 12000L, burn = 2000L, seed = 20270216L, a0 = 2, b0 = 0.04) {
  d <- dplyr::filter(inputs, estrato != "Total")
  y <- log(d$S_max)
  estratos <- intersect(ESTRATOS, unique(d$estrato))
  carreras <- unique(d$dignidad)
  h <- match(d$estrato, estratos)
  r <- match(d$dignidad, carreras)
  H <- length(estratos); R <- length(carreras); N <- length(y)
  n_h <- tabulate(h, H); n_r <- tabulate(r, R)
  set.seed(seed)
  a <- as.vector(tapply(y, h, mean)); b <- rep(0, R); tau2 <- b0 / (a0 - 1); sigma2 <- b0 / (a0 - 1)
  keep <- n_iter - burn
  A <- matrix(NA_real_, keep, H, dimnames = list(NULL, estratos))
  B <- matrix(NA_real_, keep, R, dimnames = list(NULL, carreras))
  T2 <- S2 <- numeric(keep)
  for (it in seq_len(n_iter)) {
    a <- stats::rnorm(H, as.vector(tapply(y - b[r], h, mean)), sqrt(sigma2 / n_h))
    prec <- n_r / sigma2 + 1 / tau2
    b <- stats::rnorm(R, (as.vector(tapply(y - a[h], r, sum)) / sigma2) / prec, sqrt(1 / prec))
    tau2 <- 1 / stats::rgamma(1, a0 + R / 2, b0 + sum(b^2) / 2)
    sigma2 <- 1 / stats::rgamma(1, a0 + N / 2, b0 + sum((y - a[h] - b[r])^2) / 2)
    if (it > burn) {
      k <- it - burn
      A[k, ] <- a; B[k, ] <- b; T2[k] <- tau2; S2[k] <- sigma2
    }
  }
  list(a = A, b = B, tau2 = T2, sigma2 = S2, estratos = estratos, carreras = carreras,
       datos = dplyr::select(d, dignidad, estrato, S_max), prior = c(a0 = a0, b0 = b0))
}

#' Predictive draws of S_max for a new race, by stratum (draws x strata).
predict_s <- function(fit, estratos = fit$estratos, seed = 1L) {
  set.seed(seed)
  K <- nrow(fit$a)
  b_new <- stats::rnorm(K, 0, sqrt(fit$tau2))
  e_new <- matrix(stats::rnorm(K * length(estratos), 0, sqrt(fit$sigma2)), K)
  exp(fit$a[, estratos, drop = FALSE] + b_new + e_new)
}

#' Posterior summary: race effects, spread between races and the predictive of S by
#' stratum next to the 2023 values.
summarise_s_model <- function(fit, params_2023) {
  ref <- design_inputs(params_2023)
  pred <- predict_s(fit)
  q <- function(v, p) stats::quantile(v, p, names = FALSE)
  list(
    dispersion = tibble::tibble(
      parametro = c("tau (entre elecciones, escala log)", "sigma (entre estratos dentro de una eleccion, escala log)"),
      mediana = c(q(sqrt(fit$tau2), 0.5), q(sqrt(fit$sigma2), 0.5)),
      inf_90 = c(q(sqrt(fit$tau2), 0.05), q(sqrt(fit$sigma2), 0.05)),
      sup_90 = c(q(sqrt(fit$tau2), 0.95), q(sqrt(fit$sigma2), 0.95))
    ),
    efectos_eleccion = tibble::tibble(dignidad = fit$carreras, factor_mediana = exp(apply(fit$b, 2, stats::median)),
                                      factor_inf_90 = exp(apply(fit$b, 2, q, 0.05)), factor_sup_90 = exp(apply(fit$b, 2, q, 0.95))),
    predictiva = purrr::map_dfr(colnames(pred), function(h) tibble::tibble(
      estrato = h, S_mediana = q(pred[, h], 0.5), S_inf_90 = q(pred[, h], 0.05), S_sup_90 = q(pred[, h], 0.95),
      S_prefecto_2023 = ref$S_max[ref$dignidad == "Prefecto 2023" & ref$estrato == h][1],
      S_alcalde_2023 = ref$S_max[ref$dignidad == "Alcalde Quito 2023" & ref$estrato == h][1]
    ))
  )
}

# ---- assurance ----------------------------------------------------------------------------

#' Margin of error of the junta design (formula, stratified SRS) for one S per stratum.
junta_half_width <- function(strata, n_h, S, z) {
  st <- strata[names(S)]
  for (h in names(S)) st[[h]]$S <- S[[h]]
  evaluate_junta_design(st, n_h[names(S)], z)
}

#' Calibration factor per dignidad: simulated reported margin of the chosen design
#' over the formula margin with the 2023 S_max, at the same allocation.
calibration_factors <- function(sim, escenario, n_h, params_2023, frame, conf,
                                digs = c("Prefecto 2023", "Alcalde Quito 2023")) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  semi <- sim |>
    dplyr::filter(.data$escenario == !!escenario) |>
    dplyr::summarise(semi = mean(semi_sd), .by = c(dignidad, cantidad)) |>
    dplyr::summarise(semi = max(semi), .by = dignidad)
  ref <- design_inputs(params_2023)
  purrr::map_dfr(digs, function(dg) {
    st <- design_strata(params_2023, frame, dg, "ambos")
    S <- stats::setNames(ref$S_max[ref$dignidad == dg], ref$estrato[ref$dignidad == dg])[names(st)]
    hw <- junta_half_width(st, n_h, S, z)
    tibble::tibble(dignidad = dg, margen_simulado = semi$semi[semi$dignidad == dg],
                   margen_formula = hw, c = semi$semi[semi$dignidad == dg] / hw)
  })
}

#' Probability that the reported margin is within E for both dignidades, for each total
#' of juntas (the chosen allocation scaled to that total).
#' @param calib Output of [calibration_factors()].
assurance_curve <- function(fit, n_h, params_2023, frame, calib, E, conf, totales, seed = 20270217L,
                            digs = c("Prefecto 2023", "Alcalde Quito 2023")) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  st <- lapply(stats::setNames(digs, digs), function(dg) design_strata(params_2023, frame, dg, "ambos"))
  pred <- lapply(seq_along(digs), function(i) predict_s(fit, names(st[[i]]), seed = seed + i)) # independent races
  names(pred) <- digs
  cc <- stats::setNames(calib$c, calib$dignidad)
  # Vectorized form of evaluate_junta_design(): V = sum_h W_h^2 S_h^2 / n_h (1 - n_h / J_h)
  pesos <- lapply(st, function(sd) {
    J <- vapply(sd, function(s) sum(s$J), numeric(1))
    X <- J * vapply(sd, `[[`, numeric(1), "xbar")
    list(J = J, W2 = (X / sum(X))^2)
  })
  purrr::map_dfr(totales, function(tot) {
    n_s <- scale_allocation(n_h, tot)
    hw <- sapply(digs, function(dg) {
      w <- pesos[[dg]]
      n <- n_s[names(w$J)]
      cc[[dg]] * z * sqrt(as.vector(pred[[dg]][, names(w$J), drop = FALSE]^2 %*% (w$W2 / n * (1 - n / w$J))))
    })
    peor <- apply(hw, 1, max)
    tibble::tibble(E = E, conf = conf, juntas = sum(n_s), prob_cumple = mean(peor <= E / 100),
                   margen_mediana = 100 * stats::median(peor), margen_p90 = 100 * stats::quantile(peor, 0.9, names = FALSE),
                   prob_prefecto = mean(hw[, digs[1]] <= E / 100), prob_alcalde = mean(hw[, digs[2]] <= E / 100))
  })
}

#' Smallest total with at least the given assurance, best case and with buffer.
assurance_sizes <- function(curva, n_h, perdida, perdida_recinto, N, niveles = c(0.5, 0.8, 0.9)) {
  purrr::map_dfr(niveles, function(p) {
    ok <- dplyr::filter(curva, prob_cumple >= p)
    if (!nrow(ok)) return(tibble::tibble(conf = curva$conf[1], seguridad = p, juntas = NA_integer_, juntas_con_colchon = NA_integer_))
    tot <- min(ok$juntas)
    n_s <- scale_allocation(n_h, tot)
    tibble::tibble(conf = curva$conf[1], seguridad = p, juntas = sum(n_s),
                   juntas_con_colchon = sum(buffer_allocation(n_s, perdida, perdida_recinto, N = N)))
  })
}

# ---- weighted 2019 + 2023 design (owner, 21 Oct 2026) -------------------------------------
#
# The sample is sized for the variability of the last two seccionales, with 2/3 of the
# weight on 2023 (the most recent) and 1/3 on 2019. Variances are averaged, since the
# sample size grows with S^2:  S^2 = w S^2_2023 + (1 - w) S^2_2019, by dignidad, stratum
# and quantity (1st, 2nd, 3rd candidate by rank and the margin between the first two).
# The size is then checked by simulation on the 2023 AND the 2019 juntas: the design
# must meet the target in both elections. Presidente 2025 is not used for sizing (a
# national, two-candidate race); it is only a stress test of the chosen design.

#' Weighted parameters: S^2 (and rho) averaged between two races, by stratum and rank.
#' Valid votes per junta, juntas and candidate names come from the newer race.
pool_parameters <- function(p_new, p_old, label, w = 2 / 3, n_cand = 3L) {
  key <- function(p) p |>
    dplyr::filter(estrato != "Total", orden <= n_cand | candidato == MARGEN) |>
    dplyr::mutate(rango = dplyr::if_else(candidato == MARGEN, "margen", as.character(orden)))
  old <- dplyr::select(key(p_old), estrato, rango, S_old = S, rho_old = rho)
  out <- dplyr::left_join(key(p_new), old, by = c("estrato", "rango"))
  if (anyNA(out$S_old)) cli::cli_abort("{label}: strata or ranks of the older race are missing.")
  out |>
    dplyr::mutate(dignidad = label, S_nueva = S, S_anterior = S_old, peso_nueva = w,
                  S = sqrt(w * S^2 + (1 - w) * S_old^2), rho = w * rho + (1 - w) * rho_old) |>
    dplyr::select(-S_old, -rho_old)
}

#' The Quito juntas of a province-wide vote matrix (to use a province-wide race,
#' such as Presidente 2025, as the "Quito" domain of a simulation population).
quito_subset <- function(jm) {
  q <- jm$juntas$canton == CANTON_QUITO
  list(juntas = jm$juntas[q, ], Y = jm$Y[q, , drop = FALSE], x = jm$x[q])
}

#' Dignidad labels that follow the election simulated (the simulation writes the 2023
#' labels; "2019" or "2025" replace the year, and a 2025 population is Presidente).
relabel_election <- function(sim, eleccion) {
  if (eleccion == "2025") {
    return(dplyr::mutate(sim, dignidad = dplyr::if_else(grepl("Quito", dignidad), "Presidente 2025 (Quito)", "Presidente 2025")))
  }
  dplyr::mutate(sim, dignidad = sub("2023", eleccion, dignidad, fixed = TRUE))
}

#' Scaling simulation of one allocation on several elections (named list of populations).
simulate_scaling_elections <- function(pops, n_h, factores, n_sim, seed, conf, E) {
  purrr::imap_dfr(pops, function(pop, eleccion) {
    simulate_junta_scaling(pop, n_h, factores, n_sim, seed + 100L * match(eleccion, names(pops)), conf = conf, E = E) |>
      relabel_election(eleccion) |>
      dplyr::mutate(eleccion = eleccion)
  })
}

#' Size choice when the design must meet the target in every election simulated.
#' Same rule as [junta_size_choice()] in each election; `cumple` only if all comply.
size_choice_elections <- function(sim, margen = "max", cantidades = "todas") {
  purrr::map_dfr(split(sim, sim$eleccion), function(s)
    dplyr::mutate(junta_size_choice(s, margen, cantidades), eleccion = s$eleccion[1])) |>
    dplyr::summarise(cumple = all(cumple), peor_error = max(peor_error), peor_semi = max(peor_semi),
                     cobertura_min = min(cobertura_min), juntas = dplyr::first(juntas), actas = max(actas),
                     max_por_recinto = max(max_por_recinto), eleccion_limitante = eleccion[which.max(peor_semi)],
                     .by = c(E, conf, factor)) |>
    dplyr::arrange(E, conf, factor)
}

#' Field options (best case, with buffer) of chosen designs on several elections.
#' @param disenos List of list(d = chosen design, conf).
#' @param cantidades "todas" (option A) or "cuotas" (option B), see [with_reported_margin()].
field_options_elections <- function(pops, disenos, frame, perdidas, n_sim, seed, cantidades = "todas", margen = "max",
                                    E = 1, max_recinto = Inf) {
  purrr::map_dfr(seq_along(disenos), function(i) purrr::map_dfr(seq_along(pops), function(k) {
    c0 <- disenos[[i]]
    sim <- field_scenarios(pops[[k]], c0$d$n_h, perdidas$junta, perdidas$recinto, n_sim, seed + 100L * i + 10L * k,
                           conf = c0$conf, max_recinto = max_recinto)
    dplyr::mutate(field_options(sim, E = E, conf = c0$conf, margen = margen, cantidades = cantidades),
                  eleccion = names(pops)[k], conf = c0$conf, .before = 1)
  }))
}

#' One row per option and confidence: juntas best case and with buffer, worst reported
#' margin of the target quantities and of the margin between first and second (over the
#' elections simulated), and whether the target is met in every election.
options_summary <- function(campo_a, campo_b) {
  f <- function(campo, opcion) campo |>
    dplyr::filter(startsWith(escenario, "A1") | startsWith(escenario, "B2")) |>
    dplyr::mutate(caso = dplyr::if_else(startsWith(escenario, "A1"), "optimista", "con_colchon")) |>
    dplyr::summarise(juntas = dplyr::first(juntas_muestra), actas = dplyr::first(actas_muestra),
                     peor_margen_objetivo = max(peor_semi), peor_margen_cuotas = max(semi_cuotas),
                     peor_margen_diferencia = max(semi_margen),
                     margen_tercero = max(semi_otros), max_por_recinto = max(max_por_recinto),
                     cumple_en_ambas = all(cumple), .by = c(conf, caso)) |>
    dplyr::mutate(opcion = opcion, .before = 1)
  dplyr::bind_rows(f(campo_a, "A. Cuotas y diferencia 1-2"), f(campo_b, "B. Solo cuotas")) |>
    dplyr::arrange(dplyr::desc(conf), opcion, dplyr::desc(caso))
}

# ---- nested waves (owner, 22 Oct 2026) ------------------------------------------------------
#
# On election night the full sample (+/-1 pp) is split, before election day, into two
# waves: wave 1 is a random subsample of the full sample with the size of the +/-1.5 pp
# design in each stratum; wave 2 is the rest. The first result (+/-1.5 pp) is published
# when wave 1 has arrived in every stratum; the final one (+/-1 pp) when both have.
# Wave 1 is a systematic subsample of the selected juntas, in list order, within each
# (sub-)stratum, so it keeps the geographic spread; its weights are N / n_wave1.

#' Wave 1 of a junta sample: a systematic subsample with share `f_h` in each stratum.
#' @param s Output of [draw_juntas()]. @param f_h Named by stratum (sub-strata use theirs).
subsample_wave <- function(s, f_h) {
  idx <- split(seq_along(s$fila), factor(s$estrato, levels = unique(s$estrato)))
  keep <- unlist(lapply(names(idx), function(lab) {
    i <- idx[[lab]]
    n <- length(i)
    n1 <- max(1L, min(n, as.integer(round(n * f_h[[sub("\\|R.*$", "", lab)]]))))
    I <- n / n1
    i[pmin(n, floor(stats::runif(1, 0, I) + I * (seq_len(n1) - 1)) + 1)]
  }), use.names = FALSE)
  r <- lapply(s, `[`, sort(keep))
  n1 <- table(r$estrato)
  r$w <- r$N / as.vector(n1[r$estrato])
  r
}

#' Simulate the two waves of one design: estimates from wave 1 alone and from waves 1 and 2.
#' Losses (if any) are drawn once on the full sample; wave 1 keeps its juntas that reported.
simulate_waves <- function(pop, n_h, f_h, n_sim, seed, conf, perdida = 0, perdida_recinto = 0,
                           max_recinto = Inf, etiqueta = "") {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  set.seed(seed)
  reweight <- function(ss) {
    n <- table(ss$estrato)
    ss$w <- ss$N / as.vector(n[ss$estrato])
    ss
  }
  purrr::map_dfr(seq_len(n_sim), function(b) {
    s0 <- draw_juntas(pop, n_h, max_recinto)
    w1 <- subsample_wave(s0, f_h)
    caidos <- fallen_recintos(pop$recinto[s0$fila], perdida_recinto)
    s_all <- apply_losses(s0, pop, perdida, caidos)
    s_w1 <- reweight(lapply(s_all, `[`, s_all$fila %in% w1$fila))
    one <- function(s, plan, ola) {
      ep <- estimate_juntas(s, pop$prov, s$fila, z)
      keep <- !is.na(pop$quito$fila[s$fila])
      sq <- lapply(s, `[`, keep)
      eq <- estimate_juntas(sq, pop$quito, pop$quito$fila[sq$fila], z)
      rec <- pop$recinto[s$fila]
      dplyr::bind_rows(
        tibble::tibble(dignidad = "Prefecto 2023", cantidad = colnames(pop$prov$Y), error = unname(ep$R - pop$prov$verdad),
                       semi_srs = unname(ep$semi_srs), semi_sd = unname(ep$semi_sd)),
        tibble::tibble(dignidad = "Alcalde Quito 2023", cantidad = colnames(pop$quito$Y), error = unname(eq$R - pop$quito$verdad),
                       semi_srs = unname(eq$semi_srs), semi_sd = unname(eq$semi_sd))
      ) |>
        dplyr::mutate(replica = b, ola = ola, escenario = paste(etiqueta, "|", ola),
                      juntas_plan = length(plan$fila), actas_plan = length(plan$fila) + sum(!is.na(pop$quito$fila[plan$fila])),
                      juntas = length(s$fila), actas = length(s$fila) + length(sq$fila),
                      max_por_recinto = max(tabulate(match(rec, unique(rec)))))
    }
    dplyr::bind_rows(one(s_w1, w1, "Ola 1"), one(s_all, s0, "Olas 1 y 2"))
  })
}

#' Wave check of the chosen designs on several elections: wave 1 judged against E1,
#' waves 1 and 2 against E, best case and with buffer and losses.
#' @param cfg List of list(conf, full = n_h of the full design, w1 = n_h of the wave-1 design).
wave_check <- function(pops, cfg, perdidas, n_sim, seed, margen, max_recinto, E = 1, E1 = 1.5) {
  N <- lengths(pops[[1]]$filas_estrato)
  purrr::map_dfr(seq_along(cfg), function(i) purrr::map_dfr(seq_along(pops), function(k) {
    c0 <- cfg[[i]]
    f_h <- c0$w1[names(c0$full)] / c0$full
    colchon <- buffer_allocation(c0$full, perdidas$junta, perdidas$recinto, N = N)
    sims <- dplyr::bind_rows(
      simulate_waves(pops[[k]], c0$full, f_h, n_sim, seed + 100L * i + 10L * k, c0$conf,
                     max_recinto = max_recinto, etiqueta = "Optimista"),
      simulate_waves(pops[[k]], colchon, f_h, n_sim, seed + 100L * i + 10L * k + 5L, c0$conf,
                     perdidas$junta, perdidas$recinto, max_recinto, "Con colchon")
    )
    dplyr::bind_rows(
      dplyr::mutate(field_options(dplyr::filter(sims, ola == "Ola 1"), E = E1, conf = c0$conf, margen = margen), objetivo_pp = E1),
      dplyr::mutate(field_options(dplyr::filter(sims, ola == "Olas 1 y 2"), E = E, conf = c0$conf, margen = margen), objetivo_pp = E)
    ) |>
      dplyr::mutate(eleccion = names(pops)[k], conf = c0$conf, .before = 1)
  }))
}
