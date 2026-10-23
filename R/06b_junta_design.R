# Phase 5b / 6b: junta-level design.
#
# How the client works (owner, from the client, 20 Oct 2026): the CNE lets the party
# register one delegate per junta (7121 in the province). The quick-count delegates
# are not extra people: in each sampled junta, that junta's own delegate sends its
# actas through an app (1 acta outside Quito, 2 in Quito). A larger sample costs no
# extra staff, and juntas can be drawn directly: there is no clustering penalty (rho).
#
# Design: in each stratum, a systematic sample of juntas over the list sorted by
# canton, parroquia, zona, recinto, sex and junta number (random start). The sort
# order spreads the sample over parroquias and recintos (implicit stratification),
# so recintos get 0, 1, 2 or more juntas by chance, as the client has seen before.
# One sample serves both dignidades: in Quito the same junta gives both actas.
#
# Estimator: combined ratio estimator, weights N_h / n_h. Variance, two versions:
#   "srs": stratified simple random sampling (with fpc): ignores the sort order,
#          conservative;
#   "sd":  successive differences in the sort order within each stratum, the usual
#          estimator for systematic samples: credits the implicit stratification.
# Sizing: the textbook formula (stratified SRS of juntas, Neyman allocation) gives a
# conservative size; the simulation on the 2023 juntas then finds the smallest share
# of that size whose real error, reported margin of error and coverage meet the target.

# ---- population ---------------------------------------------------------------------------

#' Junta-level simulation population: the population of [sim_population()] plus the
#' sorted junta list per stratum.
junta_population <- function(jm_prov, jm_quito, n_cand = 3L) {
  pop <- sim_population(jm_prov, jm_quito, n_cand)
  hdr <- jm_prov$juntas
  orden <- order(match(as.character(hdr$estrato), ESTRATOS), hdr$canton, hdr$parroquia_codigo,
                 hdr$zona_codigo, hdr$recinto_codigo, hdr$sexo, hdr$junta_num)
  est <- as.character(hdr$estrato)[orden]
  pop$filas_estrato <- split(orden, factor(est, levels = intersect(ESTRATOS, unique(est)))) # rows in sort order
  pop$recinto <- hdr$recinto_codigo
  pop
}

# ---- formula sizes ------------------------------------------------------------------------

#' Juntas per stratum for a total of n, Neyman on W_h * max S_h, minimum per stratum.
allocate_juntas <- function(strata, n, minimo = 4L) {
  J <- vapply(strata, function(s) sum(s$J), numeric(1))
  X <- J * vapply(strata, `[[`, numeric(1), "xbar")
  w <- X / sum(X) * vapply(strata, function(s) max(s$S), numeric(1))
  stats::setNames(as.integer(pmin(J, pmax(minimo, round(n * w / sum(w))))), names(strata))
}

#' Half-width of every target under stratified SRS of juntas (with fpc).
evaluate_junta_design <- function(strata, n_h, z) {
  J <- vapply(strata, function(s) sum(s$J), numeric(1))
  X <- J * vapply(strata, `[[`, numeric(1), "xbar")
  W <- X / sum(X)
  V <- 0
  for (h in names(strata)) V <- V + W[[h]]^2 * strata[[h]]$S^2 / n_h[[h]] * (1 - n_h[[h]] / J[[h]])
  z * sqrt(V)
}

#' Smallest total of juntas (formula) that reaches E for every target.
junta_formula_size <- function(strata, E, conf, minimo = 4L) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  ok <- function(n) max(evaluate_junta_design(strata, allocate_juntas(strata, n, minimo), z)) <= E / 100
  lo <- minimo * length(strata)
  hi <- sum(vapply(strata, function(s) sum(s$J), numeric(1)))
  if (!ok(hi)) return(NULL)
  while (lo < hi) {
    mid <- (lo + hi) %/% 2
    if (ok(mid)) hi <- mid else lo <- mid + 1
  }
  allocate_juntas(strata, lo, minimo)
}

#' One sample for both dignidades: formula sizes of each, the larger in each Quito stratum.
junta_formula_integrated <- function(params, frame, E, conf, objetivo = "ambos", version = "punto",
                                     dig_prov = "Prefecto 2023", dig_quito = "Alcalde Quito 2023", minimo = 4L,
                                     n_cand = 3L) {
  sp <- design_strata(params, frame, dig_prov, objetivo, version, n_cand = n_cand)
  sq <- design_strata(params, frame, dig_quito, objetivo, version, n_cand = n_cand)
  np <- junta_formula_size(sp, E, conf, minimo)
  nq <- junta_formula_size(sq, E, conf, minimo)
  n_h <- np
  for (h in names(nq)) n_h[[h]] <- max(n_h[[h]], nq[[h]])
  list(n_h = n_h, n_prefecto = np, n_alcalde = nq)
}

# ---- one sample -----------------------------------------------------------------------------

#' Systematic sample of n of the rows `filas` (random start, interval length / n).
systematic_rows <- function(filas, n) {
  N <- length(filas)
  I <- N / n
  filas[pmin(N, floor(stats::runif(1, 0, I) + I * (seq_len(n) - 1)) + 1)]
}

#' Recintos that would get more than `cap` juntas in a systematic sample of n from a
#' stratum (expected M_r / k > cap). Iterative: once they are set apart with exactly
#' `cap` juntas each, the interval of the rest shrinks and more recintos may exceed it.
#' @param rec Recinto of each junta of the stratum (any order).
large_recintos <- function(rec, n, cap) {
  M <- table(rec)
  g <- character(0)
  repeat {
    n_rest <- n - cap * length(g)
    N_rest <- length(rec) - sum(M[g])
    if (n_rest <= 0 || N_rest <= 0) return(character(0)) # the cap cannot hold: no cap in this stratum
    k <- N_rest / n_rest
    nuevos <- setdiff(names(M)[M / k > cap], g)
    if (!length(nuevos)) return(g)
    g <- c(g, nuevos)
  }
}

#' Systematic sample of juntas in each stratum (random start, interval N_h / n_h).
#'
#' With `max_recinto` (owner, 22 Oct 2026: at most 4 sampled juntas per recinto, so that
#' a recinto keeps delegates for the rest of the observation), the recintos that would
#' get more are set apart: each gets exactly `max_recinto` juntas (systematic within the
#' recinto) and is its own sub-stratum (label "<stratum>|R<recinto>", weight M_r / cap);
#' the rest of the stratum gets the remaining juntas by the same systematic draw. Every
#' junta keeps a known selection probability, so the estimator stays unbiased. Without a
#' cap the draw (and its random numbers) is exactly the earlier one.
#' @return List: fila (population rows), estrato (sub-stratum label), w (weights), N.
draw_juntas <- function(pop, n_h, max_recinto = Inf) {
  out <- lapply(names(n_h), function(h) {
    filas <- pop$filas_estrato[[h]]
    N <- length(filas)
    n <- min(n_h[[h]], N)
    rec <- as.character(pop$recinto[filas])
    g <- if (is.finite(max_recinto)) large_recintos(rec, n, max_recinto) else character(0)
    partes <- if (!length(g)) {
      list(list(f = filas, n = n, lab = h))
    } else {
      c(list(list(f = filas[!rec %in% g], n = n - max_recinto * length(g), lab = h)),
        lapply(g, function(r) list(f = filas[rec == r], n = max_recinto, lab = paste0(h, "|R", r))))
    }
    res <- lapply(partes, function(p) {
      sel <- systematic_rows(p$f, p$n)
      list(fila = sel, estrato = rep(p$lab, length(sel)), w = rep(length(p$f) / length(sel), length(sel)),
           N = rep(length(p$f), length(sel)))
    })
    list(fila = unlist(lapply(res, `[[`, "fila")), estrato = unlist(lapply(res, `[[`, "estrato")),
         w = unlist(lapply(res, `[[`, "w")), N = unlist(lapply(res, `[[`, "N")))
  })
  list(fila = unlist(lapply(out, `[[`, "fila")), estrato = unlist(lapply(out, `[[`, "estrato")),
       w = unlist(lapply(out, `[[`, "w")), N = unlist(lapply(out, `[[`, "N")))
}

#' Ratio estimate and half-widths ("srs" and "sd") for one junta sample.
#' @param s Output of [draw_juntas()] (possibly a subset of it).
#' @param filas Rows of the sample in the domain's matrices.
estimate_juntas <- function(s, dom, filas, z) {
  Y <- dom$Y[filas, , drop = FALSE]
  x <- dom$x[filas]
  X_hat <- sum(s$w * x)
  R_hat <- colSums(s$w * Y) / X_hat
  E <- Y - outer(x, R_hat)
  V_srs <- V_sd <- 0
  for (h in unique(s$estrato)) {
    i <- which(s$estrato == h)
    n <- length(i)
    if (n < 2) next
    N <- s$N[i[1]]
    Eh <- E[i, , drop = FALSE]
    f <- (1 - n / N) * N^2 / n
    V_srs <- V_srs + f * apply(Eh, 2, stats::var)
    V_sd <- V_sd + f * colSums(diff(Eh)^2) / (2 * (n - 1))
  }
  list(R = R_hat, semi_srs = z * sqrt(V_srs) / X_hat, semi_sd = z * sqrt(V_sd) / X_hat)
}

# ---- field losses ------------------------------------------------------------------------------
#
# On the night some sampled juntas will not report (delegate absent, no signal, acta
# illegible). Two mechanisms: a junta fails on its own (probability by stratum), or a
# whole recinto fails (no signal there, so every sampled junta in it is lost). The
# juntas that report are reweighted within their stratum (N_h / juntas reporting), which
# is right if the losses are unrelated to the vote within a stratum; the recinto losses
# are a first step towards losses that are not.

#' Recintos that fail as a whole in one replicate.
fallen_recintos <- function(recintos, perdida_recinto = 0) {
  if (perdida_recinto <= 0) return(integer(0)) # no random numbers used: results without losses are unchanged
  u <- unique(recintos)
  u[stats::runif(length(u)) < perdida_recinto]
}

#' The part of a junta sample that reports, with weights N_h / (juntas reporting in h).
#' @param perdida Probability that a junta fails: one number, or named by stratum.
#' @param caidos Recintos that failed as a whole ([fallen_recintos()]).
apply_losses <- function(s, pop, perdida = 0, caidos = integer(0)) {
  if (all(perdida <= 0) && !length(caidos)) return(s)
  ok <- !(pop$recinto[s$fila] %in% caidos)
  if (any(perdida > 0)) {
    p <- if (length(perdida) == 1) rep(perdida, length(s$fila)) else unname(perdida[s$estrato])
    p[is.na(p)] <- 0
    ok <- ok & stats::runif(length(s$fila)) >= p
  }
  r <- lapply(s, `[`, ok)
  n_resp <- table(r$estrato)
  r$w <- r$N / as.vector(n_resp[r$estrato])
  r
}

#' Buffer: inflate an allocation so that, after the expected losses, about n_h juntas report.
#' @param N Juntas per stratum in the population (caps the result).
buffer_allocation <- function(n_h, perdida, perdida_recinto = 0, N = NULL) {
  p <- if (length(perdida) == 1) rep(perdida, length(n_h)) else unname(perdida[names(n_h)])
  p[is.na(p)] <- 0
  out <- ceiling(n_h / ((1 - p) * (1 - perdida_recinto)))
  if (!is.null(N)) out <- pmin(out, N[names(n_h)])
  stats::setNames(as.integer(out), names(n_h))
}

# ---- replicates -------------------------------------------------------------------------------

#' Simulate the junta design. With `n_h_alcalde = NULL` one sample serves both
#' dignidades (Alcalde from its Quito juntas); otherwise Alcalde gets its own,
#' independent sample of Quito juntas (two separate samples). `perdida` and
#' `perdida_recinto` simulate field losses ([apply_losses()]); `juntas_plan` and
#' `actas_plan` count the sample as drawn, `juntas` and `actas` what reports.
simulate_junta_design <- function(pop, n_h, n_sim, seed, conf = 0.99, etiqueta = "", n_h_alcalde = NULL,
                                  perdida = 0, perdida_recinto = 0, max_recinto = Inf) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  set.seed(seed)
  purrr::map_dfr(seq_len(n_sim), function(b) {
    s0 <- draw_juntas(pop, n_h, max_recinto)
    sq0 <- if (!is.null(n_h_alcalde)) draw_juntas(pop, n_h_alcalde, max_recinto)
    plan <- c(s0$fila, sq0$fila)
    actas_plan <- if (is.null(n_h_alcalde)) length(s0$fila) + sum(!is.na(pop$quito$fila[s0$fila])) else length(plan)
    caidos <- fallen_recintos(pop$recinto[plan], perdida_recinto)
    s <- apply_losses(s0, pop, perdida, caidos)
    ep <- estimate_juntas(s, pop$prov, s$fila, z)
    if (is.null(n_h_alcalde)) {
      keep <- !is.na(pop$quito$fila[s$fila])
      sq <- lapply(s, `[`, keep)
      filas_todas <- s$fila
    } else {
      sq <- apply_losses(sq0, pop, perdida, caidos)
      filas_todas <- c(s$fila, sq$fila)
    }
    eq <- estimate_juntas(sq, pop$quito, pop$quito$fila[sq$fila], z)
    rec <- pop$recinto[filas_todas]
    dplyr::bind_rows(
      tibble::tibble(dignidad = "Prefecto 2023", cantidad = colnames(pop$prov$Y), error = unname(ep$R - pop$prov$verdad),
                     semi_srs = unname(ep$semi_srs), semi_sd = unname(ep$semi_sd)),
      tibble::tibble(dignidad = "Alcalde Quito 2023", cantidad = colnames(pop$quito$Y), error = unname(eq$R - pop$quito$verdad),
                     semi_srs = unname(eq$semi_srs), semi_sd = unname(eq$semi_sd))
    ) |>
      dplyr::mutate(replica = b, juntas_plan = length(unique(plan)), actas_plan = actas_plan,
                    juntas = length(unique(filas_todas)), actas = length(s$fila) + length(sq$fila),
                    recintos = length(unique(rec)), max_por_recinto = max(tabulate(match(rec, unique(rec)))), .before = 1)
  }) |>
    dplyr::mutate(escenario = etiqueta, .before = 1)
}

#' Simulations without field losses (or built before losses existed) may lack the
#' columns of the sample as drawn: then drawn = reported.
with_plan_columns <- function(sim) {
  if (!"juntas_plan" %in% names(sim)) sim$juntas_plan <- sim$juntas
  if (!"actas_plan" %in% names(sim)) sim$actas_plan <- sim$actas
  if (!"max_por_recinto" %in% names(sim)) sim$max_por_recinto <- NA_integer_
  sim
}

#' Summary in percentage points (coverage computed before the half-widths are averaged).
#' The quantile of the error is taken at each row's `conf` when the simulation has
#' that column (scaling runs), otherwise at `nivel`. (An argument called `conf` would
#' be masked by the data column of the same name inside summarise().)
summarise_junta_sim <- function(sim, nivel = 0.99) {
  has_conf <- "conf" %in% names(sim)
  sim <- with_plan_columns(sim)
  sim |>
    dplyr::summarise(
      replicas = dplyr::n(), juntas_plan = stats::median(juntas_plan), actas_plan = stats::median(actas_plan),
      juntas = stats::median(juntas), actas = stats::median(actas),
      recintos = stats::median(recintos), max_por_recinto = max(max_por_recinto),
      cobertura_srs = mean(abs(error) <= semi_srs), cobertura_sd = mean(abs(error) <= semi_sd),
      sesgo = 100 * mean(error), rmse = 100 * sqrt(mean(error^2)),
      error_cuantil = 100 * stats::quantile(abs(error), if (has_conf) dplyr::first(conf) else nivel, names = FALSE),
      cobertura_max = mean(abs(error) <= pmax(semi_sd, semi_srs)),
      semi_srs = 100 * mean(semi_srs), semi_sd = 100 * mean(semi_sd),
      .by = c(escenario, dignidad, cantidad)
    )
}

#' Scale the formula allocation and simulate each size.
simulate_junta_scaling <- function(pop, n_h, factores, n_sim, seed, conf, E, minimo = 4L) {
  purrr::map_dfr(seq_along(factores), function(i) {
    n_s <- stats::setNames(pmax(minimo, round(n_h * factores[i])), names(n_h))
    simulate_junta_design(pop, n_s, n_sim, seed + i, conf = conf,
                          etiqueta = sprintf("E %.1f, conf %.2f, factor %.2f", E, conf, factores[i])) |>
      dplyr::mutate(E = E, conf = conf, factor = factores[i], n_h = list(n_s))
  })
}

#' Reported margin of error used for sizing and on the night.
#' The sd margin alone under-covers candidates whose vote is concentrated in a few
#' parroquias (third candidate for Alcalde in 2019 and 2023: 0.92 to 0.94 at 95%), and
#' more juntas do not fix that. Rules:
#'   "sd":         the successive-differences margin for every quantity;
#'   "max":        the larger of the sd and SRS margins for every quantity (safe, but the
#'                 SRS margin is about 1.5 times the real error: +45% juntas at 99%);
#'   "focalizado": sd for the first two candidates and the margin between them, the larger
#'                 of the two for the other candidates (where sd was seen to fail).
#' The rank of a quantity is its order within the dignidad (1st, 2nd, 3rd, margin), as
#' the simulation writes them.
#' Targets (owner, 21 Oct 2026): the shares of the first `n_obj` candidates (2) and, for
#' option A, the margin between the first two. The other candidates (3rd) are estimated
#' and reported with their own margin, but they do not size the sample.
#' @param cantidades "todas" (shares of the first three and the margin, option A) or
#'   "cuotas" (shares only, option B: the margin is reported but not required).
with_reported_margin <- function(sim, margen = c("max", "sd", "focalizado"), cantidades = c("todas", "cuotas"),
                                 n_obj = 2L) {
  margen <- match.arg(margen)
  cantidades <- match.arg(cantidades)
  sim |>
    dplyr::mutate(rango = match(cantidad, unique(cantidad)), .by = dignidad) |>
    dplyr::mutate(
      semi_rep = switch(margen,
        sd = semi_sd,
        max = pmax(semi_sd, semi_srs),
        focalizado = dplyr::if_else(rango <= 2L | cantidad == MARGEN, semi_sd, pmax(semi_sd, semi_srs))
      ),
      objetivo = (cantidad != MARGEN & rango <= n_obj) | (cantidad == MARGEN & cantidades == "todas")
    )
}

#' Smallest factor whose real error and mean reported half-width are within E
#' for every target quantity of both dignidades, with coverage at least conf - 0.01.
#' See [with_reported_margin()] for `margen` and `cantidades`.
junta_size_choice <- function(sim, margen = "max", cantidades = "todas") {
  s <- with_reported_margin(sim, margen, cantidades) |>
    dplyr::filter(objetivo) |>
    dplyr::summarise(
      cobertura = mean(abs(error) <= semi_rep), error = stats::quantile(abs(error), dplyr::first(conf), names = FALSE),
      semi = mean(semi_rep), juntas = stats::median(juntas), actas = stats::median(actas),
      max_por_recinto = max(max_por_recinto), .by = c(E, conf, factor, dignidad, cantidad)
    ) |>
    dplyr::summarise(peor_error = 100 * max(error), peor_semi = 100 * max(semi), cobertura_min = min(cobertura),
                     juntas = dplyr::first(juntas), actas = dplyr::first(actas), max_por_recinto = max(max_por_recinto),
                     .by = c(E, conf, factor)) |>
    dplyr::mutate(cumple = peor_error <= E & peor_semi <= E & cobertura_min >= conf - 0.01) |>
    dplyr::arrange(E, conf, factor)
  s
}

#' The chosen junta design for one target: allocation of the smallest factor that complies.
junta_design_chosen <- function(sim, eleccion, E, conf) {
  f <- eleccion |>
    dplyr::filter(.data$E == !!E, .data$conf == !!conf, cumple) |>
    dplyr::slice_min(factor, n = 1)
  if (!nrow(f)) return(NULL)
  r <- dplyr::filter(sim, .data$E == !!E, .data$conf == !!conf, factor == f$factor)
  list(E = E, conf = conf, factor = f$factor, n_h = r$n_h[[1]], resumen = f)
}


#' Scale an allocation to a new total (minimum per stratum).
scale_allocation <- function(n_h, total, minimo = 4L) {
  stats::setNames(as.integer(pmax(minimo, round(n_h * total / sum(n_h)))), names(n_h))
}

#' One row per field scenario: sample drawn, what reports, worst real error and
#' reported half-width ("sd") over both dignidades, and whether the target is met
#' (same rule as [junta_size_choice()]).
field_options <- function(sim, E = 1, conf = 0.99, margen = "max", cantidades = "todas") {
  with_plan_columns(sim) |>
    with_reported_margin(margen, cantidades) |>
    dplyr::summarise(
      cobertura = mean(abs(error) <= semi_rep), error = stats::quantile(abs(error), conf, names = FALSE),
      semi = mean(semi_rep), juntas_plan = stats::median(juntas_plan), actas_plan = stats::median(actas_plan),
      juntas = stats::median(juntas), actas = stats::median(actas), objetivo = dplyr::first(objetivo),
      rango = dplyr::first(rango), max_por_recinto = max(max_por_recinto), .by = c(escenario, dignidad, cantidad)
    ) |>
    dplyr::mutate(quito = grepl("Quito", dignidad)) |> # Quito domain (Alcalde, or a province-wide race in Quito)
    dplyr::summarise(
      juntas_muestra = dplyr::first(juntas_plan), actas_muestra = dplyr::first(actas_plan),
      juntas_reportan = dplyr::first(juntas), actas_reportan = dplyr::first(actas),
      max_por_recinto = max(max_por_recinto),
      peor_error_prefecto = 100 * max(error[!quito & objetivo]),
      peor_error_alcalde = 100 * max(error[quito & objetivo]),
      peor_semi = 100 * max(semi[objetivo]), cobertura_min = min(cobertura[objetivo]),
      semi_margen = 100 * max(semi[cantidad == MARGEN]),
      # the shares alone (in option A, peor_semi also includes the margin 1-2)
      semi_cuotas = 100 * max(semi[objetivo & cantidad != MARGEN]),
      # reported, not required: the other candidates (3rd), with their own margin
      error_otros = 100 * max(c(-Inf, error[!objetivo & cantidad != MARGEN])),
      semi_otros = 100 * max(c(-Inf, semi[!objetivo & cantidad != MARGEN])),
      cobertura_otros = min(c(Inf, cobertura[!objetivo & cantidad != MARGEN])), .by = escenario
    ) |>
    dplyr::mutate(cumple = peor_error_prefecto <= E & peor_error_alcalde <= E & peor_semi <= E &
                    cobertura_min >= conf - 0.01)
}

#' The field scenarios for one confidence level: best case (A1, every sampled delegate
#' reports) and with losses (B1 without buffer, B2 with buffer). The seeds (seed + 1,
#' + 4, + 5) are those of the earlier six-scenario version, so results are unchanged.
#' @param n_h Chosen allocation.
field_scenarios <- function(pop, n_h, perdida, perdida_recinto, n_sim, seed, conf, max_recinto = Inf) {
  colchon <- buffer_allocation(n_h, perdida, perdida_recinto, N = lengths(pop$filas_estrato))
  dplyr::bind_rows(
    simulate_junta_design(pop, n_h, n_sim, seed + 1L, conf, "A1. Optimista: diseno elegido, una muestra",
                          max_recinto = max_recinto),
    simulate_junta_design(pop, n_h, n_sim, seed + 4L, conf, "B1. Con perdidas, sin colchon: diseno elegido",
                          perdida = perdida, perdida_recinto = perdida_recinto, max_recinto = max_recinto),
    simulate_junta_design(pop, colchon, n_sim, seed + 5L, conf, "B2. Con perdidas, con colchon: diseno elegido + reserva",
                          perdida = perdida, perdida_recinto = perdida_recinto, max_recinto = max_recinto)
  ) # no `conf` column: inside field_options() it would mask the argument of the same name
}
