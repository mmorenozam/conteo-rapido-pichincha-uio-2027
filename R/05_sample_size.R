# Phase 5: sample size.
#
# Design: in each stratum h, the certainty recintos (phase 3) are all taken and
# n_h - c_h further recintos are drawn by systematic PPS on juntas; in every
# selected recinto m juntas are recorded. The cost is the number of recintos
# (an observer costs the same whatever the number of actas), so the sample size
# is the smallest number of recintos that reaches the target precision.
#
# Approximate variance of the share (or margin) estimated by the combined ratio
# estimator, with W = share of the valid votes of the domain:
#   PPS part of h:        W_ph^2 * S_h^2 * (1 + (m - 1) rho_h) / (m * n_ph) * (1 - n_ph / N_ph)
#   certainty part of h:  W_ch^2 * S_h^2 * (1 - rho_h) / (m * c_h) * (1 - m / M_ch)
# where the certainty part only has the within-recinto variance (the recinto is
# certain; only the choice of juntas inside it varies). W is split between the
# two parts by juntas, and between strata by juntas x mean valid votes per junta.
# The half-width is z * sqrt(V). Phase 6 checks these approximations by
# simulating the full design on the 2023 juntas.

# ---- inputs -------------------------------------------------------------------------------

#' Stratum inputs for one dignidad: juntas per recinto in the 2027 frame and
#' S and rho for the target quantities.
#'
#' @param params Output of [estimate_parameters()] (with bootstrap columns if version = "superior").
#' @param objetivo "cuotas" (shares of the first three candidates), "margen" (margin
#'   between first and second) or "ambos".
#' @param version "punto" (point estimates) or "superior" (upper bootstrap bounds).
design_strata <- function(params, frame, dignidad, objetivo = c("ambos", "cuotas", "margen"),
                          version = c("punto", "superior"), n_cand = 3L) {
  objetivo <- match.arg(objetivo)
  version <- match.arg(version)
  p <- dplyr::filter(params, .data$dignidad == !!dignidad, estrato != "Total")
  p <- switch(objetivo,
    cuotas = dplyr::filter(p, orden <= n_cand, candidato != MARGEN),
    margen = dplyr::filter(p, candidato == MARGEN),
    ambos = dplyr::filter(p, orden <= n_cand | candidato == MARGEN)
  )
  if (version == "superior") {
    if (!all(c("S_sup", "rho_sup") %in% names(p)) || anyNA(p$S_sup)) {
      cli::cli_abort("Upper bounds requested but the parameters have no bootstrap columns.")
    }
    p <- dplyr::mutate(p, S = S_sup, rho = rho_sup)
  }
  estratos <- intersect(ESTRATOS, unique(p$estrato))
  lapply(stats::setNames(estratos, estratos), function(h) {
    ph <- dplyr::filter(p, estrato == h)
    J <- frame$juntas[as.character(frame$estrato) == h]
    if (!length(J)) cli::cli_abort("Stratum {h} not in the frame.")
    list(estrato = h, J = J, xbar = ph$votos_validos_por_junta[1],
         S = stats::setNames(ph$S, ph$candidato), rho = stats::setNames(ph$rho, ph$candidato))
  })
}

# ---- certainty, variance, allocation ------------------------------------------------------

#' Certainty flags for one stratum (same rule as [flag_certainty()], on a vector of sizes).
#'
#' If the rule would make every remaining recinto certainty, the whole stratum is
#' taken (all TRUE): the stratum is then enumerated, and its realized number of
#' recintos is the number of recintos of the stratum.
certainty_vector <- function(J, n) {
  cert <- rep(FALSE, length(J))
  repeat {
    quedan <- n - sum(cert)
    if (quedan <= 0) return(rep(TRUE, length(J)))
    nuevo <- cert | J >= sum(J[!cert]) / quedan
    if (identical(nuevo, cert)) return(cert)
    cert <- nuevo
  }
}

#' Half-width (in proportion units) of every target quantity for an allocation.
#' @param n_h Named integer vector: recintos to select per stratum (certainty included).
#' @return List: semiamplitud (named vector), certeza (certainty recintos per stratum),
#'   recintos (realized recintos per stratum: at least the certainty recintos).
evaluate_design <- function(strata, n_h, m, z) {
  X <- vapply(strata, function(s) sum(s$J) * s$xbar, numeric(1))
  Xt <- sum(X)
  V <- 0
  cert_h <- real_h <- integer(0)
  for (h in names(strata)) {
    s <- strata[[h]]
    cert <- if (n_h[[h]] >= length(s$J)) rep(TRUE, length(s$J)) else certainty_vector(s$J, n_h[[h]])
    c_h <- sum(cert)
    n_p <- max(n_h[[h]] - c_h, 0)
    N_p <- sum(!cert)
    W <- X[[h]] / Xt
    Wc <- W * sum(s$J[cert]) / sum(s$J)
    Wp <- W * sum(s$J[!cert]) / sum(s$J)
    if (n_p > 0) {
      V <- V + Wp^2 * s$S^2 * (1 + (m - 1) * s$rho) / (m * n_p) * (1 - n_p / N_p)
    }
    if (c_h > 0) {
      M_c <- mean(s$J[cert])
      V <- V + Wc^2 * s$S^2 * (1 - s$rho) / (m * c_h) * max(0, 1 - m / M_c)
    }
    cert_h[h] <- c_h
    real_h[h] <- c_h + n_p
  }
  list(semiamplitud = z * sqrt(V), certeza = cert_h, recintos = real_h)
}

#' Allocate n recintos across strata: proportional to juntas, or Neyman on the
#' per-recinto standard deviation (W_h * max S_h * sqrt(deff_h)), with a minimum
#' per stratum and no more than the recintos of the stratum.
allocate_recintos <- function(strata, n, m, asignacion = c("neyman", "proporcional"), minimo = 4L) {
  asignacion <- match.arg(asignacion)
  N_h <- vapply(strata, function(s) length(s$J), integer(1))
  J_h <- vapply(strata, function(s) sum(s$J), numeric(1))
  w <- if (asignacion == "proporcional") J_h else {
    X <- J_h * vapply(strata, `[[`, numeric(1), "xbar")
    X / sum(X) * vapply(strata, function(s) max(s$S * sqrt(1 + (m - 1) * s$rho)), numeric(1))
  }
  x <- n * w / sum(w)
  b <- pmin(pmax(floor(x), minimo), N_h)
  while (sum(b) < n && any(b < N_h)) {
    cand <- which(b < N_h)
    i <- cand[which.max((x - b)[cand])]
    b[i] <- b[i] + 1
  }
  while (sum(b) > n && any(b > minimo)) {
    cand <- which(b > minimo)
    i <- cand[which.min((x - b)[cand])]
    b[i] <- b[i] - 1
  }
  stats::setNames(as.integer(b), names(strata))
}

#' Smallest number of recintos that reaches a half-width E for every target.
#'
#' Binary search on the total number of recintos; for each total the recintos are
#' allocated, the certainty recintos recomputed, and the variance evaluated.
size_design <- function(strata, E, conf, m, asignacion, minimo = 4L) {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  N_tot <- sum(vapply(strata, function(s) length(s$J), integer(1)))
  ok <- function(n) {
    n_h <- allocate_recintos(strata, n, m, asignacion, minimo)
    ev <- evaluate_design(strata, n_h, m, z)
    list(ok = max(ev$semiamplitud) <= E / 100, n_h = ev$recintos, ev = ev)
  }
  lo <- minimo * length(strata)
  hi <- N_tot
  if (!ok(hi)$ok) return(NULL)
  while (lo < hi) {
    mid <- (lo + hi) %/% 2
    if (ok(mid)$ok) hi <- mid else lo <- mid + 1
  }
  ok(lo)
}

# ---- grid -----------------------------------------------------------------------------------

#' Scenarios of the sample size grid.
sample_size_scenarios <- function(E = c(1, 1.5, 2), conf = c(0.95, 0.99), m = 1:4,
                                  asignacion = c("neyman", "proporcional"),
                                  objetivo = c("ambos", "cuotas", "margen"),
                                  version = c("punto", "superior")) {
  tidyr::expand_grid(E = E, conf = conf, m = as.integer(m), asignacion = asignacion,
                     objetivo = objetivo, version = version)
}

fmt_n_h <- function(n_h) paste(sprintf("%s %d", names(n_h), n_h), collapse = "; ")

#' Integrated sample for one scenario: Prefecto sized on the province, Alcalde on
#' Quito; in each Quito stratum the larger requirement is taken, and both
#' dignidades are evaluated on the final allocation.
integrated_size <- function(params, frame, dig_prov, dig_quito, E, conf, m, asignacion, objetivo, version,
                            minimo = 4L, reserva = 0.10) {
  sp <- design_strata(params, frame, dig_prov, objetivo, version)
  sq <- design_strata(params, frame, dig_quito, objetivo, version)
  rp <- size_design(sp, E, conf, m, asignacion, minimo)
  rq <- size_design(sq, E, conf, m, asignacion, minimo)
  if (is.null(rp) || is.null(rq)) { # target not reachable even with every recinto
    return(tibble::tibble(factible = FALSE))
  }
  n_h <- rp$n_h
  for (h in names(rq$n_h)) n_h[[h]] <- max(n_h[[h]], rq$n_h[[h]])
  z <- stats::qnorm(1 - (1 - conf) / 2)
  ev_p <- evaluate_design(sp, n_h, m, z)
  n_h <- ev_p$recintos
  ev_q <- evaluate_design(sq, n_h[names(sq)], m, z)
  N_h <- vapply(sp, function(s) length(s$J), integer(1))
  n_res <- pmin(ceiling(n_h * (1 + reserva)), N_h) - n_h
  tibble::tibble(
    factible = TRUE,
    recintos_prefecto_solo = sum(rp$n_h), recintos_alcalde_solo = sum(rq$n_h),
    recintos = sum(n_h), recintos_certeza = sum(ev_p$certeza), recintos_pps = sum(n_h) - sum(ev_p$certeza),
    actas = m * sum(n_h), recintos_reserva = sum(n_res),
    semiamplitud_prefecto = 100 * max(ev_p$semiamplitud),
    semiamplitud_alcalde = 100 * max(ev_q$semiamplitud),
    cuantitativo_limitante_prefecto = names(ev_p$semiamplitud)[which.max(ev_p$semiamplitud)],
    cuantitativo_limitante_alcalde = names(ev_q$semiamplitud)[which.max(ev_q$semiamplitud)],
    asignacion_recintos = fmt_n_h(n_h),
    n_h = list(n_h), reserva_h = list(n_res)
  )
}

#' The full sample size grid.
sample_size_grid <- function(params, frame, scenarios, dig_prov = "Prefecto 2023", dig_quito = "Alcalde Quito 2023",
                             minimo = 4L, reserva = 0.10, actas_max = 3L) {
  res <- purrr::pmap_dfr(scenarios, function(E, conf, m, asignacion, objetivo, version) {
    dplyr::bind_cols(
      tibble::tibble(E = E, conf = conf, m = m, asignacion = asignacion, objetivo = objetivo, version = version),
      integrated_size(params, frame, dig_prov, dig_quito, E, conf, m, asignacion, objetivo, version, minimo, reserva)
    )
  })
  dplyr::mutate(res, dentro_limite_actas = m <= actas_max)
}

#' Scenarios to present to the client.
client_scenarios <- function(grid) {
  # 2 juntas per recinto: one observer records both dignidades in Quito (4 actas) and
  # Prefecto elsewhere (2 actas); 3 juntas per recinto (6 actas in Quito) was ruled out
  # by the owner on 20 Oct 2026, and so was m = 1 (45% more recintos, misses +/-1 pp on the
  # margin). The m = 1 and m = 3 rows stay in the grid for reference.
  keys <- tibble::tribble(
    ~escenario, ~E, ~conf, ~m, ~asignacion, ~objetivo, ~version,
    "B. +/-1 pp, 99%, 2 actas por recinto", 1, 0.99, 2L, "neyman", "ambos", "punto",
    "C. +/-1 pp, 95%, 2 actas por recinto", 1, 0.95, 2L, "neyman", "ambos", "punto",
    "D. +/-1.5 pp, 99%, 2 actas por recinto", 1.5, 0.99, 2L, "neyman", "ambos", "punto",
    "B+. como B, con S y rho en su cota superior", 1, 0.99, 2L, "neyman", "ambos", "superior"
  )
  dplyr::inner_join(keys, grid, by = c("E", "conf", "m", "asignacion", "objetivo", "version"))
}
