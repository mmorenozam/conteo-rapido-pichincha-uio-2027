# Phase 6: Monte Carlo simulation of the full design on the 2023 juntas.
#
# The 2023 election is used as a known population: every junta's result is known,
# so drawing the sample many times and estimating each time shows the real error
# of the design, and whether the night-time margin of error covers it. Each replicate:
#   1. in each stratum, the certainty recintos (same rule as phases 3 and 5) and a
#      systematic PPS draw of the other recintos, on the list sorted by canton,
#      parroquia, zona and recinto (random start);
#   2. in each selected recinto, m juntas balanced by sex (one of each for m = 2;
#      for odd m the extra junta goes to a sex chosen at random); all juntas if the
#      recinto has m or fewer;
#   3. combined ratio estimator with design weights 1 / (pi_recinto * pi_junta);
#   4. variance by ultimate clusters: PPS recintos are the units of their stratum;
#      in a certainty recinto the juntas are the units (only their choice is random);
#      certainty recintos observed with a single junta are collapsed by stratum.
#      Two versions: with replacement ("uc", the usual night-time estimator) and with
#      finite population corrections ("uc_fpc": recintos on the PPS part, juntas inside
#      certainty recintos).
# The 2027 allocation of a scenario is applied to the 2023 recintos (same strata).

SIM_ESTIMADORES <- c("uc", "uc_fpc")

# ---- population ----------------------------------------------------------------------------

#' Simulation population: the 2023 juntas of the province, with the vote matrices
#' of both dignidades for the first `n_cand` candidates and the margin.
#'
#' @param jm_prov,jm_quito Outputs of [junta_matrix()] (Prefecto and Alcalde Quito).
sim_population <- function(jm_prov, jm_quito, n_cand = 3L) {
  cols <- function(Y) c(colnames(Y)[seq_len(n_cand)], MARGEN)
  hdr <- jm_prov$juntas
  rq <- match(hdr$junta_id, jm_quito$juntas$junta_id) # NA outside Quito
  rec <- hdr |>
    dplyr::mutate(fila = dplyr::row_number()) |>
    dplyr::summarise(estrato = dplyr::first(as.character(estrato)), canton = dplyr::first(canton),
                     parroquia_codigo = dplyr::first(parroquia_codigo), zona_codigo = dplyr::first(zona_codigo),
                     M = dplyr::n(), filas_f = list(fila[sexo == "F"]), filas_m = list(fila[sexo == "M"]),
                     .by = recinto_codigo) |>
    dplyr::arrange(match(estrato, ESTRATOS), canton, parroquia_codigo, zona_codigo, recinto_codigo)
  Yp <- jm_prov$Y[, cols(jm_prov$Y), drop = FALSE]
  Yq <- jm_quito$Y[, cols(jm_quito$Y), drop = FALSE]
  list(
    recintos = rec,
    estratos = intersect(ESTRATOS, unique(rec$estrato)),
    prov = list(Y = Yp, x = jm_prov$x, verdad = colSums(Yp) / sum(jm_prov$x)),
    quito = list(Y = Yq, x = jm_quito$x, fila = rq,
                 verdad = colSums(Yq) / sum(jm_quito$x))
  )
}

# ---- one sample ---------------------------------------------------------------------------------

#' m juntas of one recinto, balanced by sex. Returns rows and their inclusion probability.
pick_juntas <- function(filas_f, filas_m, m) {
  M <- length(filas_f) + length(filas_m)
  if (m >= M) return(list(filas = c(filas_f, filas_m), p = rep(1, M)))
  if (length(filas_f) && length(filas_m)) {
    k_f <- m %/% 2 + if (m %% 2 == 1) stats::rbinom(1, 1, 0.5) else 0L
    k_f <- min(k_f, length(filas_f))
    k_m <- min(m - k_f, length(filas_m))
    k_f <- m - k_m
    s <- function(v, k) v[sample.int(length(v), k)] # safe when length(v) == 1
    return(list(filas = c(s(filas_f, k_f), s(filas_m, k_m)),
                p = c(rep(k_f / length(filas_f), k_f), rep(k_m / length(filas_m), k_m))))
  }
  v <- c(filas_f, filas_m)
  list(filas = v[sample.int(M, m)], p = rep(m / M, m))
}

#' Draw one sample of the design.
#' @param n_h Named integer vector of recintos per stratum (certainty included).
#' @return Tibble, one row per sampled junta: fila (row in the population), w (weight),
#'   vstrat (variance stratum), unidad (ultimate cluster) and fpc of its variance stratum.
draw_sample <- function(pop, n_h, m) {
  out <- vector("list", length(pop$estratos))
  for (i in seq_along(pop$estratos)) {
    h <- pop$estratos[i]
    R <- pop$recintos[pop$recintos$estrato == h, ]
    M <- R$M
    n <- min(n_h[[h]], nrow(R))
    cert <- if (n >= nrow(R)) rep(TRUE, nrow(R)) else certainty_vector(M, n)
    n_p <- max(n - sum(cert), 0)
    sel <- which(cert)
    pi <- rep(1, length(sel))
    tipo <- rep("C", length(sel))
    if (n_p > 0) {
      ip <- which(!cert)
      Mp <- M[ip]
      I <- sum(Mp) / n_p
      puntos <- stats::runif(1, 0, I) + I * (seq_len(n_p) - 1)
      k <- findInterval(puntos, c(0, cumsum(Mp)), left.open = TRUE) # recinto hit by each point
      sel <- c(sel, ip[k])
      pi <- c(pi, n_p * Mp[k] / sum(Mp))
      tipo <- c(tipo, rep("P", n_p))
    }
    fpc_p <- if (n_p > 0) 1 - n_p / sum(!cert) else 1
    picks <- lapply(seq_along(sel), function(j) pick_juntas(R$filas_f[[sel[j]]], R$filas_m[[sel[j]]], m))
    k_j <- lengths(lapply(picks, `[[`, "filas"))
    filas <- unlist(lapply(picks, `[[`, "filas"), use.names = FALSE)
    rec_j <- rep(R$recinto_codigo[sel], k_j)
    tipo_j <- rep(tipo, k_j)
    out[[i]] <- tibble::tibble(
      fila = filas,
      w = 1 / (rep(pi, k_j) * unlist(lapply(picks, `[[`, "p"), use.names = FALSE)),
      recinto = rec_j, estrato = h, tipo = tipo_j,
      # certainty recintos with 2+ juntas are their own variance stratum; those with a
      # single junta are collapsed into one stratum per estrato (a stratum with one unit
      # gives no variance estimate; collapsing is the usual, slightly conservative fix)
      vstrat = ifelse(tipo_j == "P", paste0(h, "|PPS"),
                      ifelse(rep(k_j, k_j) >= 2, paste0(h, "|C|", rec_j), paste0(h, "|C1"))),
      unidad = ifelse(tipo_j == "P", as.character(rec_j), as.character(filas)),
      # PPS part: recinto fpc; certainty recinto: share of its juntas not observed
      fpc = ifelse(tipo_j == "P", fpc_p, 1 - rep(k_j / R$M[sel], k_j))
    )
  }
  dplyr::bind_rows(out)
}

# ---- estimation ---------------------------------------------------------------------------------

#' Combined ratio estimate and half-widths (two variance estimators) for one sample.
#' @param dom Population domain (pop$prov or pop$quito).
#' @param filas Rows of the sample in that domain's matrices.
estimate_sample <- function(s, dom, filas, z) {
  Y <- dom$Y[filas, , drop = FALSE]
  x <- dom$x[filas]
  w <- s$w
  X_hat <- sum(w * x)
  R_hat <- colSums(w * Y) / X_hat
  E <- w * (Y - outer(x, R_hat)) / X_hat
  key <- paste(s$vstrat, s$unidad, sep = "#")
  U <- rowsum(E, key, reorder = FALSE) # ultimate cluster totals
  vst <- s$vstrat[match(rownames(U), key)]
  fpc <- s$fpc[match(rownames(U), key)]
  n <- as.vector(table(vst)[vst])
  ok <- n >= 2 # a variance stratum with one unit contributes no estimable variance
  Uc <- U - rowsum(U, vst)[vst, , drop = FALSE] / n # centred within variance stratum
  contrib <- (n / (n - 1)) * Uc^2
  contrib[!ok, ] <- 0
  V_uc <- colSums(contrib)
  V_fpc <- colSums(contrib * fpc)
  list(R = R_hat, semi_uc = z * sqrt(V_uc), semi_uc_fpc = z * sqrt(V_fpc))
}

# ---- replicates ---------------------------------------------------------------------------------

#' Simulate a design many times; returns one row per replicate x dignidad x quantity.
simulate_design <- function(pop, n_h, m, n_sim, seed, conf = 0.99, etiqueta = "") {
  z <- stats::qnorm(1 - (1 - conf) / 2)
  set.seed(seed)
  purrr::map_dfr(seq_len(n_sim), function(b) {
    s <- draw_sample(pop, n_h, m)
    ep <- estimate_sample(s, pop$prov, s$fila, z)
    sq <- s[!is.na(pop$quito$fila[s$fila]), ]
    eq <- estimate_sample(sq, pop$quito, pop$quito$fila[sq$fila], z)
    dplyr::bind_rows(
      tibble::tibble(dignidad = "Prefecto 2023", cantidad = colnames(pop$prov$Y),
                     error = unname(ep$R - pop$prov$verdad), semi_uc = unname(ep$semi_uc), semi_uc_fpc = unname(ep$semi_uc_fpc)),
      tibble::tibble(dignidad = "Alcalde Quito 2023", cantidad = colnames(pop$quito$Y),
                     error = unname(eq$R - pop$quito$verdad), semi_uc = unname(eq$semi_uc), semi_uc_fpc = unname(eq$semi_uc_fpc))
    ) |>
      dplyr::mutate(replica = b, recintos = dplyr::n_distinct(s$recinto), actas = nrow(s), .before = 1)
  }) |>
    dplyr::mutate(escenario = etiqueta, .before = 1)
}

#' Summary of a simulation, in percentage points.
#'
#' error_cuantil: quantile `conf` of the absolute error (the real half-width of the
#' design); semi_*: mean half-width reported by each variance estimator; cobertura_*:
#' share of replicates whose interval contains the true value.
summarise_simulation <- function(sim, conf = 0.99) {
  sim |>
    dplyr::summarise(
      replicas = dplyr::n(), recintos = stats::median(recintos), actas = stats::median(actas),
      sesgo = 100 * mean(error), rmse = 100 * sqrt(mean(error^2)),
      error_cuantil = 100 * stats::quantile(abs(error), conf, names = FALSE),
      # coverage first: the next lines replace semi_uc and semi_uc_fpc by their means
      cobertura_uc = mean(abs(error) <= semi_uc), cobertura_uc_fpc = mean(abs(error) <= semi_uc_fpc),
      semi_uc = 100 * mean(semi_uc), semi_uc_fpc = 100 * mean(semi_uc_fpc),
      .by = c(escenario, dignidad, cantidad)
    )
}

#' Simulate the client scenarios of phase 5.
simulate_scenarios <- function(pop, escenarios, n_sim, seed, conf = 0.99) {
  purrr::map_dfr(seq_len(nrow(escenarios)), function(i) {
    e <- escenarios[i, ]
    simulate_design(pop, e$n_h[[1]], e$m, n_sim, seed + i, conf = e$conf, etiqueta = e$escenario)
  })
}

#' Scale a scenario's allocation and simulate each size, to find the smallest sample
#' whose real error stays within the target.
simulate_scaling <- function(pop, n_h, m, factores, n_sim, seed, conf = 0.99, minimo = 4L) {
  purrr::map_dfr(seq_along(factores), function(i) {
    n_s <- pmax(minimo, round(n_h * factores[i]))
    names(n_s) <- names(n_h)
    simulate_design(pop, n_s, m, n_sim, seed + 100 + i, conf = conf,
                    etiqueta = sprintf("factor %.2f", factores[i])) |>
      dplyr::mutate(factor = factores[i])
  })
}

#' Smallest scale factor whose real error (quantile conf) is within E for every
#' quantity of both dignidades.
smallest_scale <- function(sum_scaling, E) {
  sum_scaling |>
    dplyr::mutate(factor = as.numeric(sub("factor ", "", escenario))) |>
    dplyr::summarise(peor_error = max(error_cuantil), recintos = dplyr::first(recintos),
                     actas = dplyr::first(actas), .by = factor) |>
    dplyr::arrange(factor) |>
    dplyr::mutate(cumple = peor_error <= E)
}
