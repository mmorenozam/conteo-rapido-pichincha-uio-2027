# Phase 4: design parameters from the 2023 juntas.
#
# For each dignidad, stratum and candidate (plus the margin between the first two):
#   pct  share of valid votes, R = Y / X (in %).
#   S    spread between juntas of the linearized residual of the ratio,
#        e_j = y_j - R x_j, relative to the mean valid votes per junta:
#        S = sd(e_j) / mean(x_j). Under simple random sampling of n juntas,
#        Var(R_hat) ~ (1 - f) S^2 / n, so S is on the scale of the share (0.07 = 7 pp).
#   rho  intraclass correlation of e_j within recinto (one-way ANOVA estimator for
#        unequal cluster sizes). With m juntas per recinto, deff = 1 + (m - 1) rho.
# Uncertainty: bootstrap of recintos within each stratum (percentile intervals).
# Sensitivity: the same estimates without the actas flagged as inconsistent.

MARGEN <- "MARGEN 1-2" # pseudo-candidate: votes of the first minus votes of the second

# ---- data -------------------------------------------------------------------------------

#' Junta x candidate vote matrix with the junta table (stratum, recinto, valid votes).
#'
#' Candidates are ordered by their total votes in the dignidad. A last column holds
#' the margin between the first two candidates.
#' @param cl Output of [clean_juntas()].
junta_matrix <- function(cl, parroquias) {
  hdr <- assign_strata(cl$juntas, parroquias_frame(parroquias)) |> dplyr::arrange(junta_id)
  w <- cl$votos |>
    dplyr::select(junta_id, candidato, votos) |>
    tidyr::pivot_wider(names_from = candidato, values_from = votos, values_fill = 0L) |>
    dplyr::arrange(junta_id)
  if (!identical(w$junta_id, hdr$junta_id)) cli::cli_abort("Vote matrix and junta table do not line up.")
  Y <- as.matrix(w[, -1])
  storage.mode(Y) <- "double"
  orden <- order(colSums(Y), decreasing = TRUE)
  Y <- Y[, orden, drop = FALSE]
  Y <- cbind(Y, Y[, 1] - Y[, 2])
  colnames(Y)[ncol(Y)] <- MARGEN
  if (any(abs(rowSums(Y[, -ncol(Y), drop = FALSE]) - hdr$votos_validos) > 0)) {
    cli::cli_abort("Valid votes per junta differ from the sum of candidate votes.")
  }
  list(juntas = hdr, Y = Y, x = hdr$votos_validos)
}

# ---- core estimators --------------------------------------------------------------------

#' Ratio, S and rho for every column of a vote matrix.
#'
#' @param Y Juntas x candidates matrix of votes.
#' @param x Valid votes per junta.
#' @param g Recinto of each junta.
#' @return List of named vectors R, S and rho (one value per column of Y).
param_core <- function(Y, x, g) {
  R <- colSums(Y) / sum(x)
  E <- Y - outer(x, R)
  N <- nrow(E)
  S <- sqrt(colSums(sweep(E, 2, colMeans(E))^2) / (N - 1)) / mean(x)
  gi <- as.integer(factor(g))
  k <- max(gi)
  m <- tabulate(gi, k)
  sums <- rowsum(E, gi, reorder = TRUE) # k x K, groups in order 1..k
  mu <- colMeans(E)
  ssb <- colSums(m * sweep(sums / m, 2, mu)^2)
  ssw <- colSums(E^2) - colSums(sums^2 / m)
  msb <- ssb / (k - 1)
  msw <- ssw / (N - k)
  m0 <- (N - sum(m^2) / N) / (k - 1)
  rho <- (msb - msw) / (msb + (m0 - 1) * msw)
  list(R = R, S = S, rho = rho)
}

#' Design effect of taking m juntas per recinto.
deff_cluster <- function(rho, m) 1 + (m - 1) * rho

#' Bootstrap of recintos: percentile intervals for S and rho.
#'
#' Recintos are drawn with replacement; a recinto drawn twice counts as two
#' recintos (new cluster id), as the bootstrap of clusters requires.
boot_params <- function(Y, x, g, B, seed, probs = c(0.025, 0.975)) {
  set.seed(seed)
  rows_by_rec <- split(seq_along(g), g)
  k <- length(rows_by_rec)
  S_b <- rho_b <- matrix(NA_real_, B, ncol(Y), dimnames = list(NULL, colnames(Y)))
  for (b in seq_len(B)) {
    pick <- rows_by_rec[sample.int(k, k, replace = TRUE)]
    rows <- unlist(pick, use.names = FALSE)
    gb <- rep.int(seq_len(k), lengths(pick))
    p <- param_core(Y[rows, , drop = FALSE], x[rows], gb)
    S_b[b, ] <- p$S
    rho_b[b, ] <- p$rho
  }
  q <- function(M) apply(M, 2, stats::quantile, probs = probs, na.rm = TRUE)
  qs <- q(S_b)
  qr <- q(rho_b)
  tibble::tibble(candidato = colnames(Y), S_inf = unname(qs[1, ]), S_sup = unname(qs[2, ]),
                 rho_inf = unname(qr[1, ]), rho_sup = unname(qr[2, ]))
}

# ---- by stratum ---------------------------------------------------------------------------

#' S and rho by stratum and candidate, with bootstrap intervals for the leading candidates.
#'
#' @param jm Output of [junta_matrix()].
#' @param dignidad Label for the output.
#' @param n_boot_cand Number of leading candidates (plus the margin) with bootstrap intervals.
#' @param B Bootstrap replicates.
#' @param seed Seed for the bootstrap (one per stratum: seed + stratum position).
#' @param filtro Optional logical vector over juntas (e.g. to drop inconsistent actas).
#' @return Tibble, one row per stratum x candidate; stratum "Total" is the province
#'   (or canton) without stratification, for reference.
estimate_parameters <- function(jm, dignidad, n_boot_cand = 3L, B = 500L, seed = 20270214L, filtro = NULL) {
  hdr <- jm$juntas
  Y <- jm$Y
  x <- jm$x
  if (!is.null(filtro)) {
    hdr <- hdr[filtro, ]
    Y <- Y[filtro, , drop = FALSE]
    x <- x[filtro]
  }
  grupos <- c(split(seq_len(nrow(hdr)), as.character(hdr$estrato)), list(Total = seq_len(nrow(hdr))))
  grupos <- grupos[intersect(c(ESTRATOS, "Total"), names(grupos))]
  boot_cols <- c(colnames(Y)[seq_len(min(n_boot_cand, ncol(Y) - 1))], MARGEN)

  purrr::imap_dfr(grupos, function(idx, estrato) {
    g <- hdr$recinto_codigo[idx]
    p <- param_core(Y[idx, , drop = FALSE], x[idx], g)
    out <- tibble::tibble(
      dignidad = dignidad, estrato = estrato, candidato = colnames(Y),
      orden = seq_len(ncol(Y)),
      # unname(): the vectors carry candidate names, which would stay on the columns
      pct = 100 * unname(p$R), S = unname(p$S), rho = unname(p$rho),
      deff_m2 = deff_cluster(unname(p$rho), 2), deff_m3 = deff_cluster(unname(p$rho), 3),
      juntas = length(idx), recintos = dplyr::n_distinct(g),
      juntas_por_recinto = length(idx) / dplyr::n_distinct(g),
      votos_validos_por_junta = mean(x[idx])
    )
    if (B > 0) {
      bt <- boot_params(Y[idx, boot_cols, drop = FALSE], x[idx], g, B = B,
                        seed = seed + match(estrato, c(ESTRATOS, "Total")))
      out <- dplyr::left_join(out, bt, by = "candidato")
    }
    out
  })
}

#' Compare the estimates with and without the inconsistent actas.
sensitivity_inconsistent <- function(todas, sin_inconsistentes) {
  dplyr::inner_join(
    dplyr::select(todas, dignidad, estrato, candidato, pct, S, rho),
    dplyr::select(sin_inconsistentes, dignidad, estrato, candidato, pct_sin = pct, S_sin = S, rho_sin = rho, juntas_sin = juntas),
    by = c("dignidad", "estrato", "candidato")
  ) |>
    dplyr::mutate(dif_pct = pct_sin - pct, dif_S_rel = S_sin / S - 1, dif_rho = rho_sin - rho)
}

#' Inputs for the sample size (phase 5), one row per dignidad x stratum.
#'
#' Conservative choice: the largest S among the leading candidates and the margin,
#' with the largest rho among them. Both are reported with the candidate they come from.
design_inputs <- function(params, n_cand = 3L) {
  if (!"S_sup" %in% names(params)) params <- dplyr::mutate(params, S_sup = NA_real_, rho_sup = NA_real_)
  params |>
    dplyr::filter(estrato != "Total", orden <= n_cand | candidato == MARGEN) |>
    dplyr::summarise(
      S_max = max(S), candidato_S_max = candidato[which.max(S)],
      rho_max = max(rho), candidato_rho_max = candidato[which.max(rho)],
      S_max_sup = max(S_sup), rho_max_sup = max(rho_sup),
      juntas = dplyr::first(juntas), recintos = dplyr::first(recintos),
      .by = c(dignidad, estrato)
    )
}
