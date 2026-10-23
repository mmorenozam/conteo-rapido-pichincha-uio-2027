# Phase 8: the lists of drawn juntas (owner, 23 Oct 2026).
#
# The designs of phase 7b give the number of juntas per stratum; this phase draws the
# actual juntas from the 2026 frame (the CNE distributivo with cut-off 11 Aug 2026),
# with the same systematic draw used in the simulations (draw_juntas(), cap of 4 juntas
# per recinto), and writes one Excel workbook per option for the client.
#
# - The lists use the sizes with the buffer for losses (`con_colchon`), so they are the
#   juntas the delegates must report; there is no separate list of substitutes.
# - Option A also marks each junta with its wave (1 or 2): wave 1 is a systematic
#   subsample of the drawn juntas within each (sub-)stratum, with the share of the
#   +/-1.5 pp design, as in wave_check().
# - The seed is fixed (`semilla_sorteo`), so the lists can be reproduced exactly; the
#   client may ask for a new draw with a seed generated under its own conditions.
# - The workbooks are confidential: they are written to reportes/sorteo/, which is in
#   .gitignore so that the lists never reach the repository.

#' One row per junta of the sampling frame, in the sort order of the draw.
#'
#' The order is the one used in the simulations ([junta_population()]): stratum,
#' canton, parroquia, zona, recinto, sex and junta number, so that the systematic draw
#' spreads the sample over the geography of each stratum.
#' @param frame The sampling frame (`marco_2027`): one row per recinto, with the junta
#'   ranges of the distributivo.
frame_juntas <- function(frame) {
  rec <- dplyr::select(frame, estrato, canton, canton_nombre, parroquia_codigo, parroquia,
                       zona_codigo, zona, recinto_codigo, recinto)
  j <- expand_junta_ranges(frame) |>
    dplyr::select(recinto_codigo, sexo, junta_num) |>
    dplyr::left_join(rec, by = "recinto_codigo") |>
    dplyr::arrange(match(as.character(estrato), ESTRATOS), canton, parroquia_codigo, zona_codigo,
                   recinto_codigo, sexo, junta_num)
  if (nrow(j) != sum(frame$juntas)) {
    cli::cli_abort(c("The junta ranges of the frame give {nrow(j)} juntas, the frame declares {sum(frame$juntas)}.",
                     "i" = "See target {.code rangos_invalidos_2027}."))
  }
  if (anyDuplicated(j[c("recinto_codigo", "sexo", "junta_num")])) {
    cli::cli_abort("Duplicated junta (recinto, sex, number) in the frame.")
  }
  dplyr::mutate(j, estrato = as.character(estrato), quito = canton == CANTON_QUITO)
}

#' The draw population of [draw_juntas()] for the frame juntas.
draw_population <- function(juntas) {
  est <- factor(juntas$estrato, levels = intersect(ESTRATOS, unique(juntas$estrato)))
  list(filas_estrato = split(seq_len(nrow(juntas)), est), recinto = juntas$recinto_codigo)
}

#' Draw the juntas of one design from the frame.
#'
#' @param juntas Output of [frame_juntas()].
#' @param n_h Juntas to draw per stratum (named by stratum).
#' @param seed Seed of the draw.
#' @param f_h Optional share of wave 1 per stratum (named); adds column `ola`.
#' @return The drawn juntas in list order, with `subestrato` (the stratum, or
#'   "<stratum>|R<recinto>" for a recinto set apart by the cap), `peso` (juntas of the
#'   sub-stratum each drawn junta represents) and, with `f_h`, `ola`.
draw_list <- function(juntas, n_h, seed, max_recinto = 4L, f_h = NULL) {
  pop <- draw_population(juntas)
  set.seed(seed)
  s <- draw_juntas(pop, n_h, max_recinto)
  out <- juntas[s$fila, ] |>
    dplyr::mutate(fila = s$fila, subestrato = s$estrato, peso = s$w, juntas_subestrato = s$N)
  if (!is.null(f_h)) {
    w1 <- subsample_wave(s, f_h)
    out$ola <- ifelse(s$fila %in% w1$fila, 1L, 2L)
  }
  out |>
    dplyr::arrange(fila) |>
    dplyr::mutate(numero = dplyr::row_number(), .before = 1) |>
    dplyr::select(-fila)
}

#' The four lists of the report: options A and B at 99% and 95%, with the buffer.
#'
#' @param disenos Named list (A_99, A_95, B_99, B_95) of designs (`$n_h`, without buffer).
#' @param olas Named list (A_99, A_95) of the wave-1 designs (`$n_h`).
#' @return Named list of tibbles.
draw_lists <- function(juntas, disenos, olas, perdidas, seed, max_recinto = 4L) {
  N <- lengths(draw_population(juntas)$filas_estrato)
  purrr::imap(disenos, function(d, nombre) {
    n_h <- buffer_allocation(d$n_h, perdidas$junta, perdidas$recinto, N = N)
    f_h <- if (!is.null(olas[[nombre]])) olas[[nombre]]$n_h[names(d$n_h)] / d$n_h
    draw_list(juntas, n_h, seed, max_recinto, f_h)
  })
}

#' Juntas and actas per stratum of each list, and the largest number of juntas of one recinto.
summarise_lists <- function(listas, juntas) {
  marco <- dplyr::count(juntas, estrato, name = "juntas_marco")
  purrr::imap_dfr(listas, function(l, nombre) {
    r <- l |>
      dplyr::summarise(juntas = dplyr::n(),
                       ola_1 = if ("ola" %in% names(l)) sum(ola == 1L) else NA_integer_,
                       actas_prefectura = dplyr::n(), actas_alcaldia = sum(quito),
                       max_por_recinto = max(table(recinto_codigo)), .by = estrato)
    dplyr::left_join(marco, r, by = "estrato") |>
      dplyr::mutate(lista = nombre, opcion = substr(nombre, 1, 1),
                    conf = as.numeric(substr(nombre, 3, 4)) / 100, .before = 1)
  }) |>
    dplyr::mutate(estrato = factor(estrato, levels = ESTRATOS)) |>
    dplyr::arrange(opcion, dplyr::desc(conf), estrato) |>
    dplyr::mutate(estrato = as.character(estrato))
}

#' Write one workbook per option: a read-me sheet, a summary and one list per confidence level.
#'
#' @param dir Output folder (created if missing).
#' @return The paths of the workbooks (for a `format = "file"` target).
write_list_workbooks <- function(listas, resumen, dir, seed, perdidas, fecha_corte = "11 de agosto de 2026") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg openxlsx} is needed: {.code renv::install(\"openxlsx\")} and {.code renv::snapshot()}.")
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  opciones <- c(A = "Opción A: cuotas y diferencia entre el 1ro y el 2do", B = "Opción B: solo cuotas")
  cab <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DDE3EA", border = "bottom", wrapText = TRUE)
  fmt_conf <- function(conf) paste0(round(100 * conf), " %")

  hoja_lista <- function(l) {
    t <- tibble::tibble(
      `N.º` = l$numero, Estrato = l$estrato, `Cantón` = l$canton_nombre, Parroquia = l$parroquia,
      Zona = l$zona, `Código recinto` = l$recinto_codigo, Recinto = l$recinto,
      Sexo = ifelse(l$sexo == "F", "Femenina", "Masculina"), Junta = l$junta_num,
      Actas = ifelse(l$quito, "Prefectura y Alcaldía", "Prefectura"))
    if ("ola" %in% names(l)) t$Ola <- l$ola
    t$Subestrato <- l$subestrato
    t$Peso <- round(l$peso, 4)
    t
  }

  paths <- character(0)
  for (op in sort(unique(substr(names(listas), 1, 1)))) {
    wb <- openxlsx::createWorkbook()
    nombres <- names(listas)[startsWith(names(listas), op)]
    confs <- as.numeric(substr(nombres, 3, 4)) / 100

    # read-me
    lee <- c(
      paste0("Conteo rápido por actas, elección seccional del 29 de noviembre de 2026: ", opciones[[op]], "."),
      "",
      "Contenido:",
      "- Resumen: juntas y actas por estrato de cada lista.",
      paste0("- ", paste(paste0("Juntas ", fmt_conf(confs)), collapse = " y "),
             ": las juntas sorteadas para cada nivel de confianza. Se usa una sola de las dos listas, según el nivel de confianza que elija la organización."),
      "",
      "Cómo se sortearon:",
      paste0("- Marco: distributivo de recintos del CNE con corte del ", fecha_corte, ", sin las juntas especiales."),
      "- En cada estrato, sorteo sistemático con arranque aleatorio sobre la lista de juntas ordenada por cantón, parroquia, zona, recinto, sexo y número de junta.",
      "- Ningún recinto aporta más de 4 juntas. Los recintos que tendrían más se sortean aparte, con 4 juntas cada uno (columna Subestrato).",
      paste0("- Las listas ya incluyen la reserva para pérdidas (entre ", round(100 * min(perdidas$junta)), " % y ",
             round(100 * max(perdidas$junta)), " % de las juntas según el estrato, y un ", round(100 * perdidas$recinto),
             " % de recintos). No hay juntas de reemplazo: todas las juntas de la lista deben reportar."),
      paste0("- Semilla del sorteo: ", seed, ". Con la misma semilla y el mismo distributivo se obtiene exactamente la misma lista. La organización puede pedir un sorteo nuevo con una semilla generada bajo sus propias condiciones."),
      if (op == "A") "- Columna Ola: la ola 1 es una submuestra de la lista, repartida por toda la geografía de cada estrato. El primer resultado (±1,5 pp) se publica cuando la ola 1 ha llegado en todos los estratos; el final (±1 pp), cuando han llegado las olas 1 y 2.",
      "- Columna Peso: número de juntas del marco que representa cada junta sorteada (se usa en la estimación). Se recalcula la noche de la elección con las juntas que reporten.",
      "",
      "Confidencial: la lista debe mantenerse reservada hasta poco antes de la elección."
    )
    openxlsx::addWorksheet(wb, "Léame")
    openxlsx::writeData(wb, "Léame", tibble::tibble(x = lee), colNames = FALSE)
    openxlsx::setColWidths(wb, "Léame", 1, 140)

    # summary
    r <- resumen[resumen$opcion == op, ]
    rt <- tibble::tibble(Confianza = fmt_conf(r$conf), Estrato = r$estrato, `Juntas en el marco` = r$juntas_marco,
                         `Juntas sorteadas` = r$juntas)
    if (op == "A") rt$`Juntas de la ola 1` <- r$ola_1
    rt$`Actas de Prefectura` <- r$actas_prefectura
    rt$`Actas de Alcaldía` <- r$actas_alcaldia
    rt$`Máximo de juntas por recinto` <- r$max_por_recinto
    tot <- rt |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(v) if (dplyr::cur_column() == "Máximo de juntas por recinto") max(v) else sum(v)),
                       .by = Confianza) |>
      dplyr::mutate(Estrato = "Total")
    rt <- dplyr::bind_rows(rt, tot) |>
      dplyr::arrange(match(Confianza, fmt_conf(sort(unique(r$conf), decreasing = TRUE))), Estrato == "Total")
    openxlsx::addWorksheet(wb, "Resumen")
    openxlsx::writeData(wb, "Resumen", rt, headerStyle = cab)
    openxlsx::setColWidths(wb, "Resumen", seq_along(rt), c(11, 26, rep(14, ncol(rt) - 2)))
    openxlsx::addStyle(wb, "Resumen", openxlsx::createStyle(textDecoration = "bold"),
                       rows = which(rt$Estrato == "Total") + 1, cols = seq_along(rt), gridExpand = TRUE, stack = TRUE)

    # one list per confidence level
    for (k in seq_along(nombres)) {
      hoja <- paste0("Juntas ", fmt_conf(confs[k]))
      t <- hoja_lista(listas[[nombres[k]]])
      openxlsx::addWorksheet(wb, hoja)
      openxlsx::writeDataTable(wb, hoja, t, tableStyle = "TableStyleLight9", withFilter = TRUE)
      openxlsx::freezePane(wb, hoja, firstRow = TRUE)
      openxlsx::setColWidths(wb, hoja, seq_along(t), "auto")
    }

    p <- file.path(dir, paste0("conteo_rapido_opcion_", op, "_juntas.xlsx"))
    openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
    paths <- c(paths, p)
  }
  paths
}
