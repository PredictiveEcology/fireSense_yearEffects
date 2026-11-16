## Reads the fitted year effects of national ignition ledgers into one long table.
## The columns of a year-effects table are those of fireSense_ignitionFit's fittedYearEffects():
## year, observed, expected, priorSD, multiplier, shape.

yearEffectColumns <- c("year", "observed", "expected", "priorSD", "multiplier", "shape")

## The fitted model slots of a ledger row that hold the two processes
yearEffectProcesses <- stats::setNames(fireSenseUtils::ignitionFitAdditionalColNamesTxt, c("ignition", "escape"))

#' Read the year effects of ignition ledgers
#'
#' @param ledgerFiles Paths of ledgers `fireSenseIgnitionParams_*.rds`: a data.frame with one row per ELF
#'   (`polygonID`) and the list columns `fireSense_IgnitionFitted` and `fireSense_EscapeFitted`, each element
#'   holding `$modelList$yearEffects`.
#' @return A data.frame, one row per ledger, ELF, process (`ignition`, `escape`) and year: `ledger`
#'   (file name without `.rds`), `ELF`, `process` and the six columns of `fittedYearEffects()`. Ledger rows whose
#'   `yearEffects` is `NULL` (older fits) are skipped with a message.
readYearEffects <- function(ledgerFiles) {
  if (anyNA(ledgerFiles) || !length(ledgerFiles)) stop("ledgerFiles must name at least one ledger file")
  missingFiles <- ledgerFiles[!file.exists(ledgerFiles)]
  if (length(missingFiles)) stop("ledger file(s) not found: ", paste(missingFiles, collapse = ", "))
  pieces <- list()
  for (f in ledgerFiles) {
    x <- readRDS(f); ledger <- sub("\\.rds$", "", basename(f))
    if (anyDuplicated(x$polygonID)) stop(ledger, " has more than one row for ELF ", paste(unique(x$polygonID[duplicated(x$polygonID)]), collapse = ", "))
    for (i in seq_len(nrow(x))) for (p in names(yearEffectProcesses)) {
      ye <- x[[yearEffectProcesses[[p]]]][[i]]$modelList$yearEffects
      if (is.null(ye)) {
        message("no yearEffects for ", p, " of ELF ", x$polygonID[i], " in ", ledger, ": skipped")
        next
      }
      pieces[[length(pieces) + 1L]] <- data.frame(ledger = ledger, ELF = as.character(x$polygonID[i]), process = p,
                                                  ye[, yearEffectColumns, drop = FALSE], row.names = NULL)
    }
  }
  if (!length(pieces)) stop("none of the ledgers has any yearEffects")
  do.call(rbind, pieces)
}
