## Summaries of the year-effects table of readYearEffects(). Medians and 2.5-97.5% ranges, never means, as the central tendency.
## Spearman correlations are of log(multiplier); year effects are post hoc diagnostics, never a model input.

## Spearman correlation of two vectors on their pairwise complete years: c(rho, n). A constant series has no rank correlation (NA).
spearmanPair <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  rho <- if (sum(ok) >= 3L && stats::sd(x[ok]) > 0 && stats::sd(y[ok]) > 0) stats::cor(x[ok], y[ok], method = "spearman") else NA_real_
  c(rho = rho, n = sum(ok))
}

#' Summarise year effects
#'
#' @param tab The table of `readYearEffects()`.
#' @return A list of four data.frames:
#'   `spread`: per ledger, ELF and process, `priorSD` (0: no extra-Poisson spread, multiplier 1) and the 2.5, 50 and
#'   97.5% quantiles of `multiplier` (`q025`, `q50`, `q975`), with `nYears`;
#'   `betweenELF`: per ledger and process, the Spearman correlation `rho` of log(multiplier) by year between each pair
#'   of ELFs (`ELF1`, `ELF2`), on their `n` common years: shared (regional weather, lightning) or local (data error) effects;
#'   `ignitionEscape`: per ledger and ELF, the Spearman correlation of the ignition and the escape log(multiplier);
#'   `drift`: per ledger, ELF and process, the Spearman correlation of log(multiplier) with year (post hoc).
summariseYearEffects <- function(tab) {
  tab$logMultiplier <- log(tab$multiplier)
  byGroup <- function(by, fn) {
    key <- do.call(paste, c(tab[by], sep = "\r"))
    out <- lapply(split(tab, factor(key, levels = unique(key))), function(d) data.frame(d[1L, by, drop = FALSE], fn(d), row.names = NULL))
    do.call(rbind, unname(out))
  }
  spread <- byGroup(c("ledger", "ELF", "process"), function(d) {
    q <- stats::quantile(d$multiplier, c(0.025, 0.5, 0.975), na.rm = TRUE, names = FALSE)
    data.frame(priorSD = d$priorSD[1L], q025 = q[1L], q50 = q[2L], q975 = q[3L], nYears = nrow(d))
  })
  betweenELF <- byGroup(c("ledger", "process"), function(d) {
    w <- tapply(d$logMultiplier, list(d$year, d$ELF), identity)   # one value per year and ELF (readYearEffects() stops on duplicate ELFs)
    elfs <- colnames(w)
    if (length(elfs) < 2L) return(NULL)
    pr <- t(utils::combn(elfs, 2L))
    r <- t(vapply(seq_len(nrow(pr)), function(k) spearmanPair(w[, pr[k, 1L]], w[, pr[k, 2L]]), numeric(2)))
    data.frame(ELF1 = pr[, 1L], ELF2 = pr[, 2L], rho = r[, "rho"], n = as.integer(r[, "n"]))
  })
  ignitionEscape <- byGroup(c("ledger", "ELF"), function(d) {
    ign <- d[d$process == "ignition", ]; esc <- d[d$process == "escape", ]
    r <- spearmanPair(ign$logMultiplier[match(esc$year, ign$year)], esc$logMultiplier)
    data.frame(rho = r[["rho"]], n = as.integer(r[["n"]]))
  })
  drift <- byGroup(c("ledger", "ELF", "process"), function(d) {
    r <- spearmanPair(d$logMultiplier, d$year)
    data.frame(rho = r[["rho"]], n = as.integer(r[["n"]]))
  })
  list(spread = spread, betweenELF = betweenELF, ignitionEscape = ignitionEscape, drift = drift)
}
