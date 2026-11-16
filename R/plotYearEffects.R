## Figures of the year effects; plain tables in, ggplot objects out. The module draws them through SpaDES.core::Plots().

## sizes (inches, dpi) of the figures for Plots(ggsaveArgs = )
yearEffectsFigSize <- list(width = 14, height = 9, dpi = 110, units = "in", bg = "white")

#' Multiplier by year, one line per ELF, faceted by process (and ledger when there are several)
#' @param tab The table of `readYearEffects()`.
#' @return A ggplot.
plotMultiplierByYear <- function(tab) {
  ggplot2::ggplot(tab, ggplot2::aes(year, multiplier, group = ELF, colour = ELF)) +
    ggplot2::geom_hline(yintercept = 1, colour = "grey60") +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::scale_y_log10() +
    ggplot2::facet_grid(ledger ~ process) +
    ggplot2::labs(x = "year", y = "year-effect multiplier (log scale)", colour = "ELF") +
    ggplot2::theme_bw() + ggplot2::theme(strip.text.y = ggplot2::element_text(size = 5))
}

#' Between-ELF Spearman correlation of log(multiplier) as a heatmap, one panel per ledger and process
#' @param betweenELF The `betweenELF` table of `summariseYearEffects()`.
#' @return A ggplot.
plotBetweenELFCorrelation <- function(betweenELF) {
  sym <- rbind(betweenELF, transform(betweenELF, ELF1 = ELF2, ELF2 = betweenELF$ELF1))
  ggplot2::ggplot(sym, ggplot2::aes(ELF1, ELF2, fill = rho)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1), low = "#B2182B", mid = "white", high = "#2166AC", na.value = "grey85") +
    ggplot2::facet_grid(ledger ~ process) +
    ggplot2::labs(x = NULL, y = NULL, fill = "Spearman\nrho") +
    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), strip.text.y = ggplot2::element_text(size = 5))
}
