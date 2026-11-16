## Everything in this file and any files in the R directory are sourced during `simInit()`.
## The functions in R/ take plain tables only; the module file reads the ledgers, summarises, saves and draws.
defineModule(sim, list(
  name = "fireSense_yearEffects",
  description = paste("Summarises the fitted per-year random effects (year effects) of fireSense ignition and escape fits across ELFs:",
                      "their size, whether ELFs share them (regional weather or lightning) or not (local data error),",
                      "ignition versus escape, and drift over time (post hoc only, never a model input)."),
  keywords = c("fire", "fireSense", "year effects", "random effects"),
  authors = c(person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(fireSense_yearEffects = "0.0.0.9001"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list("README.md"),
  reqdPkgs = list("ggplot2", "PredictiveEcology/fireSenseUtils@development (>= 0.2.3.9173)", "PredictiveEcology/SpaDES.core@development"),
  parameters = bindrows(
    defineParameter("ledgerFiles", "character", NA_character_, NA, NA,
                    "Paths of the national ignition ledgers, fireSenseIgnitionParams_*.rds (one row per ELF, with the fitted ignition and escape models)."),
    defineParameter(".plots", "character", "screen", NA, NA, "Where the figures go, as for SpaDES.core::Plots(): 'screen', 'png', both, or NA for none."),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "When the figures are drawn on screen; NA for never.")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows(
    createsOutput("yearEffectsTable", "data.frame",
                  "One row per ledger, ELF, process (ignition, escape) and year: observed, expected, priorSD, multiplier, shape. Also saved to <outputPath>/yearEffectsTable.csv."),
    createsOutput("yearEffectsSummary", "list",
                  "spread (per ELF and process), betweenELF, ignitionEscape and drift tables of summariseYearEffects(); each also saved to <outputPath>/yearEffects_<name>.csv.")
  )
))

doEvent.fireSense_yearEffects <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      sim$yearEffectsTable <- fireSenseUtils::readYearEffects(P(sim)$ledgerFiles)
      sim$yearEffectsSummary <- summariseYearEffects(sim$yearEffectsTable)
      out <- outputPath(sim); dir.create(out, recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(sim$yearEffectsTable, file.path(out, "yearEffectsTable.csv"), row.names = FALSE)
      for (n in names(sim$yearEffectsSummary))
        utils::write.csv(sim$yearEffectsSummary[[n]], file.path(out, paste0("yearEffects_", n, ".csv")), row.names = FALSE)
      Plots(data = sim$yearEffectsTable, fn = plotMultiplierByYear, filename = "multiplierByYear", ggsaveArgs = yearEffectsFigSize)
      Plots(data = sim$yearEffectsSummary$betweenELF, fn = plotBetweenELFCorrelation, filename = "betweenELFCorrelation",
            ggsaveArgs = yearEffectsFigSize)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}
