# fireSense_yearEffects

SpaDES module that summarises the fitted per-year random effects (year effects) of fireSense ignition and escape fits across ELFs.
They are post hoc diagnostics, never a model input.

Set `ledgerFiles` to national ignition ledgers (`fireSenseIgnitionParams_*.rds`). In `init` the module reads the
`yearEffects` of every ELF's ignition and escape fit (ELFs of older fits without them are skipped with a message) into
`sim$yearEffectsTable`, and computes `sim$yearEffectsSummary`, a list of four tables (`R/summariseYearEffects.R`):

- `spread`: `priorSD` and the 2.5, 50 and 97.5% quantiles of the multiplier, per ledger, ELF and process.
- `betweenELF`: Spearman correlation of log(multiplier) by year between ELFs, per ledger and process (shared: regional weather or lightning; local: data error).
- `ignitionEscape`: Spearman correlation of the ignition and escape log(multiplier), per ledger and ELF.
- `drift`: Spearman correlation of log(multiplier) with year (post hoc), per ledger, ELF and process.

The tables are written as csv to `outputPath`. Figures (`R/plotYearEffects.R`: multiplier by year, between-ELF correlation heatmap) go through `SpaDES.core::Plots()`; `.plots` decides where.
