# fireSense_yearEffects (development version)

* New module: reads the fitted year effects of fireSense ignition and escape ledgers and summarises their size, their correlation between ELFs, ignition versus escape, and drift over time, as tables (csv) and two figures.

* The ledger reader is `fireSenseUtils::readYearEffects()` (moved to fireSenseUtils so `fireSense_report` can use it too); the module keeps the summaries and figures. Needs the fireSenseUtils version that has it (floor to be set after it merges).
