## The functions are plain functions in R/; source them as simInit() would (reqdPkgs of the plots are attached by simInit()).
Sys.setenv(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1"); data.table::setDTthreads(1)
suppressMessages(library(ggplot2))
for (f in list.files(testthat::test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) source(f)

## A toy ledger in the national format: ELFs "1.1" and "1.2" share an ignition effect (1.2's is 1.1's squared, ranks identical),
## "1.3" has an unrelated one and no yearEffects (an older fit). Escape: 1.1 has priorSD 0 (multiplier 1).
toyYears <- 2001:2008
toyEffects <- function(mult, priorSD = 0.5) data.frame(year = toyYears, observed = 5, expected = 5, priorSD = priorSD,
                                                      multiplier = mult, shape = if (priorSD > 0) 10 else NA_real_)
toyLedger <- function(dir, name = "fireSenseIgnitionParams_toy") {
  shared <- c(0.6, 1.5, 0.9, 2, 1.1, 0.7, 1.3, 0.8)
  x <- data.frame(polygonID = c("1.1", "1.2", "1.3"))
  x$fireSense_IgnitionFitted <- list(list(modelList = list(yearEffects = toyEffects(shared))),
                                     list(modelList = list(yearEffects = toyEffects(shared^2))),
                                     list(modelList = list()))
  x$fireSense_EscapeFitted <- list(list(modelList = list(yearEffects = toyEffects(rep(1, 8), priorSD = 0))),
                                   list(modelList = list(yearEffects = toyEffects(rev(shared)))),
                                   list(modelList = list()))
  f <- file.path(dir, paste0(name, ".rds")); saveRDS(x, f); f
}

toyDir <- function() { d <- tempfile("yearEffects"); dir.create(d); d }
