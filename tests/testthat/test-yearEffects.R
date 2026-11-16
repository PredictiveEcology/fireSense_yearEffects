test_that("summaries: shared effect gives Spearman 1, priorSD 0 escape is reported with multiplier 1", {
  d <- toyDir(); f <- toyLedger(d)
  tab <- suppressMessages(fireSenseUtils::readYearEffects(f)); s <- summariseYearEffects(tab)
  expect_named(s, c("spread", "betweenELF", "ignitionEscape", "drift"))
  b <- s$betweenELF; bi <- b[b$process == "ignition", ]
  expect_equal(bi$rho, 1); expect_equal(bi$n, length(toyYears))
  sp <- s$spread; e0 <- sp[sp$ELF == "1.1" & sp$process == "escape", ]
  expect_equal(e0$priorSD, 0); expect_equal(c(e0$q025, e0$q50, e0$q975), c(1, 1, 1))
  i1 <- sp[sp$ELF == "1.1" & sp$process == "ignition", ]
  expect_equal(i1$q50, stats::median(c(0.6, 1.5, 0.9, 2, 1.1, 0.7, 1.3, 0.8)))
  ie <- s$ignitionEscape
  expect_true(is.na(ie$rho[ie$ELF == "1.1"]))                    # constant escape multiplier: no rank correlation
  expect_equal(ie$rho[ie$ELF == "1.2"], stats::cor(rank(c(0.6, 1.5, 0.9, 2, 1.1, 0.7, 1.3, 0.8)), rank(rev(c(0.6, 1.5, 0.9, 2, 1.1, 0.7, 1.3, 0.8)))))
  dr <- s$drift
  expect_equal(nrow(dr), 4)
  expect_equal(dr$rho[dr$ELF == "1.1" & dr$process == "ignition"],
               stats::cor(toyYears, c(0.6, 1.5, 0.9, 2, 1.1, 0.7, 1.3, 0.8), method = "spearman"))
})

test_that("the module runs and writes the csvs", {
  d <- toyDir(); f <- toyLedger(d)
  out <- file.path(d, "out")
  sim <- suppressMessages(SpaDES.core::simInitAndSpades(
    times = list(start = 0, end = 0), params = list(fireSense_yearEffects = list(ledgerFiles = f, .plots = "png")),
    modules = "fireSense_yearEffects", paths = list(modulePath = normalizePath(testthat::test_path("..", "..", "..")),
                                                    outputPath = out, inputPath = d, cachePath = file.path(d, "cache"))))
  expect_s3_class(sim$yearEffectsTable, "data.frame")
  expect_named(sim$yearEffectsSummary, c("spread", "betweenELF", "ignitionEscape", "drift"))
  for (n in c("yearEffectsTable", paste0("yearEffects_", names(sim$yearEffectsSummary)))) expect_true(file.exists(file.path(out, paste0(n, ".csv"))), info = n)
  expect_true(length(list.files(out, pattern = "\\.png$", recursive = TRUE)) >= 2)
})
