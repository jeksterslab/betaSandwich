## ---- test-betaSandwich-diff-beta-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    if (!exists("nas1982")) {
      try(
        data(
          "nas1982",
          package = "betaSandwich"
        ),
        silent = TRUE
      )
    }
    df <- nas1982
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    mvn <- DiffBetaSandwich(BetaN(object))
    print.diffbetasandwich(mvn)
    summary.diffbetasandwich(mvn)
    coef.diffbetasandwich(mvn)
    vcov.diffbetasandwich(mvn)
    confint.diffbetasandwich(mvn)
    adf <- DiffBetaSandwich(BetaADF(object))
    print.diffbetasandwich(adf)
    summary.diffbetasandwich(adf)
    coef.diffbetasandwich(adf)
    vcov.diffbetasandwich(adf)
    confint.diffbetasandwich(adf)
    hc0 <- DiffBetaSandwich(BetaHC(object, type = "hc0"))
    print.diffbetasandwich(hc0)
    summary.diffbetasandwich(hc0)
    coef.diffbetasandwich(hc0)
    vcov.diffbetasandwich(hc0)
    confint.diffbetasandwich(hc0)
    hc1 <- DiffBetaSandwich(BetaHC(object, type = "hc1"))
    print.diffbetasandwich(hc1)
    summary.diffbetasandwich(hc1)
    coef.diffbetasandwich(hc1)
    vcov.diffbetasandwich(hc1)
    confint.diffbetasandwich(hc1)
    hc2 <- DiffBetaSandwich(BetaHC(object, type = "hc2"))
    print.diffbetasandwich(hc2)
    summary.diffbetasandwich(hc2)
    coef.diffbetasandwich(hc2)
    vcov.diffbetasandwich(hc2)
    confint.diffbetasandwich(hc2)
    hc3 <- DiffBetaSandwich(BetaHC(object, type = "hc3"))
    print.diffbetasandwich(hc3)
    summary.diffbetasandwich(hc3)
    coef.diffbetasandwich(hc3)
    vcov.diffbetasandwich(hc3)
    confint.diffbetasandwich(hc3)
    hc4 <- DiffBetaSandwich(BetaHC(object, type = "hc4"))
    print.diffbetasandwich(hc4)
    summary.diffbetasandwich(hc4)
    coef.diffbetasandwich(hc4)
    vcov.diffbetasandwich(hc4)
    confint.diffbetasandwich(hc4)
    hc4m <- DiffBetaSandwich(BetaHC(object, type = "hc4m"))
    print.diffbetasandwich(hc4m)
    summary.diffbetasandwich(hc4m)
    coef.diffbetasandwich(hc4m)
    vcov.diffbetasandwich(hc4m)
    confint.diffbetasandwich(hc4m)
    hc5 <- DiffBetaSandwich(BetaHC(object, type = "hc5"))
    print.diffbetasandwich(hc5)
    summary.diffbetasandwich(hc5)
    coef.diffbetasandwich(hc5)
    vcov.diffbetasandwich(hc5)
    confint.diffbetasandwich(hc5)
  },
  text = "test-betaSandwich-diff-beta-sandwich-methods"
)
