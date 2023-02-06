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
    mvn <- dif(BetaN(object))
    print.difbetasandwich(mvn)
    summary.difbetasandwich(mvn)
    coef.difbetasandwich(mvn)
    vcov.difbetasandwich(mvn)
    confint.difbetasandwich(mvn)
    adf <- dif(BetaADF(object))
    print.difbetasandwich(adf)
    summary.difbetasandwich(adf)
    coef.difbetasandwich(adf)
    vcov.difbetasandwich(adf)
    confint.difbetasandwich(adf)
    hc0 <- dif(BetaHC(object, type = "hc0"))
    print.difbetasandwich(hc0)
    summary.difbetasandwich(hc0)
    coef.difbetasandwich(hc0)
    vcov.difbetasandwich(hc0)
    confint.difbetasandwich(hc0)
    hc1 <- dif(BetaHC(object, type = "hc1"))
    print.difbetasandwich(hc1)
    summary.difbetasandwich(hc1)
    coef.difbetasandwich(hc1)
    vcov.difbetasandwich(hc1)
    confint.difbetasandwich(hc1)
    hc2 <- dif(BetaHC(object, type = "hc2"))
    print.difbetasandwich(hc2)
    summary.difbetasandwich(hc2)
    coef.difbetasandwich(hc2)
    vcov.difbetasandwich(hc2)
    confint.difbetasandwich(hc2)
    hc3 <- dif(BetaHC(object, type = "hc3"))
    print.difbetasandwich(hc3)
    summary.difbetasandwich(hc3)
    coef.difbetasandwich(hc3)
    vcov.difbetasandwich(hc3)
    confint.difbetasandwich(hc3)
    hc4 <- dif(BetaHC(object, type = "hc4"))
    print.difbetasandwich(hc4)
    summary.difbetasandwich(hc4)
    coef.difbetasandwich(hc4)
    vcov.difbetasandwich(hc4)
    confint.difbetasandwich(hc4)
    hc4m <- dif(BetaHC(object, type = "hc4m"))
    print.difbetasandwich(hc4m)
    summary.difbetasandwich(hc4m)
    coef.difbetasandwich(hc4m)
    vcov.difbetasandwich(hc4m)
    confint.difbetasandwich(hc4m)
    hc5 <- dif(BetaHC(object, type = "hc5"))
    print.difbetasandwich(hc5)
    summary.difbetasandwich(hc5)
    coef.difbetasandwich(hc5)
    vcov.difbetasandwich(hc5)
    confint.difbetasandwich(hc5)
  },
  text = "test-betaSandwich-diff-beta-sandwich-methods"
)
