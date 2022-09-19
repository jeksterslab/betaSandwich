## ---- test-betaSandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
    hc <- BetaHC(object)
    print.betaSandwich(hc)
    summary.betaSandwich(hc)
    coef.betaSandwich(hc)
    vcov.betaSandwich(hc)
    confint.betaSandwich(hc)
    mvn <- BetaN(object)
    print.betaSandwich(mvn)
    summary.betaSandwich(mvn)
    coef.betaSandwich(mvn)
    vcov.betaSandwich(mvn)
    confint.betaSandwich(mvn)
    object <- lm(QUALITY ~ NARTIC, data = nas1982)
    hc <- BetaHC(object)
    print.betaSandwich(hc)
    summary.betaSandwich(hc)
    coef.betaSandwich(hc)
    vcov.betaSandwich(hc)
    confint.betaSandwich(hc)
    mvn <- BetaN(object)
    print.betaSandwich(mvn)
    summary.betaSandwich(mvn)
    coef.betaSandwich(mvn)
    vcov.betaSandwich(mvn)
    confint.betaSandwich(mvn)
  },
  text = "test-betaSandwich-methods"
)
