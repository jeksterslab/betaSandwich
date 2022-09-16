## ---- test-betaSandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    object <- lm(rating ~ ., data = attitude)
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
