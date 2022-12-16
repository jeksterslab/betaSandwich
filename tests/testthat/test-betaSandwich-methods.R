## ---- test-betaSandwich-methods
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
    hc <- BetaHC(object)
    print.betasandwich(hc)
    summary.betasandwich(hc)
    coef.betasandwich(hc)
    vcov.betasandwich(hc)
    confint.betasandwich(hc)
    mvn <- BetaN(object)
    print.betasandwich(mvn)
    summary.betasandwich(mvn)
    coef.betasandwich(mvn)
    vcov.betasandwich(mvn)
    confint.betasandwich(mvn)
    object <- lm(QUALITY ~ NARTIC, data = nas1982)
    hc <- BetaHC(object)
    print.betasandwich(hc)
    summary.betasandwich(hc)
    coef.betasandwich(hc)
    vcov.betasandwich(hc)
    confint.betasandwich(hc)
    mvn <- BetaN(object)
    print.betasandwich(mvn)
    summary.betasandwich(mvn)
    coef.betasandwich(mvn)
    vcov.betasandwich(mvn)
    confint.betasandwich(mvn)
  },
  text = "test-betaSandwich-methods"
)
