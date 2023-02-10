## ---- test-betaSandwich-r-sq-beta-sandwich-methods
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
    mvn <- RSqBetaSandwich(BetaN(object))
    print.rsqbetasandwich(mvn)
    summary.rsqbetasandwich(mvn)
    coef.rsqbetasandwich(mvn)
    vcov.rsqbetasandwich(mvn)
    confint.rsqbetasandwich(mvn)
    adf <- RSqBetaSandwich(BetaADF(object))
    print.rsqbetasandwich(adf)
    summary.rsqbetasandwich(adf)
    coef.rsqbetasandwich(adf)
    vcov.rsqbetasandwich(adf)
    confint.rsqbetasandwich(adf)
    hc3 <- RSqBetaSandwich(BetaHC(object))
    print.rsqbetasandwich(hc3)
    summary.rsqbetasandwich(hc3)
    coef.rsqbetasandwich(hc3)
    vcov.rsqbetasandwich(hc3)
    confint.rsqbetasandwich(hc3)
    object <- lm(QUALITY ~ NARTIC, data = df)
    mvn <- RSqBetaSandwich(BetaN(object))
    print.rsqbetasandwich(mvn)
    summary.rsqbetasandwich(mvn)
    coef.rsqbetasandwich(mvn)
    vcov.rsqbetasandwich(mvn)
    confint.rsqbetasandwich(mvn)
    adf <- RSqBetaSandwich(BetaADF(object))
    print.rsqbetasandwich(adf)
    summary.rsqbetasandwich(adf)
    coef.rsqbetasandwich(adf)
    vcov.rsqbetasandwich(adf)
    confint.rsqbetasandwich(adf)
    hc3 <- RSqBetaSandwich(BetaHC(object))
    print.rsqbetasandwich(hc3)
    summary.rsqbetasandwich(hc3)
    coef.rsqbetasandwich(hc3)
    vcov.rsqbetasandwich(hc3)
    confint.rsqbetasandwich(hc3)
  },
  text = "test-betaSandwich-r-sq-beta-sandwich-methods"
)
