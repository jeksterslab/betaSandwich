## ---- test-betaSandwich-beta-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::test_that(
      paste(text, "methods"),
      {
        testthat::skip_on_cran()
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
        print.summary.betasandwich(summary.betasandwich(hc))
        coef.betasandwich(hc)
        vcov.betasandwich(hc)
        confint.betasandwich(hc)
        mvn <- BetaN(object)
        print.betasandwich(mvn)
        summary.betasandwich(mvn)
        print.summary.betasandwich(summary.betasandwich(mvn))
        coef.betasandwich(mvn)
        vcov.betasandwich(mvn)
        confint.betasandwich(mvn)
        adf <- BetaADF(object)
        print.betasandwich(adf)
        summary.betasandwich(adf)
        print.summary.betasandwich(summary.betasandwich(adf))
        coef.betasandwich(adf)
        vcov.betasandwich(adf)
        confint.betasandwich(adf)
        object <- lm(QUALITY ~ NARTIC, data = df)
        hc <- BetaHC(object)
        print.betasandwich(hc)
        summary.betasandwich(hc)
        print.summary.betasandwich(summary.betasandwich(hc))
        coef.betasandwich(hc)
        vcov.betasandwich(hc)
        confint.betasandwich(hc)
        mvn <- BetaN(object)
        print.betasandwich(mvn)
        summary.betasandwich(mvn)
        print.summary.betasandwich(summary.betasandwich(mvn))
        coef.betasandwich(mvn)
        vcov.betasandwich(mvn)
        confint.betasandwich(mvn)
        adf <- BetaADF(object)
        print.betasandwich(adf)
        summary.betasandwich(adf)
        print.summary.betasandwich(summary.betasandwich(adf))
        coef.betasandwich(adf)
        vcov.betasandwich(adf)
        confint.betasandwich(adf)
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test-betaSandwich-beta-sandwich-methods"
)
