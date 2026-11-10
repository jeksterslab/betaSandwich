## ---- test-betaSandwich-beta-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "methods"),
      code = {
        if (!exists(x = "nas1982")) {
          try(
            expr = utils::data(
              "nas1982",
              package = "betaSandwich"
            ),
            silent = TRUE
          )
        }
        df <- nas1982
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        hc <- BetaHC(object = object)
        print.betasandwich(x = hc)
        summary.betasandwich(object = hc)
        print.summary.betasandwich(x = summary.betasandwich(object = hc))
        coef.betasandwich(object = hc)
        vcov.betasandwich(object = hc)
        confint.betasandwich(object = hc)
        mvn <- BetaN(object = object)
        print.betasandwich(x = mvn)
        summary.betasandwich(object = mvn)
        print.summary.betasandwich(x = summary.betasandwich(object = mvn))
        coef.betasandwich(object = mvn)
        vcov.betasandwich(object = mvn)
        confint.betasandwich(object = mvn)
        adf <- BetaADF(object = object)
        print.betasandwich(x = adf)
        summary.betasandwich(object = adf)
        print.summary.betasandwich(x = summary.betasandwich(object = adf))
        coef.betasandwich(object = adf)
        vcov.betasandwich(object = adf)
        confint.betasandwich(object = adf)
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        hc <- BetaHC(object = object)
        print.betasandwich(x = hc)
        summary.betasandwich(object = hc)
        print.summary.betasandwich(x = summary.betasandwich(object = hc))
        coef.betasandwich(object = hc)
        vcov.betasandwich(object = hc)
        confint.betasandwich(object = hc)
        mvn <- BetaN(object = object)
        print.betasandwich(x = mvn)
        summary.betasandwich(object = mvn)
        print.summary.betasandwich(x = summary.betasandwich(object = mvn))
        coef.betasandwich(object = mvn)
        vcov.betasandwich(object = mvn)
        confint.betasandwich(object = mvn)
        adf <- BetaADF(object = object)
        print.betasandwich(x = adf)
        summary.betasandwich(object = adf)
        print.summary.betasandwich(x = summary.betasandwich(object = adf))
        coef.betasandwich(object = adf)
        vcov.betasandwich(object = adf)
        confint.betasandwich(object = adf)
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  text = "test-betaSandwich-beta-sandwich-methods"
)
