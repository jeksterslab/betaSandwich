## ---- test-betaSandwich-beta-adf
lapply(
  X = 1,
  FUN = function(i,
                 tol,
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
    coefs <- coef(BetaADF(object))
    adf <- .BetaCI(BetaADF(object))
    testthat::test_that(
      paste(text, "coefs"),
      {
        testthat::expect_true(
          all(
            abs(
              coefs - c(0.4951, 0.3915, 0.2632)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf"),
      {
        testthat::expect_true(
          all(
            abs(
              adf[, "se"] - c(0.0674, 0.0710, 0.0769)
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.0001,
  text = "test-betaSandwich-beta-adf"
)
# This test compares the results of the package with Dudgeon (2017)
