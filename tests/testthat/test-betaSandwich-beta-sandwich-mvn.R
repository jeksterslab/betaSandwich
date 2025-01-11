## ---- test-betaSandwich-beta-sandwich-mvn
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
    message(text)
    testthat::test_that(
      paste(text, "coef"),
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
        out <- BetaN(object)
        testthat::expect_true(
          all(
            abs(
              coef(out) - c(0.4951, 0.3915, 0.2632)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              out$est - coef(out)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "se"),
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
        out <- BetaN(object)
        testthat::expect_true(
          all(
            abs(
              summary(out)[, "se"] - c(0.0759, 0.0770, 0.0747)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(out))) - summary(out)[, "se"]
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.0001,
  text = "test-betaSandwich-beta-sandwich-mvn"
)
# This test compares the results of the package with Dudgeon (2017)
