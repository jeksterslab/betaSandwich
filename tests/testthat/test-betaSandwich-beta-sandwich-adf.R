## ---- test-betaSandwich-beta-sandwich-adf
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "coef"),
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
        out <- BetaADF(object = object)
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = out) - c(0.4951, 0.3915, 0.2632)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = out$est - stats::coef(object = out)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "se"),
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
        out <- BetaADF(object = object)
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = out)[, "se"] - c(0.0674, 0.0710, 0.0769)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = out))
              ) - summary(object = out)[, "se"]
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.0001,
  text = "test-betaSandwich-beta-sandwich-adf"
)
# This test compares the results of the package with Dudgeon (2017)
