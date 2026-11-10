## ---- test-betaSandwich-r-sq-sandwich-se
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)
    testthat::test_that(
      desc = paste(text, "standard error of R-squared"),
      code = {
        set.seed(seed = 42)
        n <- 5000
        x <- matrix(data = stats::rnorm(n = n * 2), ncol = 2)
        y <- drop(x = x %*% c(0.4, 0.3)) + stats::rnorm(n = n)
        object <- stats::lm(formula = y ~ x)
        rsq <- summary(object = object)$r.squared
        # normal-theory large-sample standard error
        se <- sqrt(x = 4 * rsq * (1 - rsq)^2 / n)
        for (std in list(
          BetaN(object = object),
          BetaADF(object = object),
          BetaHC(object = object, type = "hc3")
        )) {
          out <- RSqSandwich(object = std)
          testthat::expect_true(
            object = abs(
              x = sqrt(x = diag(x = stats::vcov(object = out)))[1] / se - 1
            ) <= tol
          )
        }
      }
    )
  },
  text = "test-betaSandwich-r-sq-sandwich-se",
  tol = 0.1
)
