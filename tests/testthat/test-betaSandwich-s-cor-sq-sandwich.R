## ---- test-betaSandwich-s-cor-sq-sandwich
lapply(
  X = 1,
  FUN = function(i, text) {
    testthat::skip_on_cran()
    testthat::test_that(
      desc = paste(text, "projects the actual signed interval without draws"),
      code = {
        set.seed(seed = 713)
        fit <- stats::lm(formula = mpg ~ wt + hp, data = mtcars)
        context <- BetaHC(object = fit)
        signed <- SCorSandwich(object = context, alpha = 0.05)
        ci <- stats::confint(object = signed, level = 0.95)
        seed <- .Random.seed
        out <- SCorSqSandwich(object = context)
        testthat::expect_identical(object = .Random.seed, expected = seed)
        testthat::expect_equal(
          object = out$est,
          expected = unname(obj = stats::coef(object = signed)^2)
        )
        testthat::expect_equal(
          object = out$upper,
          expected = unname(obj = pmax(ci[, 1]^2, ci[, 2]^2))
        )
        testthat::expect_true(object = all(out$lower <= out$upper))
        testthat::expect_error(
          object = SCorSqSandwich(object = context, level = 1),
          regexp = "confidence level"
        )
      }
    )
  },
  text = "test-betaSandwich-s-cor-sq-sandwich"
)
