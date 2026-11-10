## ---- test-betaSandwich-beta-sandwich-std
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)
    set.seed(seed = 42)
    n <- 200
    x1 <- stats::rnorm(n = n)
    x2 <- stats::rbinom(n = n, size = 1, prob = 0.4)
    x3 <- stats::rnorm(n = n) + 0.5 * x1
    y <- 0.4 * x1 + 0.3 * x2 - 0.2 * x3 + stats::rnorm(n = n)
    df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
    object <- stats::lm(formula = y ~ x1 + x2 + x3, data = df)
    b <- stats::coef(object = object)[-1]
    sds <- sapply(X = df, FUN = stats::sd)
    testthat::test_that(
      desc = paste(text, "selective standardization"),
      code = {
        fits <- list(
          hc3 = function(...) {
            BetaHC(object = object, type = "hc3", ...)
          },
          mvn = function(...) {
            BetaN(object = object, ...)
          },
          adf = function(...) {
            BetaADF(object = object, ...)
          }
        )
        for (fn in fits) {
          raw <- fn(std_y = FALSE, std_x = character(length = 0))
          testthat::expect_true(
            object = all(abs(x = stats::coef(object = raw) - b) <= tol)
          )
          dummy <- fn(std_x = c("x1", "x3"))
          testthat::expect_true(
            object = abs(
              x = stats::coef(object = dummy)["x2"] - b["x2"] / sds["y"]
            ) <= tol
          )
          full <- fn()
          same <- fn(std_x = c("x1", "x2", "x3"))
          testthat::expect_true(
            object = all(
              abs(
                x = stats::vcov(object = full) - stats::vcov(object = same)
              ) <= tol
            )
          )
          testthat::expect_error(object = DiffBetaSandwich(object = raw))
        }
        # HC0 sandwich of the raw slopes
        x <- stats::model.matrix(object = object)
        e <- stats::residuals(object = object)
        bread <- solve(a = crossprod(x = x))
        hc0 <- bread %*% crossprod(x = x * e) %*% bread
        raw <- BetaHC(
          object = object,
          type = "hc0",
          std_y = FALSE,
          std_x = character(length = 0)
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = raw))
              ) / sqrt(x = diag(x = hc0))[-1] - 1
            ) <= 0.05
          )
        )
      }
    )
  },
  text = "test-betaSandwich-beta-sandwich-std",
  tol = 1e-10
)
