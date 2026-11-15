## ---- test-external-betaSandwich-beta-sandwich-mlm
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    set.seed(seed = 42)
    message(text)
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
    df <- as.data.frame(
      x = MASS::mvrnorm(
        n = n,
        mu = colMeans(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        Sigma = stats::cov(
          x = df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        empirical = TRUE
      )
    )
    object <- stats::lm(formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    delta <- BetaHC(object = object, type = "hc0")
    ci <- stats::confint(object = delta)
    model <- "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP"
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "MLM"
    )
    std <- lavaan::standardizedSolution(object = lav)
    testthat::test_that(
      desc = paste(text, "coef"),
      code = {
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = delta) - std[1:3, "est.std"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "se"),
      code = {
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(x = diag(x = stats::vcov(object = delta))) - std[1:3, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.lower"),
      code = {
        testthat::expect_true(
          object = all(
            abs(
              x = ci[, 1] - std[1:3, "ci.lower"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "ci.upper"),
      code = {
        testthat::expect_true(
          object = all(
            abs(
              x = ci[, 2] - std[1:3, "ci.upper"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaSandwich-beta-sandwich-mlm",
  n = 100000L,
  tol = 0.001
)
