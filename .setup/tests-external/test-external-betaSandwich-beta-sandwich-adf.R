## ---- test-external-betaSandwich-beta-sandwich-adf
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 n,
                 tol) {
    set.seed(42)
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
    df <- as.data.frame(
      MASS::mvrnorm(
        n = n,
        mu = colMeans(
          df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        Sigma = cov(
          df[, c("QUALITY", "NARTIC", "PCTGRT", "PCTSUPP")]
        ),
        empirical = TRUE
      )
    )
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    delta <- BetaADF(object)
    ci <- confint(delta)
    model <- "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP"
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "WLS"
    )
    std <- lavaan::standardizedSolution(lav)
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(delta) - std[1:3, "est.std"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "se"),
      {
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(delta))) - std[1:3, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ci.lower"),
      {
        testthat::expect_true(
          all(
            abs(
              ci[, 1] - std[1:3, "ci.lower"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "ci.upper"),
      {
        testthat::expect_true(
          all(
            abs(
              ci[, 2] - std[1:3, "ci.upper"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaSandwich-beta-sandwich-adf",
  n = 100000L,
  tol = 0.001
)
