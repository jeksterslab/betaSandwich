## ---- test-external-betaSandwich-r-sq-mlm
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
    sandwich <- BetaHC(object, type = "hc0")
    rsq <- RSqBetaSandwich(sandwich)
    ci <- confint(rsq)
    model <- paste(
      "QUALITY ~ NARTIC + PCTGRT + PCTSUPP;",
      "QUALITY ~~ sigmasq * QUALITY;",
      "rsq := 1 - sigmasq;",
      "adj := 1 - ((", n, " - 1) / (", n, " - 4)) * (1 - rsq)"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "MLM"
    )
    std <- lavaan::standardizedSolution(lav)
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(rsq) - std[11:12, "est.std"]
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
              sqrt(diag(vcov(rsq))) - std[11:12, "se"]
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
              ci[, 1] - std[11:12, "ci.lower"]
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
              ci[, 2] - std[11:12, "ci.upper"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaSandwich-r-sq-mlm",
  n = 100000L,
  tol = 0.001
)
