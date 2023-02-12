## ---- test-external-betaSandwich-diff-mvn
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
    sandwich <- BetaN(object)
    diff <- DiffBetaSandwich(sandwich)
    ci <- confint(diff)
    model <- paste(
      "QUALITY ~ b1 * NARTIC + b2 * PCTGRT + b3 * PCTSUPP;",
      "diff12 := b1 - b2; diff13 := b1 - b3; diff23 := b2 - b3"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "ML"
    )
    std <- lavaan::standardizedSolution(lav)
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(diff) - std[11:13, "est.std"]
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
              sqrt(diag(vcov(diff))) - std[11:13, "se"]
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
              ci[, 1] - std[11:13, "ci.lower"]
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
              ci[, 2] - std[11:13, "ci.upper"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaSandwich-diff-mvn",
  n = 100000L,
  tol = 0.001
)
