## ---- test-external-betaSandwich-r-sq-mvn
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
    sandwich <- BetaN(object = object)
    rsq <- RSqSandwich(object = sandwich)
    ci <- stats::confint(object = rsq)
    model <- paste(
      "QUALITY ~ NARTIC + PCTGRT + PCTSUPP;",
      "QUALITY ~~ sigmasq * QUALITY;",
      "rsq := 1 - sigmasq;",
      "adj := 1 - ((", n, " - 1) / (", n, " - 4)) * (1 - rsq)"
    )
    lav <- lavaan::sem(
      model = model,
      data = df,
      estimator = "ML"
    )
    std <- lavaan::standardizedSolution(object = lav)
    testthat::test_that(
      desc = paste(text, "coef"),
      code = {
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = rsq) - std[11:12, "est.std"]
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
              x = sqrt(x = diag(x = stats::vcov(object = rsq))) - std[11:12, "se"]
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
              x = ci[, 1] - std[11:12, "ci.lower"]
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
              x = ci[, 2] - std[11:12, "ci.upper"]
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-external-betaSandwich-r-sq-mvn",
  n = 100000L,
  tol = 0.001
)
