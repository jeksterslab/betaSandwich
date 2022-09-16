## ---- test-betaSandwich-beta-mvn
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
    message(text)
    object <- lm(rating ~ ., data = attitude)
    coefs <- coef(BetaN(object))
    mvn <- .BetaCI(BetaN(object))
    result_coef <- c(
      0.67072520,
      -0.07342743,
      0.30887024,
      0.06981172,
      0.03119975,
      -0.18346445
    )
    result_se <- c(
      0.1432881,
      0.1196668,
      0.1431252,
      0.1656987,
      0.1046937,
      0.1337535
    )
    testthat::test_that(
      paste(text, "coefs"),
      {
        testthat::expect_true(
          all(
            abs(
              coefs - result_coef
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "mvn"),
      {
        testthat::expect_true(
          all(
            abs(
              mvn[, "se"] - result_se
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.0001,
  text = "test-betaSandwich-beta-mvn"
)
