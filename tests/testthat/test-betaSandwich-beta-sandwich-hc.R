## ---- test-betaSandwich-beta-sandwich-hc
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
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
    object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = df)
    out <- BetaHC(object)
    hc0 <- BetaHC(object, type = "hc0")
    hc1 <- BetaHC(object, type = "hc1")
    hc2 <- BetaHC(object, type = "hc2")
    hc3 <- BetaHC(object, type = "hc3")
    hc4 <- BetaHC(object, type = "hc4")
    hc4m <- BetaHC(object, type = "hc4m")
    hc5 <- BetaHC(object, type = "hc5")
    testthat::test_that(
      paste(text, "coef"),
      {
        testthat::expect_true(
          all(
            abs(
              coef(out) - c(0.4951, 0.3915, 0.2632)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              out$est - coef(out)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc0)[, "se"] - c(0.0668, 0.0703, 0.0760)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc0))) - summary(hc0)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc1)[, "se"] - c(0.0699, 0.0736, 0.0795)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc1))) - summary(hc1)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc2)[, "se"] - c(0.0723, 0.0758, 0.0805)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc2))) - summary(hc2)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc3)[, "se"] - c(0.0786, 0.0818, 0.0855)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc3))) - summary(hc3)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc4)[, "se"] - c(0.0810, 0.0821, 0.0834)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc4))) - summary(hc4)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc4m)[, "se"] - c(0.0811, 0.0843, 0.0868)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc4m))) - summary(hc4m)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5"),
      {
        testthat::expect_true(
          all(
            abs(
              summary(hc5)[, "se"] - c(0.0730, 0.0757, 0.0794)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              sqrt(diag(vcov(hc5))) - summary(hc5)[, "se"]
            ) <= tol
          )
        )
      }
    )
  },
  tol = 0.0001,
  text = "test-betaSandwich-beta-sandwich-hc"
)
# This test compares the results of the package with Dudgeon (2017)
