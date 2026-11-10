## ---- test-betaSandwich-beta-sandwich-hc
lapply(
  X = 1,
  FUN = function(i,
                 tol,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "coef"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        testthat::expect_true(
          object = all(
            abs(
              x = stats::coef(object = out) - c(0.4951, 0.3915, 0.2632)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = out$est - stats::coef(object = out)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc0"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc0 <- BetaHC(object = object, type = "hc0")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc0)[, "se"] - c(0.0668, 0.0703, 0.0760)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc0))
              ) - summary(object = hc0)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc1"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc1 <- BetaHC(object = object, type = "hc1")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc1)[, "se"] - c(0.0699, 0.0736, 0.0795)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc1))
              ) - summary(object = hc1)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc2"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc2 <- BetaHC(object = object, type = "hc2")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc2)[, "se"] - c(0.0723, 0.0758, 0.0805)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc2))
              ) - summary(object = hc2)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc3"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc3 <- BetaHC(object = object, type = "hc3")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc3)[, "se"] - c(0.0786, 0.0818, 0.0855)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc3))
              ) - summary(object = hc3)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc4 <- BetaHC(object = object, type = "hc4")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc4)[, "se"] - c(0.0810, 0.0821, 0.0834)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc4))
              ) - summary(object = hc4)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4m"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc4m <- BetaHC(object = object, type = "hc4m")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc4m)[, "se"] - c(0.0811, 0.0843, 0.0868)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc4m))
              ) - summary(object = hc4m)[, "se"]
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc5"),
      code = {
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = df
        )
        out <- BetaHC(object = object)
        hc5 <- BetaHC(object = object, type = "hc5")
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc5)[, "se"] - c(0.0730, 0.0757, 0.0794)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = sqrt(
                x = diag(x = stats::vcov(object = hc5))
              ) - summary(object = hc5)[, "se"]
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
