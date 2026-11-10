## ---- test-betaSandwich-diff-beta-sandwich
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "mvn", "multiple regression"),
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
        mvn <- DiffBetaSandwich(object = BetaN(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - stats::coef(object = mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf", "multiple regression"),
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
        adf <- DiffBetaSandwich(object = BetaADF(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - adf$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc0", "multiple regression"),
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
        hc0 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc0"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc0$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc1", "multiple regression"),
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
        hc1 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc1"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc1$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc2", "multiple regression"),
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
        hc2 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc2"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc2$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc3", "multiple regression"),
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
        hc3 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc3"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc3$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4m", "multiple regression"),
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
        hc4m <- DiffBetaSandwich(
          object = BetaHC(object = object, type = "hc4m")
        )
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc4m$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4", "multiple regression"),
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
        hc4 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc4"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc4$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc5", "multiple regression"),
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
        hc5 <- DiffBetaSandwich(object = BetaHC(object = object, type = "hc5"))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - hc5$est
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "mvn", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(object = BetaN(object = object))
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "adf", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(object = BetaADF(object = object))
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc0", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc0")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc1", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc1")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc2", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc2")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc3", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc3")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc4")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc4m", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc4m")
          )
        )
      }
    )
    testthat::test_that(
      desc = paste(text, "hc5", "simple regression"),
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
        object <- stats::lm(formula = QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          object = DiffBetaSandwich(
            object = BetaHC(object = object, type = "hc5")
          )
        )
      }
    )
  },
  text = "test-betaSandwich-diff-beta-sandwich",
  tol = 0.0001
)
