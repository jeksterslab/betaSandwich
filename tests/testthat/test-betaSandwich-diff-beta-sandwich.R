## ---- test-betaSandwich-diff-beta-sandwich
# nolint start: cyclocomp_linter
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)
    testthat::test_that(
      paste(text, "mvn", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        mvn <- DiffBetaSandwich(BetaN(object))
        testthat::expect_true(
          all(
            abs(
              c(
                0.4951 - 0.3915,
                0.4951 - 0.2632,
                0.3915 - 0.2632
              ) - coef(mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        adf <- DiffBetaSandwich(BetaADF(object))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc0", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc0 <- DiffBetaSandwich(BetaHC(object, type = "hc0"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc1", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc1 <- DiffBetaSandwich(BetaHC(object, type = "hc1"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc2", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc2 <- DiffBetaSandwich(BetaHC(object, type = "hc2"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc3", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc3 <- DiffBetaSandwich(BetaHC(object, type = "hc3"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc4m", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc4m <- DiffBetaSandwich(BetaHC(object, type = "hc4m"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc4", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc4 <- DiffBetaSandwich(BetaHC(object, type = "hc4"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "hc5", "multiple regression"),
      {
        testthat::skip_on_cran()
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
        hc5 <- DiffBetaSandwich(BetaHC(object, type = "hc5"))
        testthat::expect_true(
          all(
            abs(
              c(
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
      paste(text, "mvn", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaN(object))
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaADF(object))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc0", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc0"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc1", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc1"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc2", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc2"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc3"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc4"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc4m", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc4m"))
        )
      }
    )
    testthat::test_that(
      paste(text, "hc5", "simple regression"),
      {
        testthat::skip_on_cran()
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
        object <- lm(QUALITY ~ NARTIC, data = df)
        testthat::expect_error(
          DiffBetaSandwich(BetaHC(object, type = "hc5"))
        )
      }
    )
  },
  text = "test-betaSandwich-diff-beta-sandwich",
  tol = 0.0001
)
# nolint end
