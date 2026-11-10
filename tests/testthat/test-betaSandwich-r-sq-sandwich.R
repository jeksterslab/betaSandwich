## ---- test-betaSandwich-r-sq-sandwich
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
        ############################################################
        # coverage #################################################
        ############################################################
        lm_process <- .ProcessLM(object = object)
        .JacVechSigmaWRTTheta(
          beta = lm_process$beta,
          sigmacapx = lm_process$sigmacap[
            2:lm_process$k,
            2:lm_process$k,
            drop = TRUE
          ],
          q = lm_process$q,
          p = lm_process$p,
          rsq = NULL
        )
        ############################################################
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        mvn <- RSqSandwich(object = BetaN(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = mvn)[, "est"] - stats::coef(object = mvn)
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
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        adf <- RSqSandwich(object = BetaADF(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = adf)[, "est"] - stats::coef(object = adf)
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
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        hc3 <- RSqSandwich(object = BetaHC(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc3)[, "est"] - stats::coef(object = hc3)
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
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        mvn <- RSqSandwich(object = BetaN(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = mvn)[, "est"] - stats::coef(object = mvn)
            ) <= tol
          )
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
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        adf <- RSqSandwich(object = BetaADF(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = adf)[, "est"] - stats::coef(object = adf)
            ) <= tol
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
        r_sq <- summary(object = object)$r.squared
        adj <- summary(object = object)$adj.r.squared
        hc3 <- RSqSandwich(object = BetaHC(object = object))
        testthat::expect_true(
          object = all(
            abs(
              x = c(
                r_sq,
                adj
              ) - stats::coef(object = hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          object = all(
            abs(
              x = summary(object = hc3)[, "est"] - stats::coef(object = hc3)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-betaSandwich-r-sq-sandwich",
  tol = 0.0001
)
