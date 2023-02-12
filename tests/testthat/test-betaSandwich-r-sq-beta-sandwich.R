## ---- test-betaSandwich-r-sq-beta-sandwich
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
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
    ############################################################
    # coverage #################################################
    ############################################################
    lm_process <- .ProcessLM(object)
    .JacobianVechSigmaWRTTheta(
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
    r_sq <- summary(object)$r.squared
    adj <- summary(object)$adj.r.squared
    mvn <- RSqBetaSandwich(BetaN(object))
    adf <- RSqBetaSandwich(BetaADF(object))
    hc3 <- RSqBetaSandwich(BetaHC(object))
    testthat::test_that(
      paste(text, "mvn", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(mvn)[, "est"] - coef(mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(adf)[, "est"] - coef(adf)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "multiple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc3)[, "est"] - coef(hc3)
            ) <= tol
          )
        )
      }
    )
    object <- lm(QUALITY ~ NARTIC, data = df)
    r_sq <- summary(object)$r.squared
    adj <- summary(object)$adj.r.squared
    mvn <- RSqBetaSandwich(BetaN(object))
    adf <- RSqBetaSandwich(BetaADF(object))
    hc3 <- RSqBetaSandwich(BetaHC(object))
    testthat::test_that(
      paste(text, "mvn", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(mvn)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(mvn)[, "est"] - coef(mvn)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "adf", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(adf)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(adf)[, "est"] - coef(adf)
            ) <= tol
          )
        )
      }
    )
    testthat::test_that(
      paste(text, "hc3", "simple regression"),
      {
        testthat::expect_true(
          all(
            abs(
              c(
                r_sq,
                adj
              ) - coef(hc3)
            ) <= tol
          )
        )
        testthat::expect_true(
          all(
            abs(
              summary(hc3)[, "est"] - coef(hc3)
            ) <= tol
          )
        )
      }
    )
  },
  text = "test-betaSandwich-r-sq-beta-sandwich",
  tol = 0.0001
)
