## ---- test-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)
    testthat::test_that(
      paste(text, "methods"),
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
        lm_process <- betaSandwich:::.ProcessLM(object)
        print(
          betaSandwich:::.JacobianVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = FALSE
          )
        )
        print(
          betaSandwich:::.JacobianVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = TRUE
          )
        )
        print(
          betaSandwich:::.JacobianVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = FALSE
          )
        )
        print(
          betaSandwich:::.JacobianVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = TRUE
          )
        )
        testthat::expect_true(
          TRUE
        )
      }
    )
  },
  text = "test-zzz-coverage"
)
