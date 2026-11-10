## ---- test-zzz-coverage
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "methods"),
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
        lm_process <- betaSandwich:::.ProcessLM(object = object)
        print(
          x = betaSandwich:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = FALSE
          )
        )
        print(
          x = betaSandwich:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = NULL,
            fixed_x = TRUE
          )
        )
        print(
          x = betaSandwich:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = FALSE
          )
        )
        print(
          x = betaSandwich:::.JacVechSigmaWRTTheta(
            beta = lm_process$beta,
            sigmacapx = lm_process$sigmacapx,
            q = lm_process$q,
            p = lm_process$p,
            rsq = lm_process$rsq[1],
            fixed_x = TRUE
          )
        )
        testthat::expect_true(
          object = TRUE
        )
      }
    )
  },
  text = "test-zzz-coverage"
)
