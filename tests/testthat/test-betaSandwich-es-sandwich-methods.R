## ---- test-betaSandwich-es-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    set.seed(42)

    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }

    testthat::test_that(
      paste(text, "methods for every function and covariance estimator"),
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
        object <- lm(QUALITY ~ NARTIC + PCTGRT + PCTSUPP, data = nas1982)
        fits <- c(
          list(
            BetaN(object),
            BetaADF(object)
          ),
          lapply(
            X = c("hc0", "hc1", "hc2", "hc3", "hc4", "hc4m", "hc5"),
            FUN = function(type) {
              BetaHC(object, type = type)
            }
          )
        )
        funs <- list(
          RSqSandwich = RSqSandwich,
          SCorSandwich = SCorSandwich,
          PCorSandwich = PCorSandwich,
          DeltaRSqSandwich = DeltaRSqSandwich,
          FSqSandwich = FSqSandwich,
          DiffBetaSandwich = DiffBetaSandwich
        )
        for (fit in fits) {
          for (fun in names(funs)) {
            out <- funs[[fun]](fit, alpha = c(0.05, 0.01))
            testthat::expect_s3_class(out, "essandwich")
            testthat::expect_identical(out$fun, fun)
            testthat::expect_output(print(out), "standard errors")
            testthat::expect_output(print(out, alpha = 0.10, digits = 2))
            sum_out <- summary(out)
            testthat::expect_s3_class(sum_out, "summary.essandwich")
            testthat::expect_output(print(sum_out), toupper(fit$args$type))
            testthat::expect_equal(sum_out[, "est"], coef(out))
            testthat::expect_equal(
              unname(sum_out[, "se"]),
              unname(sqrt(diag(vcov(out))))
            )
            testthat::expect_identical(
              colnames(summary(out)),
              c("est", "se", "z", "p", "0.5%", "2.5%", "97.5%", "99.5%")
            )
            ci <- confint(out)
            testthat::expect_identical(colnames(ci), c("2.5 %", "97.5 %"))
            testthat::expect_identical(rownames(ci), names(coef(out)))
            testthat::expect_true(all(ci[, 1] < coef(out)))
            testthat::expect_true(all(ci[, 2] > coef(out)))
            testthat::expect_equal(
              as.vector(confint(out, parm = 1, level = 0.90)),
              as.vector(
                coef(out)[1] + stats::qnorm(c(0.05, 0.95)) *
                  sqrt(diag(vcov(out)))[1]
              )
            )
            testthat::expect_error(
              summary(out, alpha = 1.5)
            )
          }
        }
        testthat::expect_s3_class(
          DiffBetaSandwich(fits[[1]]),
          "diffbetasandwich"
        )
      }
    )
  },
  text = "test-betaSandwich-es-sandwich-methods"
)
