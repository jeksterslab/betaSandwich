## ---- test-betaSandwich-es-sandwich-methods
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(
        text,
        "methods for every function and covariance estimator"
      ),
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
        object <- stats::lm(
          formula = QUALITY ~ NARTIC + PCTGRT + PCTSUPP,
          data = nas1982
        )
        fits <- c(
          list(
            BetaN(object = object),
            BetaADF(object = object)
          ),
          lapply(
            X = c("hc0", "hc1", "hc2", "hc3", "hc4", "hc4m", "hc5"),
            FUN = function(type) {
              BetaHC(object = object, type = type)
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
          for (fun in names(x = funs)) {
            out <- funs[[fun]](object = fit, alpha = c(0.05, 0.01))
            testthat::expect_s3_class(object = out, class = "essandwich")
            testthat::expect_identical(object = out$fun, expected = fun)
            testthat::expect_output(
              object = print(x = out),
              regexp = "standard errors"
            )
            testthat::expect_output(
              object = print(x = out, alpha = 0.10, digits = 2)
            )
            sum_out <- summary(object = out)
            testthat::expect_s3_class(
              object = sum_out,
              class = "summary.essandwich"
            )
            testthat::expect_output(
              object = print(x = sum_out),
              regexp = toupper(x = fit$args$type)
            )
            testthat::expect_equal(
              object = sum_out[, "est"],
              expected = stats::coef(object = out)
            )
            testthat::expect_equal(
              object = unname(obj = sum_out[, "se"]),
              expected = unname(
                obj = sqrt(x = diag(x = stats::vcov(object = out)))
              )
            )
            testthat::expect_identical(
              object = colnames(x = summary(object = out)),
              expected = c(
                "est",
                "se",
                "z",
                "p",
                "0.5%",
                "2.5%",
                "97.5%",
                "99.5%"
              )
            )
            ci <- stats::confint(object = out)
            testthat::expect_identical(
              object = colnames(x = ci),
              expected = c("2.5 %", "97.5 %")
            )
            testthat::expect_identical(
              object = rownames(x = ci),
              expected = names(x = stats::coef(object = out))
            )
            testthat::expect_true(
              object = all(ci[, 1] < stats::coef(object = out))
            )
            testthat::expect_true(
              object = all(ci[, 2] > stats::coef(object = out))
            )
            testthat::expect_equal(
              object = as.vector(
                x = stats::confint(object = out, parm = 1, level = 0.90)
              ),
              expected = as.vector(
                x = stats::coef(
                  object = out
                )[1] + stats::qnorm(p = c(0.05, 0.95)) *
                  sqrt(x = diag(x = stats::vcov(object = out)))[1]
              )
            )
            testthat::expect_error(
              object = summary(object = out, alpha = 1.5)
            )
          }
        }
        testthat::expect_s3_class(
          object = DiffBetaSandwich(object = fits[[1]]),
          class = "diffbetasandwich"
        )
      }
    )
  },
  text = "test-betaSandwich-es-sandwich-methods"
)
