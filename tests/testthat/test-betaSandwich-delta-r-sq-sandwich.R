## ---- test-betaSandwich-delta-r-sq-sandwich
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    message(text)

    set.seed(42)

    if (!identical(Sys.getenv("NOT_CRAN"), "true") && !interactive()) {
      message("CRAN: tests skipped.")
      # nolint start
      return(invisible(NULL))
      # nolint end
    }

    # effect size as a function of betastar and rhocapx
    es <- function(betastar,
                   rhocapx) {
      rsq <- drop(crossprod(betastar, rhocapx %*% betastar))
      srsq <- betastar^2 / diag(solve(rhocapx))
      srsq
    }
    n <- 2000
    x <- matrix(data = stats::rnorm(n * 3), ncol = 3)
    x[, 2] <- 0.5 * x[, 1] + x[, 2]
    y <- drop(x %*% c(0.4, 0.3, 0.2)) + stats::rnorm(n) * sqrt(1 + x[, 1]^2)
    colnames(x) <- c("x1", "x2", "x3")
    df <- data.frame(y = y, x)
    object <- lm(y ~ x1 + x2 + x3, data = df)

    testthat::test_that(
      paste(text, "estimates"),
      {
        testthat::skip_on_cran()
        ref <- (function() {
          r2 <- summary(object)$r.squared
          sapply(
            1:3,
            function(j) {
              r2 - summary(
                stats::lm(df$y ~ as.matrix(df[paste0("x", setdiff(1:3, j))]))
              )$r.squared
            }
          )
        })()
        for (fit in list(BetaN(object), BetaADF(object), BetaHC(object))) {
          testthat::expect_equal(
            unname(coef(DeltaRSqSandwich(fit))),
            unname(ref),
            tolerance = tol
          )
        }
      }
    )

    testthat::test_that(
      paste(text, "standard errors against a Monte Carlo of the same vcov"),
      {
        testthat::skip_on_cran()
        p <- 3
        lower <- which(lower.tri(diag(p)), arr.ind = TRUE)
        for (type in c("mvn", "adf", "hc3")) {
          fit <- switch(
            EXPR = type,
            mvn = BetaN(object),
            adf = BetaADF(object),
            BetaHC(object, type = type)
          )
          lm_process <- fit$lm_process
          if (type %in% c("mvn", "adf")) {
            vcov_theta <- fit$acov / lm_process$n
          } else {
            vcov_theta <- .CovHC(
              acov = fit$acov,
              type = type,
              n = lm_process$n,
              df = lm_process$df
            )
          }
          theta <- c(
            lm_process$betastar,
            lm_process$sigma,
            lm_process$rhocap[-1, -1][lower.tri(diag(p))]
          )
          draws <- matrix(
            data = stats::rnorm(2e4 * length(theta)),
            ncol = length(theta)
          ) %*% chol(vcov_theta)
          draws <- sweep(draws, 2, theta, "+")
          values <- t(
            apply(
              X = draws,
              MARGIN = 1,
              FUN = function(th) {
                rhocapx <- diag(p)
                rhocapx[lower] <- th[2 * p + 1 + seq_len(nrow(lower))]
                rhocapx[lower[, 2:1]] <- th[2 * p + 1 + seq_len(nrow(lower))]
                es(betastar = th[seq_len(p)], rhocapx = rhocapx)
              }
            )
          )
          if (nrow(values) == 1) {
            values <- t(values)
          }
          out <- DeltaRSqSandwich(fit)
          ratio <- sqrt(diag(vcov(out))) / apply(values, 2, stats::sd)
          testthat::expect_true(all(abs(ratio - 1) < 0.03))
        }
      }
    )

    testthat::test_that(
      paste(text, "errors"),
      {
        testthat::skip_on_cran()
        testthat::expect_error(DeltaRSqSandwich(object))
        testthat::expect_error(DeltaRSqSandwich(list()))
        one <- BetaHC(lm(y ~ x1, data = df))
        if (2 > 1) {
          testthat::expect_error(
            DeltaRSqSandwich(one),
            "Two or more regressors"
          )
        } else {
          out <- DeltaRSqSandwich(one)
          testthat::expect_true(all(is.finite(sqrt(diag(vcov(out))))))
        }
      }
    )
  },
  text = "test-betaSandwich-delta-r-sq-sandwich",
  tol = 1e-8
)
