## ---- test-betaSandwich-f-sq-sandwich
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    # effect size as a function of betastar and rhocapx
    es <- function(betastar,
                   rhocapx) {
      rsq <- drop(x = crossprod(x = betastar, y = rhocapx %*% betastar))
      srsq <- betastar^2 / diag(x = solve(a = rhocapx))
      c(rsq / (1 - rsq), srsq / (1 - rsq))
    }
    n <- 2000
    x <- matrix(data = stats::rnorm(n = n * 3), ncol = 3)
    x[, 2] <- 0.5 * x[, 1] + x[, 2]
    y <- drop(
      x = x %*% c(0.4, 0.3, 0.2)
    ) + stats::rnorm(n = n) * sqrt(x = 1 + x[, 1]^2)
    colnames(x = x) <- c("x1", "x2", "x3")
    df <- data.frame(y = y, x)
    object <- stats::lm(formula = y ~ x1 + x2 + x3, data = df)

    testthat::test_that(
      desc = paste(text, "estimates"),
      code = {
        ref <- (function() {
          r2 <- summary(object = object)$r.squared
          c(
            r2 / (1 - r2),
            sapply(
              X = 1:3,
              FUN = function(j) {
                (r2 - summary(
                  object = stats::lm(
                    formula = df$y ~ as.matrix(
                      x = df[paste0("x", setdiff(x = 1:3, y = j))]
                    )
                  )
                )$r.squared) / (1 - r2)
              }
            )
          )
        })()
        for (fit in list(
          BetaN(object = object),
          BetaADF(object = object),
          BetaHC(object = object)
        )) {
          testthat::expect_equal(
            object = unname(
              obj = stats::coef(object = FSqSandwich(object = fit))
            ),
            expected = unname(obj = ref),
            tolerance = tol
          )
        }
      }
    )

    testthat::test_that(
      desc = paste(
        text,
        "standard errors against a Monte Carlo of the same vcov"
      ),
      code = {
        p <- 3
        lower <- which(x = lower.tri(x = diag(x = p)), arr.ind = TRUE)
        for (type in c("mvn", "adf", "hc3")) {
          fit <- switch(
            EXPR = type,
            mvn = BetaN(object = object),
            adf = BetaADF(object = object),
            BetaHC(object = object, type = type)
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
            lm_process$rhocap[-1, -1][lower.tri(x = diag(x = p))]
          )
          draws <- matrix(
            data = stats::rnorm(n = 2e4 * length(x = theta)),
            ncol = length(x = theta)
          ) %*% chol(x = vcov_theta)
          draws <- sweep(x = draws, MARGIN = 2, STATS = theta, FUN = "+")
          values <- t(
            x = apply(
              X = draws,
              MARGIN = 1,
              FUN = function(th) {
                rhocapx <- diag(x = p)
                rhocapx[lower] <- th[2 * p + 1 + seq_len(
                  length.out = nrow(x = lower)
                )]
                rhocapx[lower[, 2:1]] <- th[2 * p + 1 + seq_len(
                  length.out = nrow(x = lower)
                )]
                es(betastar = th[seq_len(length.out = p)], rhocapx = rhocapx)
              }
            )
          )
          if (nrow(x = values) == 1) {
            values <- t(x = values)
          }
          out <- FSqSandwich(object = fit)
          ratio <- sqrt(
            x = diag(x = stats::vcov(object = out))
          ) / apply(X = values, MARGIN = 2, FUN = stats::sd)
          testthat::expect_true(object = all(abs(x = ratio - 1) < 0.03))
        }
      }
    )

    testthat::test_that(
      desc = paste(text, "errors"),
      code = {
        testthat::expect_error(object = FSqSandwich(object = object))
        testthat::expect_error(object = FSqSandwich(object = list()))
        one <- BetaHC(object = stats::lm(formula = y ~ x1, data = df))
        if (1 > 1) {
          testthat::expect_error(
            object = FSqSandwich(object = one),
            regexp = "Two or more regressors"
          )
        } else {
          out <- FSqSandwich(object = one)
          testthat::expect_true(
            object = all(
              is.finite(x = sqrt(x = diag(x = stats::vcov(object = out))))
            )
          )
        }
      }
    )
  },
  text = "test-betaSandwich-f-sq-sandwich",
  tol = 1e-8
)
