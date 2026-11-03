## ---- test-betaSandwich-jac-f-sq-wrt-theta-star
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

    # central differences
    Num <- function(fun,
                    theta,
                    h = 1e-6) {
      out <- sapply(
        X = seq_along(theta),
        FUN = function(i) {
          e <- replace(0 * theta, i, h)
          (fun(theta + e) - fun(theta - e)) / (2 * h)
        }
      )
      if (is.null(dim(out))) {
        out <- matrix(data = out, nrow = 1)
      }
      out
    }
    Rho <- function(theta,
                    p) {
      rhocapx <- diag(p)
      if (p > 1) {
        idx <- 2 * p + 1 + seq_len(p * (p - 1) / 2)
        rhocapx[lower.tri(rhocapx)] <- theta[idx]
        rhocapx[upper.tri(rhocapx)] <- t(rhocapx)[upper.tri(rhocapx)]
      }
      rhocapx
    }
    Value <- function(theta,
                      p,
                      what) {
      betastar <- theta[seq_len(p)]
      rhocapx <- Rho(theta, p)
      rsq <- drop(crossprod(betastar, rhocapx %*% betastar))
      scor <- betastar / sqrt(diag(solve(rhocapx)))
      switch(
        EXPR = what,
        rsq = rsq,
        scor = scor,
        deltarsq = scor^2,
        pcor = scor^2 / (1 - rsq + scor^2),
        fsq = c(rsq / (1 - rsq), scor^2 / (1 - rsq))
      )
    }
    funs <- list(
      fsq = .JacFSqWRTThetaStar
    )
    for (p in c(1, 2, 5)) {
      a <- matrix(data = stats::rnorm(p * p), nrow = p)
      rhocapx <- stats::cov2cor(crossprod(a) + diag(p))
      betastar <- stats::rnorm(p) * 0.3
      while (drop(crossprod(betastar, rhocapx %*% betastar)) > 0.8) {
        betastar <- betastar / 2
      }
      theta <- c(
        betastar,
        1.3,
        stats::runif(p, 1, 2),
        rhocapx[lower.tri(rhocapx)]
      )
      for (what in names(funs)) {
        testthat::test_that(
          paste(text, what, "p =", p),
          {
            testthat::skip_on_cran()
            jcap <- funs[[what]](
              betastar = betastar,
              rhocapx = rhocapx
            )
            testthat::expect_identical(
              dim(jcap),
              c(
                length(Value(theta, p, what)),
                length(theta)
              )
            )
            testthat::expect_true(
              max(
                abs(
                  jcap - Num(
                    fun = function(x) Value(x, p, what),
                    theta = theta
                  )
                )
              ) < tol
            )
          }
        )
      }
    }
  },
  text = "test-betaSandwich-jac-f-sq-wrt-theta-star",
  tol = 1e-6
)
