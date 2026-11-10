## ---- test-betaSandwich-jac-p-cor-sq-wrt-theta-star
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    # central differences
    Num <- function(fun,
                    theta,
                    h = 1e-6) {
      out <- sapply(
        X = seq_along(along.with = theta),
        FUN = function(i) {
          e <- replace(x = 0 * theta, list = i, values = h)
          (fun(x = theta + e) - fun(x = theta - e)) / (2 * h)
        }
      )
      if (is.null(x = dim(x = out))) {
        out <- matrix(data = out, nrow = 1)
      }
      out
    }
    Rho <- function(theta,
                    p) {
      rhocapx <- diag(x = p)
      if (p > 1) {
        idx <- 2 * p + 1 + seq_len(length.out = p * (p - 1) / 2)
        rhocapx[lower.tri(x = rhocapx)] <- theta[idx]
        rhocapx[upper.tri(
          x = rhocapx
        )] <- t(x = rhocapx)[upper.tri(x = rhocapx)]
      }
      rhocapx
    }
    Value <- function(theta,
                      p,
                      what) {
      betastar <- theta[seq_len(length.out = p)]
      rhocapx <- Rho(theta = theta, p = p)
      rsq <- drop(x = crossprod(x = betastar, y = rhocapx %*% betastar))
      scor <- betastar / sqrt(x = diag(x = solve(a = rhocapx)))
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
      pcor = .JacPCorSqWRTThetaStar
    )
    for (p in c(1, 2, 5)) {
      a <- matrix(data = stats::rnorm(n = p * p), nrow = p)
      rhocapx <- stats::cov2cor(V = crossprod(x = a) + diag(x = p))
      betastar <- stats::rnorm(n = p) * 0.3
      while (drop(
        x = crossprod(x = betastar, y = rhocapx %*% betastar)
      ) > 0.8) {
        betastar <- betastar / 2
      }
      theta <- c(
        betastar,
        1.3,
        stats::runif(n = p, min = 1, max = 2),
        rhocapx[lower.tri(x = rhocapx)]
      )
      for (what in names(x = funs)) {
        testthat::test_that(
          desc = paste(text, what, "p =", p),
          code = {
            jcap <- funs[[what]](
              betastar = betastar,
              rhocapx = rhocapx
            )
            testthat::expect_identical(
              object = dim(x = jcap),
              expected = c(
                length(x = Value(theta = theta, p = p, what = what)),
                length(x = theta)
              )
            )
            testthat::expect_true(
              object = max(
                abs(
                  x = jcap - Num(
                    fun = function(x) {
                      Value(theta = x, p = p, what = what)
                    },
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
  text = "test-betaSandwich-jac-p-cor-sq-wrt-theta-star",
  tol = 1e-6
)
