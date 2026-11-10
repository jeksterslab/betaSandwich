## ---- test-betaSandwich-jac-vech-sigma-wrt-theta-star
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    VechSigma <- function(theta,
                          p) {
      betastar <- theta[seq_len(length.out = p)]
      sigmay <- theta[p + 1]
      sigmax <- theta[p + 1 + seq_len(length.out = p)]
      rhocapx <- diag(x = p)
      if (p > 1) {
        idx <- 2 * p + 1 + seq_len(length.out = p * (p - 1) / 2)
        rhocapx[lower.tri(x = rhocapx)] <- theta[idx]
        rhocapx[upper.tri(
          x = rhocapx
        )] <- t(x = rhocapx)[upper.tri(x = rhocapx)]
      }
      rhoyx <- drop(x = rhocapx %*% betastar)
      rhocap <- rbind(
        c(1, rhoyx),
        cbind(rhoyx, rhocapx)
      )
      sigmacap <- diag(
        x = c(sigmay, sigmax)
      ) %*% rhocap %*% diag(x = c(sigmay, sigmax))
      sigmacap[lower.tri(x = sigmacap, diag = TRUE)]
    }
    for (p in c(1, 2, 4)) {
      testthat::test_that(
        desc = paste(text, "p =", p),
        code = {
          a <- matrix(data = stats::rnorm(n = p * p), nrow = p)
          rhocapx <- stats::cov2cor(V = crossprod(x = a) + diag(x = p))
          betastar <- stats::rnorm(n = p) * 0.3
          sigmay <- 1.3
          sigmax <- stats::runif(n = p, min = 1, max = 2)
          theta <- c(betastar, sigmay, sigmax, rhocapx[lower.tri(x = rhocapx)])
          q <- p + 1 + p * (p + 1) / 2
          jcap <- .JacVechSigmaWRTThetaStar(
            betastar = betastar,
            sigmay = sigmay,
            sigmax = sigmax,
            rhocapx = rhocapx,
            q = q,
            p = p
          )
          num <- sapply(
            X = seq_along(along.with = theta),
            FUN = function(i) {
              e <- replace(x = 0 * theta, list = i, values = 1e-6)
              (VechSigma(
                theta = theta + e,
                p = p
              ) - VechSigma(theta = theta - e, p = p)) / 2e-6
            }
          )
          testthat::expect_true(
            object = max(abs(x = unname(obj = jcap) - num)) < tol
          )
        }
      )
    }
  },
  text = "test-betaSandwich-jac-vech-sigma-wrt-theta-star",
  tol = 1e-6
)
