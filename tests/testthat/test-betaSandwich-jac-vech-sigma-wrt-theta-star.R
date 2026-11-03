## ---- test-betaSandwich-jac-vech-sigma-wrt-theta-star
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

    VechSigma <- function(theta,
                          p) {
      betastar <- theta[seq_len(p)]
      sigmay <- theta[p + 1]
      sigmax <- theta[p + 1 + seq_len(p)]
      rhocapx <- diag(p)
      if (p > 1) {
        idx <- 2 * p + 1 + seq_len(p * (p - 1) / 2)
        rhocapx[lower.tri(rhocapx)] <- theta[idx]
        rhocapx[upper.tri(rhocapx)] <- t(rhocapx)[upper.tri(rhocapx)]
      }
      rhoyx <- drop(rhocapx %*% betastar)
      rhocap <- rbind(
        c(1, rhoyx),
        cbind(rhoyx, rhocapx)
      )
      sigmacap <- diag(c(sigmay, sigmax)) %*% rhocap %*% diag(c(sigmay, sigmax))
      sigmacap[lower.tri(sigmacap, diag = TRUE)]
    }
    for (p in c(1, 2, 4)) {
      testthat::test_that(
        paste(text, "p =", p),
        {
          testthat::skip_on_cran()
          a <- matrix(data = stats::rnorm(p * p), nrow = p)
          rhocapx <- stats::cov2cor(crossprod(a) + diag(p))
          betastar <- stats::rnorm(p) * 0.3
          sigmay <- 1.3
          sigmax <- stats::runif(p, 1, 2)
          theta <- c(betastar, sigmay, sigmax, rhocapx[lower.tri(rhocapx)])
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
            X = seq_along(theta),
            FUN = function(i) {
              e <- replace(0 * theta, i, 1e-6)
              (VechSigma(theta + e, p) - VechSigma(theta - e, p)) / 2e-6
            }
          )
          testthat::expect_true(max(abs(unname(jcap) - num)) < tol)
        }
      )
    }
  },
  text = "test-betaSandwich-jac-vech-sigma-wrt-theta-star",
  tol = 1e-6
)
