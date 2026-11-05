## ---- test-betaSandwich-jac-beta-star-sel-wrt-theta-star
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

    p <- 3
    for (std_y in c(TRUE, FALSE)) {
      for (std_x in list(rep(TRUE, p), c(TRUE, FALSE, TRUE), rep(FALSE, p))) {
        testthat::test_that(
          paste(
            text,
            "std_y =",
            std_y,
            "std_x =",
            paste(std_x, collapse = " ")
          ),
          {
            testthat::skip_on_cran()
            Value <- function(theta) {
              betastar <- theta[seq_len(p)]
              sigmay <- theta[p + 1]
              sigmax <- theta[p + 1 + seq_len(p)]
              betastar * sigmay^(1 - std_y) * sigmax^(std_x - 1)
            }
            theta <- c(stats::rnorm(p), 1.5, stats::runif(p, 1, 2))
            jcap <- .JacBetaStarSelWRTThetaStar(
              betastar = theta[seq_len(p)],
              sigmay = theta[p + 1],
              sigmax = theta[p + 1 + seq_len(p)],
              std_y = std_y,
              std_x = std_x
            )
            num <- sapply(
              X = seq_along(theta),
              FUN = function(i) {
                e <- replace(0 * theta, i, 1e-6)
                (Value(theta + e) - Value(theta - e)) / 2e-6
              }
            )
            testthat::expect_true(max(abs(jcap - num)) < tol)
          }
        )
      }
    }
  },
  text = "test-betaSandwich-jac-beta-star-sel-wrt-theta-star",
  tol = 1e-6
)
