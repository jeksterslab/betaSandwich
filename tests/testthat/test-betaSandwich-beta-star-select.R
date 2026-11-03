## ---- test-betaSandwich-beta-star-select
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
      paste(text, "vector and matrix inputs agree"),
      {
        testthat::skip_on_cran()
        p <- 3
        draws <- matrix(data = stats::rnorm(5 * p), nrow = 5)
        # one set of standard deviations per draw, as in
        # .BetaStarSelectofVechSigmaMat()
        sigmay <- stats::runif(5, 1, 2)
        sigmax <- matrix(data = stats::runif(5 * p, 0.5, 3), nrow = 5)
        for (std_y in c(TRUE, FALSE)) {
          for (std_x in list(rep(TRUE, p), c(TRUE, FALSE, TRUE))) {
            mat <- .BetaStarSelect(
              betastar = draws,
              sigmay = sigmay,
              sigmax = sigmax,
              std_y = std_y,
              std_x = std_x
            )
            vec <- t(
              sapply(
                X = seq_len(nrow(draws)),
                FUN = function(r) {
                  .BetaStarSelect(
                    betastar = draws[r, ],
                    sigmay = sigmay[r],
                    sigmax = sigmax[r, ],
                    std_y = std_y,
                    std_x = std_x
                  )
                }
              )
            )
            testthat::expect_equal(mat, vec)
            testthat::expect_equal(
              vec[1, ],
              draws[1, ] * sigmay[1]^(1 - std_y) * sigmax[1, ]^(std_x - 1)
            )
          }
        }
      }
    )
  },
  text = "test-betaSandwich-beta-star-select"
)
