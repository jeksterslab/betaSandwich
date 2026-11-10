## ---- test-betaSandwich-beta-star-select
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    testthat::test_that(
      desc = paste(text, "vector and matrix inputs agree"),
      code = {
        p <- 3
        draws <- matrix(data = stats::rnorm(n = 5 * p), nrow = 5)
        # one set of standard deviations per draw, as in
        # the matrix standardization helper.
        sigmay <- stats::runif(n = 5, min = 1, max = 2)
        sigmax <- matrix(
          data = stats::runif(n = 5 * p, min = 0.5, max = 3),
          nrow = 5
        )
        for (std_y in c(TRUE, FALSE)) {
          for (std_x in list(rep(x = TRUE, p), c(TRUE, FALSE, TRUE))) {
            mat <- .BetaStarSelect(
              betastar = draws,
              sigmay = sigmay,
              sigmax = sigmax,
              std_y = std_y,
              std_x = std_x
            )
            vec <- t(
              x = sapply(
                X = seq_len(length.out = nrow(x = draws)),
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
            testthat::expect_equal(object = mat, expected = vec)
            testthat::expect_equal(
              object = vec[1, ],
              expected = draws[1, ] * sigmay[1]^(1 - std_y) * sigmax[
                1,
              ]^(std_x - 1)
            )
          }
        }
      }
    )
  },
  text = "test-betaSandwich-beta-star-select"
)
