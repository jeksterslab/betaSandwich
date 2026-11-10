## ---- test-betaSandwich-jac-beta-star-sel-wrt-theta-star
lapply(
  X = 1,
  FUN = function(i,
                 text,
                 tol) {
    testthat::skip_on_cran()
    message(text)

    set.seed(seed = 42)

    p <- 3
    for (std_y in c(TRUE, FALSE)) {
      for (std_x in list(
        rep(x = TRUE, p),
        c(TRUE, FALSE, TRUE),
        rep(x = FALSE, p)
      )) {
        testthat::test_that(
          desc = paste(
            text,
            "std_y =",
            std_y,
            "std_x =",
            paste(std_x, collapse = " ")
          ),
          code = {
            Value <- function(theta) {
              betastar <- theta[seq_len(length.out = p)]
              sigmay <- theta[p + 1]
              sigmax <- theta[p + 1 + seq_len(length.out = p)]
              betastar * sigmay^(1 - std_y) * sigmax^(std_x - 1)
            }
            theta <- c(
              stats::rnorm(n = p),
              1.5,
              stats::runif(n = p, min = 1, max = 2)
            )
            jcap <- .JacBetaStarSelWRTThetaStar(
              betastar = theta[seq_len(length.out = p)],
              sigmay = theta[p + 1],
              sigmax = theta[p + 1 + seq_len(length.out = p)],
              std_y = std_y,
              std_x = std_x
            )
            num <- sapply(
              X = seq_along(along.with = theta),
              FUN = function(i) {
                e <- replace(x = 0 * theta, list = i, values = 1e-6)
                (Value(theta = theta + e) - Value(theta = theta - e)) / 2e-6
              }
            )
            testthat::expect_true(object = max(abs(x = jcap - num)) < tol)
          }
        )
      }
    }
  },
  text = "test-betaSandwich-jac-beta-star-sel-wrt-theta-star",
  tol = 1e-6
)
