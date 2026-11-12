# Squared Signed-Semipartial Confidence Intervals

Square the signed interval as a set, with lower limit zero if it
contains zero. This differs from an interval of squared draws. No draws
are generated and no endpoints are clipped to one. Failed signed
endpoints remain missing.

## Usage

``` r
SCorSqSandwich(object, level = 0.95)
```

## Arguments

- object:

  Context accepted by
  [`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md).

- level:

  Single confidence level in (0, 1).

## Value

Data frame with est, lower and upper columns and predictor row names.
The point estimate is the squared signed estimate. No standard error is
implied by this interval transformation.

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
fit <- stats::lm(formula = mpg ~ wt + hp, data = mtcars)
out <- BetaHC(object = fit)
SCorSqSandwich(object = out)
#>           est      lower     upper
#> wt 0.22434811 0.07255841 0.4596044
#> hp 0.07395266 0.01974972 0.1626921
```
