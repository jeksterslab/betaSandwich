# Changelog

## betaSandwich 1.0.9.9001

- Add SCorSqSandwich() for the image of a signed-semipartial interval
  under squaring.

## betaSandwich 1.0.9.9000

### Major

- Added
  [`SCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/SCorSandwich.md),
  [`PCorSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/PCorSandwich.md),
  [`DeltaRSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DeltaRSqSandwich.md),
  and
  [`FSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/FSqSandwich.md):
  delta-method intervals for semipartial correlations, squared partial
  correlations, the improvement in R-squared, and Cohen’s f-squared
  (global and local), with normal-theory, ADF, or HC standard errors, as
  in betaMC and betaNB.
- `RSqBetaSandwich()` is removed. Use
  [`RSqSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/RSqSandwich.md),
  which gives the same estimates and standard errors; its intervals and
  tests use the normal distribution, as the other functions do, and its
  estimates are named `rsq` and `adj`.
- The new functions and
  [`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md)
  return objects of class `essandwich` with one set of methods.
  [`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md)
  objects also keep the class `diffbetasandwich`.

### Minor

- Selective standardization: the arguments `std_y` and `std_x`
  standardize only y or only the named regressors (for example, leave a
  dummy variable in its 0-1 units). The defaults give the same results
  as before.
- [`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md)
  stops when the slopes are not fully standardized.

### Patch

- Fixed the standard error of R-squared in `RSqBetaSandwich()`, which
  was about three times too small for every covariance type. With
  R-squared as a parameter, the variance of y is beta’ Sigma_X beta /
  R-squared, so its derivatives with respect to beta and Sigma_X are now
  divided by R-squared.

## betaSandwich 1.0.9

CRAN release: 2026-06-11

### Patch

- Minor edits to methods.

## betaSandwich 1.0.8

CRAN release: 2025-10-19

### Patch

- Minor edits to methods.

## betaSandwich 1.0.7

CRAN release: 2024-04-14

### Patch

- Minor documentation edits.

## betaSandwich 1.0.6

CRAN release: 2023-10-14

### Patch

- Minor documentation edits.

## betaSandwich 1.0.5

CRAN release: 2023-07-08

### Patch

- Added degrees of freedom to the table of results.

## betaSandwich 1.0.4

CRAN release: 2023-04-25

### Patch

- Added the
  [`DiffBetaSandwich()`](https://jeksterslab.github.io/betaSandwich/reference/DiffBetaSandwich.md)
  and the `RSqBetaSandwich()` functions.

## betaSandwich 1.0.3

CRAN release: 2022-12-17

### Patch

- Added the
  [`BetaADF()`](https://jeksterslab.github.io/betaSandwich/reference/BetaADF.md)
  function.

## betaSandwich 1.0.2

CRAN release: 2022-10-07

### Patch

- Factor variables are dummy coded.

## betaSandwich 1.0.1

CRAN release: 2022-09-20

### Patch

- Added the `nas1982` data set.
- Minor edits to methods.

## betaSandwich 1.0.0

### Major

- And so it begins.
