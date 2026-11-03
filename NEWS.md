# betaSandwich 1.0.9.9000

## Major

* Added `SCorSandwich()`, `PCorSandwich()`, `DeltaRSqSandwich()`, and
  `FSqSandwich()`: delta-method intervals for semipartial correlations,
  squared partial correlations, the improvement in R-squared, and Cohen's
  f-squared (global and local), with normal-theory, ADF, or HC standard
  errors, as in betaMC and betaNB.
* `RSqBetaSandwich()` is removed. Use `RSqSandwich()`, which gives the same
  estimates and standard errors; its intervals and tests use the normal
  distribution, as the other functions do, and its estimates are named
  `rsq` and `adj`.
* The new functions and `DiffBetaSandwich()` return objects of class
  `essandwich` with one set of methods. `DiffBetaSandwich()` objects also
  keep the class `diffbetasandwich`.

## Minor

* Selective standardization: the arguments `std_y` and `std_x` standardize
  only y or only the named regressors (for example, leave a dummy
  variable in its 0-1 units). The defaults give the same results as
  before.
* `DiffBetaSandwich()` stops when the slopes are not fully standardized.

## Patch

* Fixed the standard error of R-squared in `RSqBetaSandwich()`, which was
  about three times too small for every covariance type. With R-squared
  as a parameter, the variance of y is beta' Sigma_X beta / R-squared, so
  its derivatives with respect to beta and Sigma_X are now divided by
  R-squared.

# betaSandwich 1.0.9

## Patch

* Minor edits to methods.

# betaSandwich 1.0.8

## Patch

* Minor edits to methods.

# betaSandwich 1.0.7

## Patch

* Minor documentation edits.

# betaSandwich 1.0.6

## Patch

* Minor documentation edits.

# betaSandwich 1.0.5

## Patch

* Added degrees of freedom to the table of results.

# betaSandwich 1.0.4

## Patch

* Added the `DiffBetaSandwich()` and the `RSqBetaSandwich()` functions.

# betaSandwich 1.0.3

## Patch

* Added the `BetaADF()` function.

# betaSandwich 1.0.2

## Patch

* Factor variables are dummy coded.

# betaSandwich 1.0.1

## Patch

* Added the `nas1982` data set.
* Minor edits to methods.

# betaSandwich 1.0.0

## Major

* And so it begins.
