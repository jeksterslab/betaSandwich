## Release summary

This is a minor update (1.0.9 -> 1.1.0).

- New functions: `SCorSandwich()`, `PCorSandwich()`, `DeltaRSqSandwich()`, `FSqSandwich()`, and `SCorSqSandwich()`.
- Selective standardization through `std_y` and `std_x`.
- The standard error of R-squared in `RSqBetaSandwich()` was about three times too small; the fix is in `RSqSandwich()`.
- `RSqBetaSandwich()` is removed. Its replacement, `RSqSandwich()`, gives the same estimates and standard errors.
  The reverse dependencies that Suggest betaSandwich (betaDelta, betaMC, betaNB) do not use `RSqBetaSandwich()`.

## Test environments

- Linux (x86_64, Apptainer image), R 4.6.2: `R CMD check --as-cran`, Status: OK.
- win-builder (R-devel): TODO (the maintainer runs this before submission).
- R-hub: TODO (the maintainer runs this before submission).

## R CMD check results

0 errors | 0 warnings | 0 notes (Linux, R 4.6.2; test coverage 98.43%). The win-builder and R-hub results go here.

## Reverse dependencies

betaDelta, betaMC, and betaNB Suggest betaSandwich. None calls `RSqBetaSandwich()`.
