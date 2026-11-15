## Submission

This is an update of mlpwr from 1.1.1 to 1.2.0: bug fixes, faster
searches, two changed defaults that make designs reach the target power
(both can be switched back), and an optional `parallel` argument.
See NEWS.md.

## Test environments

* local: Ubuntu 26.04 LTS, R 4.5.2
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R devel,
  release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

pmsims 1.1.0, the only reverse dependency on CRAN, passes `R CMD check`
and its tests with mlpwr 1.2.0.
