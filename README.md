
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlpwr

<!-- badges: start -->
<!-- badges: end -->

Implements a surrogate modeling algorithm to guide simulation-based
sample size planning. The method is described in detail in a recent
preprint (Zimmer & Debelak, 2022,
<http://dx.doi.org/10.31234/osf.io/tnhb2>). Supports multiple study
design parameters and optimization with respect to a cost function. Can
find optimal designs that correspond to a desired statistical power or
designs that fullfill a cost constraint.

## Installation

You can install the CRAN version using:

``` r
# install.packages("mlpwr")
```

Or, you can install the development version of mlpwr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("flxzimmer/mlpwr")
```

## Toy Example

This is a basic demonstration to the mlpwr pacakge by going through a
toy example.

The package can be loaded via

``` r
library(mlpwr)
```

### Setting up a simulation function (simfun)

A simulation function (simfun) is a function to generate artificial data
and subsequently perform an hypothesis test.

A simple function simulates a group mean comparison with a t-test. The
input is a sample size n and the output is either TRUE or FALSE
depending on the significance of the hypothesis test.

``` r
simfun_ttest = function(N) {
    # Generate a data set
    dat = rnorm(n = N, mean = .3) 
    # Test the hypothesis
    res = t.test(dat)
    res$p.value < .01
}
```

We can test the data generating function as follows for the sample sizes
30 and 400.

``` r
simfun_ttest(30)
#> [1] FALSE
simfun_ttest(400)
#> [1] TRUE
```

### Performing the search for design parameters

The find.design functions can be used to find an appropriate sample size
given a power level that should be surpassed.

The central arguments to be specified are the following:

-   simfun: The data generating function as defined above

-   boundaries: The lower and upper bound to search within, e.g. a
    sample size between 50 and 200.

-   power: The desired power of the design.

Internally, the function performs an initialization phase, in which it
evaluates the simfun at some random initial values and the boundaries.
After that, it performs the following steps in a loop:

-   Fit: A surrogate model is fitted to the relationship of sample size
    and power.

-   Predict: A candidate for a good sample size is determined.

-   Update: The simfun is evaluated at the predicted sample size.

Once a termination criterion is met (e.g. the number of permissible
simfun evaluation specified with evaluations), the algorithm is
terminated.

We can perform the search with the above arguments in use.

``` r
ds = find.design(simfun = simfun_ttest,
    boundaries = c(100,300), power = .95)
#>  Updates: 1, Evaluations: 1000, Time: 0.7 Updates: 2, Evaluations: 1200, Time: 0.9 Updates: 3, Evaluations: 1400, Time: 1 Updates: 4, Evaluations: 1600, Time: 1.1 Updates: 5, Evaluations: 1800, Time: 1.2 Updates: 6, Evaluations: 2000, Time: 1.4 Updates: 7, Evaluations: 2200, Time: 1.5 Updates: 8, Evaluations: 2400, Time: 1.7 Updates: 9, Evaluations: 2600, Time: 1.8 Updates: 10, Evaluations: 2800, Time: 1.9 Updates: 11, Evaluations: 3000, Time: 2.1 Updates: 12, Evaluations: 3200, Time: 2.2 Updates: 13, Evaluations: 3400, Time: 2.4 Updates: 14, Evaluations: 3600, Time: 2.5 Updates: 15, Evaluations: 3800, Time: 2.6 Updates: 16, Evaluations: 4000, Time: 2.7
```

While it is running, the function gives us some updates regarding the
number of updates performed, the time used, and the the number of
evaluations.

We can get an overview of the results via summary.

``` r
summary(ds)
#> 
#> Call:
#> find.design(simfun = simfun_ttest, boundaries = c(100, 300), 
#>     power = 0.95)
#> 
#> Design: N = 201
#> 
#> Power: 0.95064,  SE: 0.00379
#> Evaluations: 4000,  Time: 3.05,  Updates: 16
#> Surrogate: Logistic regression
```

The results indicate that a sample size of 201 is suitable.

Also, we can plot the fitted relationship between sample size and power.
The black dots show us the simulated data. The gray ribbon indicates the
uncertainty of the power at the respective sample sizes.

``` r
plot(ds)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
