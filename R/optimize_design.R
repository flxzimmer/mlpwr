# Optimising the surrogate-based objective over the design space
# ---------------------------------------------------------------------
#
# optimize_design() minimises fn over the box Domains (one row per design
# parameter: lower, upper). It is the single place where mlpwr chooses a
# design from a fitted surrogate (get.pred(), and any later re-optimisation
# of the final design).
#
# optimizer = "genoud": rgenoud::genoud with mlpwr's settings, exactly as in
# mlpwr <= 1.1.1 (the extra arguments in ... are passed on to genoud).
#
# optimizer = "auto" (default), for integer designs:
#   * If the integer grid of the boundaries is small enough that scanning it
#     costs no more than one genoud call (grid points x simulated designs <=
#     exact_scan_budget; the GPR predictor's cost grows with the number of
#     designs), fn is evaluated on every grid point and the best point is
#     returned. Ties are broken by the lowest index in lexicographic order
#     (the first design parameter varies slowest, i.e. the smallest first
#     parameter wins, then the smallest second one, ...), so the choice is
#     deterministic.
#   * Otherwise genoud runs exactly as with "genoud". (A local refinement of
#     genoud's point on larger grids was studied and dropped: it cost
#     1.04-1.2x per search and improved no case.)
# Continuous designs use genoud as with "genoud".
#
# RNG: genoud draws its two seeds from R's RNG through its default arguments
# unif.seed and int.seed (round(runif(1, 1, 2147483647L)) each), i.e. exactly
# two runif() draws per call, whatever fn, the dimension or data.type.int
# (checked with rgenoud 5.9-0.11: .Random.seed after a genoud call equals
# .Random.seed after two runif(1) draws). When the grid search replaces a
# genoud call, it makes the same two draws and discards them, so the main
# random stream (simfun, km, jitter, CV folds) stays aligned with the
# genoud path. Wherever genoud already found the grid optimum (nearly always
# in 1-D), results are therefore bit-identical to optimizer = "genoud".

# budget for screening grid points: a full scan is used if grid points x
# simulated designs <= exact_scan_budget, else genoud is used; and the chunk
# size (number of grid points evaluated at once). 1e5: per call, a scan up to 1e5 took at
# most 0.74x a genoud call (0.83x for a search's first scan) in every bin
# measured, for both tasks and in 2-D and 3-D, on a laptop's efficiency
# cores. Above 1e5 it is not: genoud is about twice as fast with a cost
# threshold (no SD term, fewer evaluations) as with a power target, and a
# grid point costs more in 3-D, so scans at 1e5-3e5 took 1.0-2.3x a genoud
# call there (3e5 was slower per search in MI 2-D and 3-D cells).
# A costfun that cannot be vectorised (see costs_rows()) is evaluated at most
# at exact_scan_pointwise grid points per scan; a scan that would need more
# per-point costfun calls uses genoud instead. Each genoud call makes at least
# about 120 costfun calls (two per objective evaluation, >= 58 evaluations
# measured with pop.size = 20), so such a scan never costs more costfun calls
# than the genoud call it replaces, however slow the costfun.
exact_scan_budget <- 1e5
exact_grid_chunk <- 1e5
exact_scan_pointwise <- 100

optimize_design <- function(fn, Domains, integer, starting.values,
    optimizer = "auto", fn_batch = NULL, n_designs = 1, cache = NULL, ...) {

    auto <- identical(optimizer, "auto") && isTRUE(integer)
    grid <- if (auto) exact_grid_dims(Domains, Inf)
    budget <- exact_scan_budget / max(1, n_designs)
    if (!is.null(grid) && grid$n <= budget) {
        par <- exact_grid_search(fn, Domains, fn_batch = fn_batch,
            max_points = Inf, cache = cache)
        if (!is.null(par)) {
            # consume genoud's two seed draws (see above)
            round(stats::runif(1, 1, 2147483647L))
            round(stats::runif(1, 1, 2147483647L))
            return(par)
        }
    }

    newre <- rgenoud::genoud(fn, nvars = nrow(Domains),
        data.type.int = integer, Domains = Domains, print.level = 0,
        pop.size = 20, starting.values = starting.values, ...)
    newre$par
}


# Integer grid of the box Domains, or NULL if it is empty or has more than
# max_points points.
exact_grid_dims <- function(Domains, max_points = Inf) {
    lo <- ceiling(Domains[, 1])
    hi <- floor(Domains[, 2])
    sizes <- hi - lo + 1
    if (anyNA(sizes) || any(sizes < 1) || prod(sizes) > max_points)
        return(NULL)
    list(lo = unname(lo), sizes = unname(sizes), n = prod(sizes))
}

# Grid points with 0-based lexicographic indices idx (first parameter
# slowest), one point per row. The index arithmetic is done in integers where
# the grid allows it (an order of magnitude faster than on doubles; the
# values are the same whole numbers).
exact_grid_points <- function(grid, idx) {
    d <- length(grid$sizes)
    X <- matrix(0, length(idx), d)
    rest <- idx
    sizes <- grid$sizes
    if (prod(sizes) <= .Machine$integer.max) {
        rest <- as.integer(idx)
        sizes <- as.integer(sizes)
    }
    for (j in d:1) {
        X[, j] <- grid$lo[j] + rest %% sizes[j]
        rest <- rest %/% sizes[j]
    }
    X
}

# The grid points of grid in chunks of chunk points (a list of matrices, as
# exact_grid_points() returns them for consecutive index ranges). With a cache
# environment (one per find.design() run) they are computed once per grid and
# chunk size and reused in later updates instead of being rebuilt every time.
exact_grid_chunks <- function(grid, chunk, cache = NULL) {
    key <- list(lo = grid$lo, sizes = grid$sizes, chunk = chunk)
    if (is.environment(cache) && identical(cache$grid_key, key))
        return(cache$grid_chunks)
    n <- grid$n
    chunks <- lapply(seq(0, n - 1, by = chunk), function(st)
        exact_grid_points(grid, st:min(n - 1, st + chunk - 1)))
    if (is.environment(cache)) {
        cache$grid_key <- key
        cache$grid_chunks <- chunks
    }
    chunks
}

# Exhaustive minimisation of fn over the integer grid of Domains. fn_batch
# (optional) evaluates the objective at once for the grid points in the rows
# of X; idx are their 0-based grid indices, for caching values per grid point
# (see grid_costs()), or NULL. It only screens the grid: see pick_best().
# cache (optional): environment for reusing the grid points across updates
# (see exact_grid_chunks()).
# Returns NULL (caller falls back to genoud) if the grid is empty or has more
# than max_points points, fn_batch returns NULL (costs not available cheaply,
# see grid_costs()) or no grid point has a finite objective.
exact_grid_search <- function(fn, Domains, fn_batch = NULL,
    max_points = Inf, chunk = exact_grid_chunk, cache = NULL) {

    grid <- exact_grid_dims(Domains, max_points)
    if (is.null(grid))
        return(NULL)
    if (is.null(fn_batch))
        fn_batch <- function(X, idx) apply(X, 1, fn)

    n <- grid$n
    vals <- numeric(n)
    chunks <- exact_grid_chunks(grid, chunk, cache)
    for (ci in seq_along(chunks)) {
        st <- (ci - 1) * chunk
        idx <- st:min(n - 1, st + chunk - 1)
        v <- fn_batch(chunks[[ci]], idx)
        if (is.null(v))
            return(NULL)
        vals[idx + 1] <- as.numeric(v)
    }
    pick_best(fn, vals, function(i) exact_grid_points(grid, i - 1))
}

# The best of a set of points given their screened values vals (in
# lexicographic order of the points; points(i) returns the points with
# 1-based positions i as matrix rows). The points within a small tolerance of
# the screened minimum are re-evaluated with fn itself, so the result is the
# minimiser of fn (the function genoud minimises), the first one on ties.
# NULL if no value is finite.
pick_best <- function(fn, vals, points) {
    vals[is.na(vals)] <- Inf
    best <- min(vals)
    if (!is.finite(best))
        return(NULL)

    # candidates: within a relative tolerance of the screened minimum (the
    # batch predictors agree with the pointwise ones up to rounding)
    tol <- 1e-8 * max(1, abs(best))
    cand <- which(vals <= best + tol)
    if (length(cand) > 1000) {
        # flat objective: decide on the screened values alone
        cand <- cand[vals[cand] == best]
        return(as.numeric(points(cand[1])))
    }
    X <- points(cand)
    pv <- vapply(seq_along(cand), function(i) as.numeric(fn(X[i, ]))[1],
        numeric(1))
    if (all(is.na(pv)))
        return(as.numeric(X[1, ]))
    # which.min returns the first minimum, i.e. the lowest index
    as.numeric(X[which.min(pv), ])
}


# Surrogate predictions for the rows of a numeric matrix (no column names
# needed), for screening the grid. Uses the cached km predictor or the direct
# lm/glm predictor where the fit has one, and fitfun row by row otherwise.
batch_mean <- function(fitfun, X) {
    env <- environment(fitfun)
    if (is.environment(env)) {
        kmc <- get0("kmc", envir = env, inherits = FALSE)
        if (is.list(kmc) && is.function(kmc$mean_screen) && ncol(X) == kmc$d)
            return(kmc$mean_screen(X))
        linp <- get0("linp", envir = env, inherits = FALSE)
        if (is.list(linp) && is.function(linp$predict_batch))
            return(linp$predict_batch(X))
        # svr fits and km fits without a cached predictor: one predict()
        # call for all rows (fitfun names the columns as in xvars)
        mod <- get0("mod", envir = env, inherits = FALSE)
        xvars <- get0("xvars", envir = env, inherits = FALSE)
        if (is.data.frame(xvars) && ncol(xvars) == ncol(X)) {
            newdata <- as.data.frame(X)
            names(newdata) <- names(xvars)
            if (inherits(mod, "wsvm"))
                return(as.numeric(stats::predict(mod, newdata = newdata)))
            if (inherits(mod, "km"))
                return(DiceKriging::predict.km(mod, newdata = newdata,
                    type = "UK")$mean)
        }
    }
    apply(X, 1, fitfun)
}

# Surrogate predictions for the rows of a data frame or matrix (e.g. the
# grid of the 2-D heat plot); the same values as apply(rows, 1, fitfun) up to
# rounding.
predict_rows <- function(fitfun, rows) {
    X <- as.matrix(rows)
    storage.mode(X) <- "double"
    dimnames(X) <- NULL
    as.numeric(batch_mean(fitfun, X))
}

batch_meansd <- function(fitfun, fitfun.sd, X) {
    env <- environment(fitfun)
    kmc <- if (is.environment(env))
        get0("kmc", envir = env, inherits = FALSE)
    # fitfun and fitfun.sd of one gauss.fit share the cached predictor
    if (is.list(kmc) && is.function(kmc$meansd_batch) && ncol(X) == kmc$d &&
        identical(environment(fitfun.sd), env))
        return(kmc$meansd_batch(X))
    mod <- if (is.environment(env))
        get0("mod", envir = env, inherits = FALSE)
    xvars <- if (is.environment(env))
        get0("xvars", envir = env, inherits = FALSE)
    if (inherits(mod, "km") && is.data.frame(xvars) && ncol(xvars) == ncol(X) &&
        identical(environment(fitfun.sd), env)) {
        newdata <- as.data.frame(X)
        names(newdata) <- names(xvars)
        p <- DiceKriging::predict.km(mod, newdata = newdata, type = "UK")
        return(list(mean = p$mean, sd = p$sd))
    }
    list(mean = batch_mean(fitfun, X), sd = apply(X, 1, fitfun.sd))
}


# costfun at grid points X (0-based grid indices idx). The costs do not depend
# on the surrogate, so with a cache environment (one per find.design() run)
# each grid point's cost is computed only once per search instead of in every
# update. costfun must be deterministic, which mlpwr assumes anyway. With
# idx = NULL nothing is cached. NULL if the costs are not vectorised and more
# than exact_scan_pointwise of them are missing (the scan then uses genoud);
# with a cache this sets cache$no_scan, so that later updates of the search
# use genoud directly (see scan_refused()).
grid_costs <- function(costfun, X, idx, Domains, cache = NULL) {
    if (!is.environment(cache) || is.null(idx))
        return(costs_rows(costfun, X))
    if (!identical(cache$Domains, Domains)) {
        grid <- exact_grid_dims(Domains, Inf)
        cache$Domains <- Domains
        cache$costs <- numeric(grid$n)
        cache$done <- logical(grid$n)
        cache$no_scan <- FALSE
    }
    miss <- which(!cache$done[idx + 1])
    if (length(miss)) {
        v <- costs_rows(costfun, X[miss, , drop = FALSE], cache)
        if (is.null(v)) {
            cache$no_scan <- TRUE
            return(NULL)
        }
        cache$costs[idx[miss] + 1] <- v
        cache$done[idx[miss] + 1] <- TRUE
    }
    cache$costs[idx + 1]
}

# TRUE if an earlier update of this search found that the grid's costs
# cannot be computed cheaply (see grid_costs()); the scan is then skipped.
scan_refused <- function(cache, Domains) {
    is.environment(cache) && isTRUE(cache$no_scan) &&
        identical(cache$Domains, Domains)
}

# costfun at the rows of X, exactly apply(X, 1, costfun). If costfun carries a
# vectorised form (attribute "vectorised", see fix.argtype() and the default
# costfun in find.design()), that is tried first in one call over all rows.
# Its result is used only if the call gives no error, warning, message, output
# or change of the random number state, returns one plain numeric value per
# row and reproduces the per-point values identically on a fixed subset of
# rows (first, last and evenly spaced ones). Otherwise the per-point values
# are computed one row at a time if there are at most exact_scan_pointwise
# rows, else NULL; with a cache environment the vectorised form is not tried
# again in this search. (Costfuns with if (n > 10), max(n, k), sum(n, k),
# sum(x) etc. fail these checks.)
costs_rows <- function(costfun, X, cache = NULL) {
    vec <- attr(costfun, "vectorised", exact = TRUE)
    m <- nrow(X)
    if (is.function(vec) && m > 1 &&
        !(is.environment(cache) && isFALSE(cache$vectorised))) {
        v <- try_vectorised(costfun, vec, X)
        if (is.environment(cache))
            cache$vectorised <- !is.null(v)
        if (!is.null(v))
            return(v)
    }
    if (m > exact_scan_pointwise)
        return(NULL)
    apply(X, 1, costfun)
}

try_vectorised <- function(costfun, vec, X) {
    m <- nrow(X)
    seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    chk <- unique(round(seq(1, m, length.out = min(m, 12))))
    res <- tryCatch({
        out <- utils::capture.output({
            v <- vec(X)
            ok <- is.numeric(v) && is.null(dim(v)) && length(v) == m &&
                !anyNA(v)
            if (ok) {
                v <- as.vector(v)
                ok <- identical(v[chk], apply(X[chk, , drop = FALSE], 1,
                    costfun))
            }
        })
        if (ok && !length(out)) v
    }, error = function(e) NULL, warning = function(w) NULL,
        message = function(m) NULL)
    if (!identical(get0(".Random.seed", envir = globalenv(),
        inherits = FALSE), seed)) {
        # restore the random number stream (fallback = per-point path)
        if (is.null(seed)) {
            rm(".Random.seed", envir = globalenv())
        } else {
            assign(".Random.seed", seed, envir = globalenv())
        }
        res <- NULL
    }
    res
}
