# Opt-in parallel evaluation of the simulation function (find.design(parallel=)).
#
# Only the simfun evaluations of a batch (all design points x replicates of one
# addval() call) run in parallel. Everything else (surrogate fits, genoud,
# termination checks, print_progress) stays in the main process.
#
# RNG: every simfun evaluation gets its own L'Ecuyer-CMRG stream. The streams
# of a batch are derived from one draw of the main RNG (make_streams()), and the
# main RNG state is restored after future_lapply(), so the main stream (genoud,
# km, jitter) advances by exactly one sample.int() draw per batch. Neither the
# streams nor the main stream depend on the number of workers or on how the
# evaluations are split into chunks.

# Packages needed for parallel = TRUE / an integer (all in Suggests).
parallel_pkgs <- c("future", "future.apply", "parallelly")

# Separate function so that tests can mock a missing package.
pkg_available <- function(pkg) requireNamespace(pkg, quietly = TRUE)

check_parallel_arg <- function(parallel) {
    ok <- (is.logical(parallel) && length(parallel) == 1 && !is.na(parallel)) ||
        (is.numeric(parallel) && length(parallel) == 1 && !is.na(parallel) &&
            parallel >= 1 && parallel == round(parallel))
    if (!ok)
        stop("parallel must be FALSE, TRUE or a positive whole number (the number of workers)")
    invisible(TRUE)
}

check_parallel_pkgs <- function() {
    missing <- parallel_pkgs[!vapply(parallel_pkgs, pkg_available, logical(1))]
    if (length(missing) > 0)
        stop("Parallel computation (the parallel argument of find.design) needs the package",
            if (length(missing) > 1) "s" else "", " ", paste(missing, collapse = ", "),
            ". Install ", if (length(missing) > 1) "them" else "it", " with:\n  install.packages(",
            if (length(missing) > 1) "c(" else "",
            paste0("\"", missing, "\"", collapse = ", "),
            if (length(missing) > 1) ")" else "", ")", call. = FALSE)
    invisible(TRUE)
}

# Number of cores for parallel = TRUE (separate function so that tests can mock it).
n_cores <- function() max(1L, as.integer(parallelly::availableCores(omit = 1)))

# Set up the workers. Returns list(par = <settings for addval>, restore = <function
# that restores the user's plan>), or NULL for parallel = FALSE or 1. An explicit
# 1 means "one core", i.e. the ordinary sequential mode (same as FALSE).
# parallel = TRUE on a machine
# with only one usable core still takes the stream path, so its results match
# runs on bigger machines.
parallel_setup <- function(parallel, dnames, silent = FALSE) {
    if (isFALSE(parallel) || (is.numeric(parallel) && parallel == 1))
        return(NULL)
    check_parallel_pkgs()

    restore <- function() invisible(NULL)
    userplan <- isTRUE(parallel) && !inherits(future::plan(), "sequential")
    if (userplan) {
        # advanced route: the user's own future::plan() is used as is
        nworkers <- future::nbrOfWorkers()
    } else {
        nworkers <- if (isTRUE(parallel)) n_cores() else as.integer(parallel)
        oplan <- if (nworkers == 1L)
            future::plan(future::sequential) else future::plan(future::multisession,
            workers = nworkers)
        restore <- function() future::plan(oplan)
    }

    if (!silent)
        message("Running simulations on ", nworkers, " parallel worker",
            if (nworkers != 1) "s", if (userplan) " (using your future::plan())")

    list(par = list(nworkers = nworkers, dnames = dnames), restore = restore)
}

# n independent L'Ecuyer-CMRG streams. Uses one sample.int() draw of the main
# RNG; the RNG kind of the main session is left unchanged.
make_streams <- function(n) {
    s0 <- sample.int(.Machine$integer.max, 1L)
    oseed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", oseed, envir = globalenv()))
    RNGkind("L'Ecuyer-CMRG")
    set.seed(s0)
    s <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    out <- vector("list", n)
    for (k in seq_len(n)) {
        s <- parallel::nextRNGStream(s)
        out[[k]] <- s
    }
    out
}

# fix.argtype() wraps a simfun with separate design arguments, function(n, k),
# as function(x) do.call(fun, as.list(x)). future only finds the globals and
# packages that the user's function needs if it gets that function itself, so
# the wrapper is undone here and redone in the worker (par_worker()).
# When mlpwr is installed with srcrefs (R_KEEP_PKG_SOURCE=yes, as on GitHub
# Actions, or devtools::load_all()), the body of the wrapper (and the quoted
# template below) carry srcref attributes; they are dropped before the
# comparison. Otherwise the wrapper is shipped as is and future misses the
# globals of the user's function (e.g. a helper function defined in the global
# environment).
unwrap_simfun <- function(simfun) {
    e <- environment(simfun)
    nosrc <- function(b) {
        attributes(b) <- NULL
        b
    }
    if (!is.null(e) && exists("fun", envir = e, inherits = FALSE) &&
        identical(nosrc(body(simfun)), nosrc(quote({
            do.call(fun, as.list(x))
        }))))
        return(list(fun = get("fun", envir = e, inherits = FALSE), islist = TRUE))
    list(fun = simfun, islist = FALSE)
}

# Runs in the worker: one simfun evaluation, output and conditions silenced as
# in hush()/addval(); errors are returned as values.
par_worker <- function(i, fun, islist, xrows) {
    tryCatch(list(ok = TRUE, value = suppressWarnings(suppressMessages({
        x <- as.numeric(xrows[i, ])
        if (islist) do.call(fun, as.list(x)) else fun(x)
    }))), error = function(e) list(ok = FALSE, message = conditionMessage(e)))
}

# Evaluate simfun once for every entry of rows (row indices into xrows), each
# with its own RNG stream. Returns the values as a list in the order of rows.
par_eval <- function(simfun, xrows, rows, par) {
    n <- length(rows)
    if (n == 0)
        return(list())
    streams <- make_streams(n)

    # spread the evaluations of each design point over the workers (results
    # don't depend on this: every evaluation keeps its own stream)
    nw <- par$nworkers
    if (!is.finite(nw) || nw < 1) nw <- n
    perm <- order((seq_len(n) - 1L) %% min(nw, n))

    # future.apply moves the main RNG (by one draw per call in 1.21.0); keep
    # the main stream a function of the seed only
    oseed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    sf <- unwrap_simfun(simfun)
    res <- future.apply::future_lapply(rows[perm], par_worker, fun = sf$fun,
        islist = sf$islist, xrows = xrows, future.seed = streams[perm], future.stdout = FALSE,
        future.scheduling = 1)
    assign(".Random.seed", oseed, envir = globalenv())
    res <- res[order(perm)]

    failed <- which(!vapply(res, function(r) isTRUE(r$ok), logical(1)))
    if (length(failed) > 0) {
        r1 <- failed[1]
        x <- xrows[rows[r1], ]
        nm <- par$dnames
        design <- if (!is.null(nm) && length(nm) == length(x) && all(nzchar(nm)))
            paste(nm, x, sep = " = ", collapse = ", ") else paste(x, collapse = ", ")
        stop("simfun failed at design ", design, " (", length(failed), " of ", n,
            " parallel evaluations failed): ", res[[r1]]$message, call. = FALSE)
    }
    lapply(res, `[[`, "value")
}

# Parallel version of the evaluation loop in addval(): same bookkeeping, but
# all evaluations of the batch are done in one parallel call.
addval_parallel <- function(simfun, dat, points, each, minrun, autosave_dir, par) {

    xvalues <- sapply(dat, function(y) digest::digest(as.numeric(y$x)))
    xrows <- do.call(rbind, lapply(seq_len(nrow(points)),
        function(i) as.numeric(points[i, ])))

    rows <- rep(seq_len(nrow(points)), each = each)
    vals <- par_eval(simfun, xrows, rows, par)

    for (i in seq_len(nrow(points))) {

        ind <- which(xvalues == digest::digest(as.numeric(points[i, ])))

        if (length(ind) == 0) {
            ind <- length(dat) + 1
            dat[[ind]] <- list()
            dat[[ind]]$x <- points[i, ]
            resx <- c()
            # (as in addval()) register the new design, so that a later row
            # of the same batch with the same design adds to it instead of
            # storing it twice
            xvalues <- c(unlist(xvalues), digest::digest(as.numeric(points[i, ])))
        } else {
            resx <- dat[[ind]]$y
        }

        # combine the values as replicate() does in the sequential path
        resx <- c(resx, simplify2array(vals[rows == i], higher = TRUE))

        while (minrun && length(resx) > 1 && stats::var(resx) == 0) {
            resx <- c(resx, simplify2array(par_eval(simfun, xrows, i, par),
                higher = TRUE))
        }
        dat[[ind]]$y <- resx
    }

    if (!is.null(autosave_dir))
        save(dat, file = file.path(autosave_dir, "dat_autosave.RData"))

    return(dat)
}
