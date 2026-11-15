addval <- function(simfun, dat = list(), points = NULL,
    each = 1, minrun = FALSE, autosave_dir = NULL, par = NULL) {

    # opt-in parallel evaluation (find.design(parallel = ...)), see parallel.R
    if (!is.null(par))
        return(addval_parallel(simfun = simfun, dat = dat, points = points,
            each = each, minrun = minrun, autosave_dir = autosave_dir, par = par))

    xvalues <- sapply(dat, function(y) digest::digest(as.numeric(y$x)))

    # Silence simfun output once for the whole batch instead of once per
    # evaluation (see hush_begin()). n.done counts completed evaluations.
    n.done <- 0L
    hush_begin()
    on.exit(hush_end(n.done))

    # Rows of points as numeric vectors, extracted once per point instead of
    # once per evaluation. sim() evaluates the same call text as before,
    # simfun(as.numeric(points[i, ])), on this matrix.
    xrows <- do.call(rbind, lapply(seq_len(nrow(points)),
        function(i) as.numeric(points[i, ])))
    sim <- function(points, i, n) suppressWarnings(suppressMessages(replicate(n,
        {
            v <- simfun(as.numeric(points[i, ]))
            n.done <<- n.done + 1L
            v
        })))

    for (i in seq_len(nrow(points))) {

        ind <- which(xvalues == digest::digest(as.numeric(points[i,
            ])))

        if (length(ind) == 0) {
            ind <- length(dat) + 1
            dat[[ind]] <- list()
            dat[[ind]]$x <- points[i, ]
            resx <- c()
            # register the new design, so that a later row of the same
            # batch with the same design adds to it instead of storing it
            # twice
            xvalues <- c(unlist(xvalues), digest::digest(as.numeric(points[i, ])))
        } else {
            resx <- dat[[ind]]$y
        }
        a <- as.numeric(points[i, ])

        resx <- c(resx, sim(xrows, i, each))

        while (minrun && length(resx) > 1 && stats::var(resx) ==
            0) {
            resx <- c(resx, sim(xrows, i, 1))
        }
        dat[[ind]]$y <- resx
    }

    hush_end(n.done)
    on.exit()

    if (!is.null(autosave_dir))
        save(dat, file = file.path(autosave_dir, "dat_autosave.RData"))

    return(dat)

}




# find.design(autosave_dir = ): the directory for dat_autosave.RData, created
# if needed; checked before any simulation
check_autosave_dir <- function(autosave_dir) {
    if (is.null(autosave_dir))
        return(invisible(NULL))
    if (!is.character(autosave_dir) || length(autosave_dir) != 1 ||
        is.na(autosave_dir))
        stop("autosave_dir must be a single directory name.", call. = FALSE)
    if (!dir.exists(autosave_dir) &&
        !dir.create(autosave_dir, recursive = TRUE, showWarnings = FALSE))
        stop("Could not create the directory autosave_dir = \"", autosave_dir,
            "\".", call. = FALSE)
    invisible(autosave_dir)
}
