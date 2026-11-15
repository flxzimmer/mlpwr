

timer <- function(obj = NULL, printit = FALSE) {
    # usage: a = timer() timer(a)

    if (is.null(obj)) {
        obj <- proc.time()
        return(obj)
    }

    if (!is.null(obj)) {

        if (printit) {
            timex <- proc.time() - obj
            t <- timex[3]
            a <- paste(round(t/60/60, 2), "Hours |",
                round(t/60, 2), "Minutes |", round(t,
                  2), "Seconds")
            print(a)
        }


        timex <- proc.time() - obj
        return(timex[3])

    }
}



hush <- function(code) {

    os <- Sys.info()["sysname"]
    st <- "/dev/null"
    if (os == "Windows")
        st <- "NUL"

    sink(st)  # use /dev/null in UNIX
    on.exit(sink())  # reset the sink on exit (also on error), exactly once
    tmp <- code
    return(tmp)
}


# Batch version of hush() used by addval(): divert output to the null device
# once for a whole batch of simfun evaluations. hush_end(n) removes that sink
# so the sink stack ends up exactly as after n hush() calls.
null_device <- function() if (.Platform$OS.type == "windows") "NUL" else "/dev/null"

hush_begin <- function() sink(null_device())

hush_end <- function(n) {
    # n is kept for the call sites; hush() pops only its own sink, so a
    # batch also pops exactly one
    sink()
}


usedevaluations <- function(dat) {
    return(sum(sapply(dat, function(x) length(x$y))))
}



todataframe <- function(dat, aggregate = TRUE, pseudo = FALSE, aggregate_fun=mean) {

    dim.design <- length(dat[[1]]$x)

    if (aggregate) {
        temp <- t(sapply(dat, function(v) c(as.numeric(v$x), aggregate_fun(as.numeric(v$y)))))
    }

    if (!aggregate & !pseudo) {
        temp <- lapply(dat, function(v) {
            tempx <- data.frame(y = v$y)
            for (i in 1:dim.design) {
                tempx[, i + 1] <- v$x[i]
            }
            return(tempx)
        })
        temp <- do.call(rbind, temp)
        temp <- temp[, c(2:length(temp), 1)]
    }


    if (!aggregate & pseudo) {
        temp <- lapply(dat, function(v) {
            tempx <- data.frame(y = rep(mean(v$y),
                length(v$y)))
            for (i in 1:dim.design) {
                tempx[, i + 1] <- v$x[i]
            }
            return(tempx)
        })
        temp <- do.call(rbind, temp)
        temp <- temp[, c(2:length(temp), 1)]
    }

    temp <- apply(temp, 2, as.numeric)
    temp <- as.data.frame(temp)
    names(temp) <- c(paste0("V", 1:dim.design), "y")

    return(temp)
}




get.sd <- function(dat, value) {

    ind <- which(sapply(dat, function(ele) all(ele$x ==
        value)))
    if (length(ind) == 0)
        return(10) else {
        return(getweight(dat, "sd")[ind])
    }
}



getweight <- function(dat, weight.type = "freq", correct_zero = TRUE) {

    # correct_zero Adds one number to the data if
    # it consists only of 0s or only of 1s.
    # Variance can only then be calculated.

    if (is.null(weight.type))
        return(NULL)

    fun <- function(vec, weight.type) {
        vec <- as.numeric(vec)
        p <- mean(vec)
        n <- length(vec)
        if (correct_zero) {
            if (p == 0)
                vec <- c(vec, 1)
            if (p == 1)
                vec <- c(vec, 0)
            p <- mean(vec)
        }
        variance <- p * (1 - p)/n

        if (weight.type == "freq")
            return(n)
        if (weight.type == "var")
            return(variance)
        if (weight.type == "inv_var")
            return(1/variance)
        if (weight.type == "sd")
            return(sqrt(variance))
        if (weight.type == "inv_sd")
            return(1/sqrt(variance))
    }

    w <- sapply(dat, function(v) fun(v$y, weight.type))
    return(w)
}


print_progress <- function(n_updates, evaluations_used,
    time_used) {
    # cat('\r',paste(c('Updates','evaluations','Time'),c(n_updates,evaluations_used,round(time_used,1)),sep=':
    # ',collapse=', '),'\n')
    cat("\r", paste(c("Updates", "Evaluations", "Time"),
        c(n_updates, evaluations_used, round(time_used,
            1)), sep = ": ", collapse = ", "))
    utils::flush.console()
}


# while (1) {
# cat('\r',format(Sys.time(),'%H:%M:%S'))
# flush.console() }


relu <- function(x) ifelse(x > 0, x, 0)


fix.argtype <- function(fun, boundaries, vectorise = FALSE) {

    ismulti <- length(boundaries) > 1

    # Does fun take the design parameters as separate arguments (fun(n, k))
    # or as one vector (fun(x))? Arguments with defaults that are not design
    # parameters (e.g. fun(x, verbose = FALSE)) must not count as design
    # arguments.
    fmls <- formals(args(fun))
    nondots <- fmls[names(fmls) != "..."]
    required <- vapply(nondots, function(a) identical(a, quote(expr = )),
        logical(1))
    bnames <- names(boundaries)
    if (sum(required) > 1) {
        islist <- TRUE
    } else if (!is.null(bnames) && all(nzchar(bnames))) {
        # named design parameters: separate arguments iff fun has them all
        islist <- all(bnames %in% names(nondots))
    } else {
        # unnamed boundaries: count all arguments, as before
        islist <- length(fmls) > 1
    }

    if (islist & ismulti) {
        re <- function(x) {
            do.call(fun, as.list(x))
        }
        # the same call for all rows of a matrix at once, with whole columns
        # as arguments (named like the elements of a row, as in re); used by
        # grid_costs() only if it reproduces re exactly
        attr(re, "vectorised") <- function(X) {
            cols <- lapply(seq_len(ncol(X)), function(j) X[, j])
            names(cols) <- colnames(X)
            do.call(fun, cols)
        }
    } else {
        re <- fun
        # with vectorise = TRUE (the costfun), the same for a function of
        # one argument, called with all rows at once: the column vector (one
        # design parameter, if that gives one value per row) or a data frame
        # of the columns without names, so that x[1], x[[2]], as.list(x) give
        # whole columns (used only if it reproduces re exactly, as above)
        if (vectorise && is.null(attr(re, "vectorised", exact = TRUE))) {
            attr(re, "vectorised") <- function(X) {
                if (ncol(X) == 1) {
                    v <- tryCatch(fun(X[, 1]), error = function(e) NULL)
                    if (is.numeric(v) && length(v) == nrow(X))
                        return(v)
                }
                cols <- as.data.frame(X)
                names(cols) <- rep("", ncol(X))
                v <- fun(cols)
                if (is.data.frame(v) && length(v) == 1L) v[[1L]] else v
            }
        }
    }

    # points =
    # initpoints(boundaries=boundaries,n.points =
    # 3)[1,] names(points) = c() islist =
    # is.na(tryCatch(hush(fun(as.numeric(points))),error=function(e)NA))
    # suppressWarnings(sink());suppressWarnings(sink())
    # if (islist) re = function(x) {
    # do.call(fun,as.list(x)) } else { re = fun }

    return(re)
}




