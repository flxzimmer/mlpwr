# Cached kriging predictor for a fitted DiceKriging::km model
# ---------------------------------------------------------------------
#
# gauss.fit() predicts the surrogate's mean and standard deviation at many
# points of the design space for one fitted model: pointwise (fitfun,
# fitfun.sd, the plane check, the optimiser) and for whole grids (the exact
# grid search). km_cache(mod) returns a predictor with this interface:
#
#   accepts(x)          TRUE if x is a plain numeric vector of length d
#   mean(x), sd(x)      pointwise; the last point is memoised, so fitfun(x)
#                       followed by fitfun.sd(x) solves only once
#   mean_batch(Z), mean_screen(Z), meansd_batch(Z)
#                       rows of the numeric matrix Z, in column blocks of at
#                       most screen_block cross-covariances
#   cross_cov(Z)        n x nrow(Z) cross-covariances
#   d
#
# It wraps DiceKriging::predict.km(type = "UK") itself: pointwise calls are
# memoised and grids are predicted in blocks of rows, so the values are
# those of predict.km.

# cross-covariance elements per block of screened points (memory bound)
km_screen_block_default <- 2^16

km_cache <- function(mod) {
    if (!inherits(mod, "km"))
        return(NULL)
    km_cache_calls(mod)
}

km_as_matrix <- function(xmat) {
    Z <- as.matrix(xmat)
    storage.mode(Z) <- "double"
    dimnames(Z) <- NULL
    Z
}

km_cache_calls <- function(mod) {
    d <- ncol(mod@X)
    n <- nrow(mod@X)
    nms <- colnames(mod@X)
    screen_block <- km_screen_block_default

    pk <- function(Z, want_sd) {
        nd <- as.data.frame(Z)
        names(nd) <- nms
        DiceKriging::predict.km(mod, newdata = nd, type = "UK",
            se.compute = want_sd, light.return = TRUE, checkNames = FALSE)
    }

    blocks <- function(Z, want_sd) {
        k <- nrow(Z)
        m <- numeric(k)
        s <- if (want_sd) numeric(k)
        bs <- max(1L, floor(screen_block/n))
        for (st in seq(1L, k, by = bs)) {
            rows <- st:min(k, st + bs - 1L)
            p <- pk(Z[rows, , drop = FALSE], want_sd)
            m[rows] <- p$mean
            if (want_sd)
                s[rows] <- p$sd
        }
        list(mean = m, sd = s)
    }

    last.x <- NULL
    last.mean <- NULL
    last.sd <- NULL

    accepts <- function(x) is.numeric(x) && is.null(dim(x)) && length(x) ==
        d

    mean <- function(x) {
        xd <- as.double(x)
        if (!identical(xd, last.x)) {
            last.x <<- xd
            last.mean <<- pk(matrix(xd, 1L), FALSE)$mean
            last.sd <<- NULL
        }
        last.mean
    }

    sd <- function(x) {
        xd <- as.double(x)
        if (!identical(xd, last.x) || is.null(last.sd)) {
            p <- pk(matrix(xd, 1L), TRUE)
            last.x <<- xd
            last.mean <<- p$mean
            last.sd <<- p$sd
        }
        last.sd
    }

    mean_screen <- function(xmat) blocks(km_as_matrix(xmat), FALSE)$mean
    mean_batch <- function(xmat, chunk = 2000L) mean_screen(xmat)
    meansd_batch <- function(xmat) blocks(km_as_matrix(xmat), TRUE)
    cross_cov <- function(newdata) DiceKriging::covMat1Mat2(mod@covariance,
        mod@X, km_as_matrix(newdata), nugget.flag = TRUE)

    list(accepts = accepts, mean = mean, sd = sd, mean_batch = mean_batch,
        meansd_batch = meansd_batch, mean_screen = mean_screen,
        cross_cov = cross_cov, d = d)
}
