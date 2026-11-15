# Fast pointwise prediction for the lm / glm surrogates (reg, logreg)
# ---------------------------------------------------------------------
#
# stats::predict(mod, newdata = data.frame(t(x))) costs ~100 microseconds per
# point, almost all of it model.frame/model.matrix overhead. For the models
# mlpwr fits (y ~ . on V1..Vd, intercept, no offset, full rank) the prediction
# is drop(X[, piv] %*% beta[piv]) on the row c(1, x), followed by
# family$linkinv for glm type = "response". lin_cache() precomputes the
# coefficients and replays exactly those operations, including the row name
# "1" that predict() puts on the result, so results are bit-identical.
# It returns NULL for any model it does not reproduce (callers then use
# predict()).

lin_cache <- function(mod, xnames, linkinv = NULL) {

    if (is.null(mod) || !inherits(mod, "lm"))
        return(NULL)
    beta <- mod$coefficients
    cn <- c("(Intercept)", xnames)
    p <- mod$rank
    if (!identical(names(beta), cn) || anyNA(beta) || p != length(beta) ||
        !is.null(mod$call$offset) || !is.null(mod$offset))
        return(NULL)
    piv <- mod$qr$pivot[seq_len(p)]
    beta.piv <- beta[piv]
    d <- length(xnames)
    dn <- list("1", cn)

    accepts <- function(x) is.numeric(x) && is.null(dim(x)) && length(x) ==
        d

    predict <- function(x) {
        X <- matrix(c(1, as.double(x)), 1L, p, dimnames = dn)
        predictor <- drop(X[, piv, drop = FALSE] %*% beta.piv)
        if (!is.null(linkinv))
            predictor <- linkinv(predictor)
        predictor
    }

    # predictions for the rows of a numeric matrix (for screening a grid in
    # the exact design search; same operations as predict() row by row)
    predict_batch <- function(xmat) {
        X <- cbind(1, matrix(as.double(xmat), ncol = d))
        predictor <- drop(X[, piv, drop = FALSE] %*% beta.piv)
        if (!is.null(linkinv))
            predictor <- linkinv(predictor)
        as.numeric(predictor)
    }

    list(accepts = accepts, predict = predict, predict_batch = predict_batch)
}
