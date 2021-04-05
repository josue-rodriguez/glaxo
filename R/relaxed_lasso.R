#'Relaxed glasso
#'
#' @param Y
#' @param S
#' @param n
#' @param nlambda
#' @param ...
#'
#'
#' @example
#'library(GGMnonreg)
#'library(GGMncv)
#'main <- gen_net()
#'n <- 5000
#'Y <- MASS::mvrnorm(n, mu = rep(0, 20), main$cors)
#'fit <- ggmncv(cor(Y), penalty = "lasso",
#'              n = n,
#'              ic = "bic")
#'


relaxed_lasso <- function(Y, n, S = NULL, p = NULL, nlambda = 4, ...) {
  dots <- list(...)

  if (is.null(S)) {R <- cor(Y); p <- ncol(Y)} else {R <- cov2cor(S); p <- p}

  fit <- GGMncv::ggmncv(R,
                        penalty = "lasso",
                        n = n,
                        ic = "bic",
                        ...)
  ls <- list()
  l <- fit$lambda
  l_length <- seq_along(l)
  for(i in l_length){
    li <- seq(0, l[i], length.out = nlambda)
    adji <- ifelse(fit$fitted_models[[i]]$wi == 0, 0 , 1)
    newR <- GGMncv::constrained(R, adj = adji)$Sigma
    fit2 <- GGMncv::ggmncv(newR, n = n, lambda = li, ...)
    ls[[i]] <- fit2
  }
  bics <- sapply(l_length, function(x) {
    GGMncv:::gic_helper(Theta = ls[[x]]$Theta,
                        R = R,
                        edges = sum(ls[[x]]$Theta[upper.tri(diag(p))] != 0),
                        n = n,
                        p = p,
                        type = "bic")
  })
  # adjnew <- ls[[which.min(bics)]]$Theta
  # adj <- ifelse(adjnew == 0, 0, 1)
  relaxed <- ls[[which.min(bics)]]
  class(relaxed) <- c("glaxo", class(relaxed))
  return(relaxed)
}



#' Predict
#'
#' @param object
#' @param newdata
#' @param scale
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'


predict.glaxo <- function(object, newdata, scale = TRUE, ...) {

  if (scale) {
    newdata <- scale(newdata)
    og_mean <- attr(newdata, "scaled:center")
    og_sd <- attr(newdata, "scaled:scale")
  }

  # precision matrix (glasso precision matrix is not always symmetric)
  theta <- solve(object$fit$w)
  p <- ncol(theta)

  # matrix to store coefficients
  coef_mat <- matrix(NA, ncol = p, nrow = p)
  for (j in seq(p)) {
    # coefficients predicting jth node
    coef_mat[j, -j] <- - (theta[j, -j] / theta[j, j])
    # variance for jth node
    coef_mat[j, j] <- 1 / theta[j, j]
  }

  # predictions
  pred_mat <- matrix(NA, nrow = nrow(newdata), ncol = p)
  for (i in seq(p)) {
    pred_mat[, i] <- newdata[, -i] %*% coef_mat[i, -i]
  }

  # rescale
  prediction_rescaled <- sweep(pred_mat, 2, og_sd, "*")
  predictions <- sweep(pred_mat, 2, og_mean, "+")

  ret <- list(predictions = predictions,
              beta_matrix = coef_mat)
  return(ret)
}
