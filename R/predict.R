#' Predict method for glaxo object
#'
#' Generate predictions for new data based on glasso estimates
#' @param object An object of type \code{glaxo}
#' @param newdata A \code{data.frame} or matrix with with same number of columns as the original data.
#' @param scale Whether the newdata should be scaled. Default is true.
#' @param ... Currently ignored
#'
#' @return A list containing the predictions and associated coefficients
#' \describe{
#' \item{predictions}{A matrix containing predictions for the new data}
#' \item{beta_matrix}{A matrix where each (i,j)th element is the beta coefficient of variable j when predicting variable i. The diagonal elements encode the variances for the corresponding variables.}
#' }
#'
#' @rdname predict.glaxo
#'
#' @export
#'


predict.glaxo <- function(object, newdata, scale = TRUE, ...) {
  if (missing(newdata)) stop("newdata must be specified")
  if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
  if (scale) {
    newdata <- scale(newdata)
    og_mean <- attr(newdata, "scaled:center")
    og_sd <- attr(newdata, "scaled:scale")
  }
  # invert glasso covariance matrix w (glasso precision `wi` matrix is not always symmetric)
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
  predictions_rescaled <- sweep(pred_mat, 2, og_sd, "*")
  predictions <- sweep(predictions_rescaled, 2, og_mean, "+")

  ret <- list(predictions = predictions,
              beta_matrix = coef_mat)
  return(ret)
}
