#' Relaxed glasso
#'
#' An implementation of relaxed glasso based on the 'simple algorithm'
#' for the relaxed lasso presented in section 2.2 of Meinshausen (2006)
#'
#' @param Y A \code{data.frame} or matrix containing data. Specified if not supplying covariance matrix.
#' @param S A covariance matrix. Specified if not supplying raw data.
#' @param n Number of observations. Specified if not supplying raw data.
#' @param nlambda_relaxed Number of lambdas to be generated in Step 2 of algorithm
#' @param ic Information critera to be used for selecting model. See \code{\link[GGMncv]{ggmncv}}.
#' @param ...  Other arguments to be passed on to \code{\link[GGMncv]{ggmncv}}
#'
#'
#' @examples
#' data("bfi", package = "psych")
#' Y <- subset(bfi, select = -c(gender, education, age))
#' Y <- na.omit(Y)
#'
#'model <- glaxo(Y)
#' @export
#' @return An object of classes \code{glaxo}, \code{ggmncv}, and \code{default}
#' @importFrom GGMncv ggmncv
#' @importFrom stats cor cov2cor



glaxo <- function(Y, S = NULL, n = NULL, nlambda_relaxed = 4, ic = "bic", ...) {
  if (is.null(S)) {
    R <- cor(Y)
    p <- ncol(Y)
    n <- nrow(Y)
  } else {
    R <- cov2cor(S)
    p <- ncol(S)
    }
  # Step 1
  fit <- GGMncv::ggmncv(R,
                        penalty = "lasso",
                        n = n,
                        ic = ic,
                        ...)

  # Step 2
  lambdas <- fit$lambda
  lambdas_length <- length(lambdas)
  model_list <- vector("list", length = lambdas_length)

  for (i in 1:lambdas_length) {
    li <- seq(0, lambdas[i], length.out = nlambda_relaxed)
    adji <- ifelse(fit$fitted_models[[i]]$wi == 0, 0 , 1)
    newR <- GGMncv::constrained(R, adj = adji)$Sigma
    tmp_fit <- GGMncv::ggmncv(newR, n = n, lambda = li, ...)
    model_list[[i]] <- tmp_fit
  }

  # helper function included so `model_list` and `p` are within function scope
  ic_helper <- function(x) {
    Theta <- model_list[[x]]$Theta

    edges <- sum(model_list[[x]]$Theta[upper.tri(diag(p))] != 0)

    gic_helper(Theta = Theta,
               R = R,
               n = n,
               p = p,
               edges = edges,
               type = ic)
  }



  # Compute information criteria for each model and select model
  ics <- sapply(lambdas_length, function(x) ic_helper(x))
  selected_model <- model_list[[which.min(ics)]]

  class(selected_model) <- c("glaxo", class(selected_model))

  return(selected_model)
}


