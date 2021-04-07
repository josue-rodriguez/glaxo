#'Relaxed glasso
#'
#' @param Y
#' @param S
#' @param n
#' @param nlambda_relaxed
#' @param ...
#'
#'
#' @example
#'library(GGMncv)
#'main <- gen_net()
#'n <- 5000
#'Y <- MASS::mvrnorm(n, mu = rep(0, 20), main$cors)
#'
#'relaxed_glass <- glaxo(Y, n)
#'
#'


glaxo <- function(Y, n, S = NULL, p = NULL, nlambda_relaxed = 4, ic = "bic", ...) {
  dots <- list(...)

  if (is.null(S)) {R <- cor(Y); p <- ncol(Y)} else {R <- cov2cor(S); p <- p}

  fit <- GGMncv::ggmncv(R,
                        penalty = "lasso",
                        n = n,
                        ic = ic,
                        ...)
  ls <- list()
  l <- fit$lambda
  l_length <- seq_along(l)
  for(i in l_length){
    li <- seq(0, l[i], length.out = nlambda_relaxed)
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
                        type = ic)
  })
  relaxed <- ls[[which.min(bics)]]
  class(relaxed) <- c("glaxo", class(relaxed))
  return(relaxed)
}



#' Predict method for glaxo object
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
  if (missing(newdata)) stop("newdata must be specified")
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
  predictions_rescaled <- sweep(pred_mat, 2, og_sd, "*")
  predictions <- sweep(predictions_rescaled, 2, og_mean, "+")

  ret <- list(predictions = predictions,
              beta_matrix = coef_mat)
  return(ret)
}


#' Plotting method for glaxo objects
#'
#' @param object
#' @param layout
#' @param node_labels
#' @param ...
#'
#' @return
#' @export
#' @import ggnetwork
#' @examples
#'

plot_edges <- function(object,
                       layout = "fruchtermanreingold",
                       node_labels = NULL,
                       ...) {
    theta <- solve(object$fit$w)
    ds <- diag(diag(theta^-0.5))
    pcors <- -(ds %*% theta %*% ds)
    diag(pcors) <- -(diag(pcors))

    plot_list <- list(P = pcors, adj = ifelse(pcors > 0, 1, 0))

    net <- network::network(plot_list$adj)

    network::set.edge.value(net, "edge_weights", value = plot_list$P[upper.tri(plot_list$P)])

    if (is.null(node_labels)) node_labels <- as.character(1:ncol(pcors))
    network::set.vertex.attribute(net, "node_labels", value = node_labels)

    gg_net <- ggnetwork(net, layout = layout)
    p <-
      ggplot(gg_net, aes(x = x,
                         y = y,
                         xend = xend,
                         yend = yend)) +
      geom_edges(aes(size = edge_weights)) +
      geom_nodes(size = 11, col = "black") +
      geom_nodes(size = 10, col = "white") +
      geom_nodetext(aes(label = node_labels))
    return(p)
}
