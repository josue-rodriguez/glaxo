#' Plotting method for glaxo objects

#' @param object An object of type \code{glaxo}
#' @param layout A layout as specified in \code{\link[ggnetwork]{ggnetwork}}
#' @param node_labels Labels for nodes. Optional.
#' @param node_label_size Size of node labels. Default is 5.
#' @param node_size Size of nodes. Default is 11.
#' @param node_color Color of nodes. Default is "white".
#' @param node_border_size Size of node borders. Default is 12.
#' @param node_border_color Color of node borders. Default is "black".
#' @param edge_alpha Opaqueness of edges. Default is 1.
#' @param ... Additional arguments to be passed to \code{\link[ggnetwork]{ggnetwork}}
#'
#' @examples
#' data("bfi", package = "psych")
#' Y <- subset(bfi, select = -c(gender, education, age))
#' Y <- na.omit(Y)
#'
#' model <- glaxo(Y, progress = FALSE)
#' plot_edges(model)
#'
#' @export
#'
#' @import ggnetwork
#' @import ggplot2
#' @importFrom network network
#' @importFrom network set.edge.value
#' @importFrom network set.vertex.attribute

plot_edges <- function(object,
                       layout = "fruchtermanreingold",
                       node_labels = NULL,
                       node_label_size = 5,
                       node_size = 11,
                       node_color = "white",
                       node_border_size = 12,
                       node_border_color = "black",
                       edge_alpha = 1,
                       ...) {

  P_lower <- object$P[lower.tri(object$P)]
  P_not_zero <- P_lower[which(P_lower != 0)]

  pos_neg <- ifelse(P_not_zero >= 0, "pos", "neg")

  net <- network::network(object$adj,
                          directed = FALSE)

  network::set.edge.attribute(net, attrname = "edge_weights", value = abs(P_not_zero))

  network::set.edge.attribute(net, attrname = "pos_neg", value = pos_neg)

  if (is.null(node_labels)) node_labels <- as.character(1:ncol(object$P))
  network::set.vertex.attribute(net, "node_labels", value = node_labels)

  gg_net <- ggnetwork(net, layout = layout, ...)

    p <-
      ggplot(gg_net, aes(x = x,
                         y = y,
                         xend = xend,
                         yend = yend)) +
      geom_edges(aes(size = edge_weights, col = pos_neg),
                 alpha = edge_alpha) +
      geom_nodes(size = node_border_size, col = node_border_color) +
      geom_nodes(size = node_size, col = node_color) +
      geom_nodetext(aes(label = node_labels), size = node_label_size)
  return(p)
}

