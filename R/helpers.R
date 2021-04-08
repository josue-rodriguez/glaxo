# taken from GGMncv:::gic_helper
gic_helper <- function (Theta, R, edges, n, p, type = "bic", ...)
{
  log.like <- (n/2) * (log(det(Theta)) - sum(diag(R %*% Theta)))
  neg_ll <- -2 * log.like
  if (type == "bic" | type == "gic_1") {
    ic <- neg_ll + edges * log(n)
  }
  else if (type == "aic") {
    ic <- neg_ll + edges * 2
  }
  else if (type == "gic_2") {
    ic <- neg_ll + edges * p^(1/3)
  }
  else if (type == "ric" | type == "gic_3") {
    ic <- neg_ll + edges * 2 * log(p)
  }
  else if (type == "gic_4") {
    ic <- neg_ll + edges * 2 * (log(p) + log(log(p)))
  }
  else if (type == "gic_5") {
    ic <- neg_ll + edges * log(log(n)) * log(p)
  }
  else if (type == "gic_6") {
    ic <- neg_ll + edges * log(n) * log(p)
  }
  else if (type == "ebic") {
    dots <- list(...)
    if (is.null(dots$ebic_gamma)) {
      gamma <- 0.5
    }
    else {
      gamma <- dots$ebic_gamma
    }
    ic <- neg_ll + edges * log(n) + 4 * edges * gamma * log(p)
  }
  else {
    stop("ic not found. see documentation")
  }
  return(ic)
}

utils::globalVariables("edge_weights")

utils::globalVariables(c("x", "y", "xend", "yend"))
