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


# taken from IRCcheck:::compare
compare <- function (Estimate, True) {
  True <- as.matrix(True)
  Estimate <- as.matrix(Estimate)
  TN <- ifelse(True[upper.tri(True)] == 0 & Estimate[upper.tri(Estimate)] ==
                 0, 1, 0)
  TN <- sum(TN)
  FP <- ifelse(True[upper.tri(True)] == 0 & Estimate[upper.tri(Estimate)] !=
                 0, 1, 0)
  FP <- sum(FP)
  TP <- ifelse(True[upper.tri(True)] != 0 & Estimate[upper.tri(Estimate)] !=
                 0, 1, 0)
  TP <- sum(TP)
  FN <- ifelse(True[upper.tri(True)] != 0 & Estimate[upper.tri(Estimate)] ==
                 0, 1, 0)
  FN <- sum(FN)
  Specificity <- TN/(TN + FP)
  Sensitivity <- TP/(TP + FN)
  Precision <- TP/(TP + FP)
  Recall <- TP/(TP + FN)
  F1_score <- 2 * ((Precision * Recall)/(Precision + Recall))
  MCC <- (TP * TN - FP * FN)/sqrt((TP + FP) * (TP + FN) * (TN +
                                                             FP) * (TN + FN))
  results <- c(Specificity, Sensitivity, Precision, Recall,
               F1_score, MCC)
  results_name <- c("Specificity", "Sensitivity",
                    "Precision", "Recall", "F1_score",
                    "MCC")
  results <- cbind.data.frame(measure = results_name, score = results)
  return(results)
}

# Declare global variables

utils::globalVariables(c("x", "y", "xend", "yend", "edge_weights"))


