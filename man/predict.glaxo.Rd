% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.glaxo}
\alias{predict.glaxo}
\title{Predict method for glaxo object}
\usage{
\method{predict}{glaxo}(object, newdata, scale = TRUE, ...)
}
\arguments{
\item{object}{An object of type \code{glaxo}}

\item{newdata}{A \code{data.frame} or matrix with with same number of columns as the original data.}

\item{scale}{Whether the newdata should be scaled. Default is true.}

\item{...}{Currently ignored}
}
\value{
A list containing the predictions and associated coefficients
\describe{
\item{predictions}{A matrix containing predictions for the new data}
\item{beta_matrix}{A matrix where each (i,j)th element is the beta coefficient of variable j when predicting variable i. The diagonal elements encode the variances for the corresponding variables.}
}
}
\description{
Generate predictions for new data based on glasso estimates
}
