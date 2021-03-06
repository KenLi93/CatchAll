% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_parametric_model.R
\name{best_model}
\alias{best_model}
\title{"Best" parametric model for estimating species richness}
\usage{
best_model(input_data, alpha_estimate = T, ...)
}
\arguments{
\item{input_data}{An input type that can be processed by convert()}

\item{alpha_estimate}{If \code{alpha_estimate == T}, the result will be returned as an \code{alpha_estimate} object.}

\item{...}{Additional arguments for the function \code{select_best_models}}
}
\value{
A data frame displaying the point estimates, standard errors, AICc, GOF0 and GOF5 for different
parametric models and cutoffs.
}
\description{
This function gives the "best" model among Poisson model, geometric model,
two-component geometric mixture model and three-component geometric model.
}
\examples{
library(breakaway)
data(apples)
best_model(apples)

}
