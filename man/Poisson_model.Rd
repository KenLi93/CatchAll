% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_models.R
\name{Poisson_model}
\alias{Poisson_model}
\title{Poisson model for estimating species richness}
\usage{
Poisson_model(input_data, cutoff = 10)
}
\arguments{
\item{input_data}{An input type that can be processed by convert()}

\item{cutoff}{Maximal frequency count of the data used to estimating the species richness. Default 10.}
}
\value{
An object of class \code{alpha_estimate}
}
\description{
This function implements the species richness estimation with a Poisson distribution
}
\examples{
library(breakaway)
data(apples)
Poisson_model(apples, cutoff = 20)
}
