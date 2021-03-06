% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_deriv.R
\name{deriv1.logistic}
\alias{deriv1.logistic}
\title{First Derivative}
\usage{
deriv1.logistic(X = NULL, Y = NULL, param = NULL)
}
\arguments{
\item{X}{is a n x p matrix with p features}

\item{Y}{is an n x 1 vector of binary outcomes}

\item{param}{is the parameter to be estimated in logistic() function}
}
\description{
This function calculates the first derivative of the log likelihood function of the logistic regression GLM.
}
