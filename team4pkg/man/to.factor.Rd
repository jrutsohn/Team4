% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_var.R
\name{to.factor}
\alias{to.factor}
\title{Factor Variables}
\usage{
to.factor(dat, x)
}
\arguments{
\item{dat}{is the data be imputed, where we want to produce factor variables}

\item{x}{is the list of variables to be factored}
}
\description{
This function converts numeric variables into factor variables for the purposes of imputation.  
The first step is to supply the list of variables needed to be converted into factors.
}
\examples{
\dontrun{
v <- c("job","marital","education")
to.factor(data,v)
}
}
