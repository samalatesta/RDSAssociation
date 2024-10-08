% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPRTCA.R
\name{sprtca}
\alias{sprtca}
\title{Semi-parametric randomization test for continuous association}
\usage{
sprtca(net, var1, var2, iter = 5000)
}
\arguments{
\item{net}{A RDS data frame.See the RDS package documentation (https://cran.r-project.org/web/packages/RDS/RDS.pdf) for more detail.}

\item{var1}{Variable 1, a numeric vector}

\item{var2}{Variable 2, a numeric or character vector}

\item{iter}{Number of iterations for randomization}
}
\description{
SPRTCA was developed to infer bivariate association between two variables
collected using respondent-driven sampling (RDS) when one or both variables are continuous.
}
