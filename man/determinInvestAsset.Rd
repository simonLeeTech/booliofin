% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portfolioElement.R
\name{determinInvestAsset}
\alias{determinInvestAsset}
\title{A determinInvestAsset Function
determin}
\usage{
determinInvestAsset(
  par_list,
  price,
  momentum_function = c("tsMomentumScore", "priceRankCorr")
)
}
\arguments{
\item{par_list}{is list of asset parameters}

\item{price}{is xts object}

\item{momentum_function}{tsMomentumScore, priceRankCorr}
}
\description{
A determinInvestAsset Function
determin
}
\examples{
#' determinInvestAsset(par_list, price, momentum_function = "tsMomentumScore")
}
\keyword{say}
