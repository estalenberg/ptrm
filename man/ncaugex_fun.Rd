% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncaugex.R
\name{ncaugex_fun}
\alias{ncaugex_fun}
\title{Netcapex functions: iii Augex}
\usage{
ncaugex_fun(fcnetavg, augexratio, growth.df, noassets, assetcode)
}
\arguments{
\item{fcnetavg}{empty dataframe with first 5 years from DNSP and an avg}

\item{augexratio}{augex ratio, DNSP static variable}

\item{growth.df}{dataframe of capex growth function}

\item{noassets}{count of asset classes}

\item{assetcode}{if asset class is coded as 1 or 2}
}
\description{
Third formula for extending netcapex beyond five years March 22 2019
}
\details{
Returns a database of projected netcapex:iii augex for all assets across all years.

Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
}
