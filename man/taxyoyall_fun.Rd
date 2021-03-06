% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxyoyall.R
\name{taxyoyall_fun}
\alias{taxyoyall_fun}
\title{WARL formulas}
\usage{
taxyoyall_fun(noassets, yearslabel, noyears, taxremlife, taxoavalue,
  startyearend, projyearend, taxstdlife, fcnetavg.full, fccust.full,
  cif.df, taxiabcheck)
}
\arguments{
\item{noassets}{count of asset classes}

\item{yearslabel}{from 2020 to projected year end}

\item{noyears}{count of years}

\item{taxremlife}{vector of tax remaining life of assets from dnsp assets.df input dataframe}

\item{taxoavalue}{vector of tax opening asset value of assets from dnsp assets.df input dataframe}

\item{startyearend}{first regulatory year (2019 or 2020)}

\item{projyearend}{final projected year}

\item{taxstdlife}{vector of tax standard life of assets from dnsp assets.df input dataframe}

\item{fcnetavg.full}{dataframe of all netcapex for all assets defined by netcapex function}

\item{fccust.full}{dataframe of all customer contribution for all assets}

\item{cif.df}{dataframe of cumulative inflation}

\item{taxiabcheck}{dataframe of raw tax iab values from ptrm for the first ten years}
}
\description{
Weighted average remaining life method for real depreciation
}
\details{
Function calculates only one asset class: ac, which is specified by user. The function has to be looped to project the WARL depreciation for all asset classes.
Prerequisites that need to be defined in ptrm model, i.e. run above code: ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue, noyears
}
\keyword{capex,}
\keyword{depreciation,}
\keyword{on}
\keyword{remlife,}
\keyword{stdlife}
\keyword{tax,}
\keyword{year}
\keyword{year,}
