% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warl.R
\name{warl_fun}
\alias{warl_fun}
\title{WARL formulas}
\usage{
warl_fun(ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife,
  oavalue, noyears)
}
\arguments{
\item{ac}{asset class}

\item{yearslabel}{from 2020 to projected year end}

\item{fcnetavg.full}{dataframe of all netcapex for all assets defined by netcapex function}

\item{rvanilla}{vector of real vanilla WACC}

\item{remlife}{vector of remaining life of assets from dnsp assets.df input dataframe}

\item{stdlife}{vector of standard life of assets from dnsp assets.df input dataframe}

\item{oavalue}{vector of opening asset value of assets from dnsp assets.df input dataframe}

\item{noyears}{count of years}
}
\description{
Weighted average remaining life method for real depreciation
}
\details{
Function calculates only one asset class: ac, which is specified by user. The function has to be looped to project the WARL depreciation for all asset classes.
Prerequisites that need to be defined in ptrm model, i.e. run above code: ac, yearslabel, fcnetavg.full, rvanilla, remlife, stdlife, oavalue, noyears
}
\keyword{WACC}
\keyword{capex,}
\keyword{depreciation,}
\keyword{remlife,}
\keyword{stdlife,}
\keyword{vanilla}
\keyword{warl,}
