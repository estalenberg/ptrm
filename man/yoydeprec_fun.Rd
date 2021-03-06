% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yoydeprec.R
\name{yoydeprec_fun}
\alias{yoydeprec_fun}
\title{YOY capex function}
\usage{
yoydeprec_fun(y, capexstage, assetclass, stdlife, startyearend,
  fcnetavg.full, rvanilla)
}
\arguments{
\item{y}{vector of years from 2019:projyearend, 2019 gets removed later}

\item{capexstage}{corresponds to the year number or 'years'}

\item{assetclass}{single asset class to model}

\item{stdlife}{standard life for the asset}

\item{startyearend}{2019 or 19}

\item{fcnetavg.full}{dataframe of the forecasted netcapex determined above}

\item{rvanilla}{real vanilla WACC vector by years}
}
\description{
Additive formula for capex depreciation in each year for a single asset.
All assets must be calculated by looping this function
}
\details{
This is used in the year on year depreciation calculations.
Returns the capex for one asset class for all years.

Prerequisites that need to be defined in ptrm model (i.e. run above code):
remlife,oavalue,assetclass
}
\keyword{WACC}
\keyword{asset}
\keyword{capex,}
\keyword{class,}
\keyword{depreciation,}
\keyword{on}
\keyword{real}
\keyword{stdlife,}
\keyword{tracking,}
\keyword{vanilla}
\keyword{year}
