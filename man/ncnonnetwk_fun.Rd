% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncnonnetwk.R
\name{ncnonnetwk_fun}
\alias{ncnonnetwk_fun}
\title{Netcapex functions: v Non-network}
\usage{
ncnonnetwk_fun(noassets, yearslabel, fcnetcapex, assetcode)
}
\arguments{
\item{noassets}{Count of asset classes}

\item{yearslabel}{Label of years from 2020}

\item{fcnetcapex}{Dataframe of PTRM input net capex for 2020 to 2024}

\item{assetcode}{If asset class is coded as 1 or 2}
}
\description{
Fifth formula for extending netcapex beyond five years March 22 2019
}
\details{
Returns a database of projected netcapex:v non-network for all assets across all years.

Prerequisites that need to be defined in ptrm model (i.e. run above code) are defined in arguments below.
}
\keyword{asset}
\keyword{capex,}
\keyword{capital}
\keyword{class}
\keyword{dnsp,}
\keyword{netcapex,}
\keyword{overhead,}
\keyword{system}
