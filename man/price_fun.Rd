% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/price_fun.R
\name{price_fun}
\alias{price_fun}
\title{Price per GWh and per customer}
\usage{
price_fun(dnsp.in, df.real, energyvol.df, other.df, priceprof.df,
  projyearend.in, cust.in, signal.in)
}
\arguments{
\item{dnsp.in}{DNSP selected for function loop}

\item{df.real}{dataframe of real revenue calculated from ptrm_fun}

\item{energyvol.df}{dataframe of relative energy volumes from allenergyvol_fun}

\item{other.df}{dataframe of static dnsp inputs}

\item{priceprof.df}{dataframe of static dnsp price profile inputs}

\item{projyearend.in}{dynamic variable of final year}

\item{cust.in}{dynamic variable of customer growth percentage}

\item{signal.in}{dynamic variable of price signal text input}
}
\description{
Formulas for calculating the relative price for the six types of customers
}
