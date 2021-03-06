% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profiles.R
\name{make_load_profile}
\alias{make_load_profile}
\title{Make Load Profile}
\usage{
make_load_profile(
  bodyweight,
  external_load,
  height,
  aerial_time,
  gravity_const = 9.81,
  plot = TRUE
)
}
\arguments{
\item{bodyweight}{Numeric value}

\item{external_load}{Numeric vector}

\item{height}{Numeric vector}

\item{aerial_time}{Numeric vector}

\item{gravity_const}{Numeric vector. Default 9.81}

\item{plot}{TRUE/FALSE. Default is TRUE}
}
\value{
List of L-TOV profile parameters
}
\description{
\code{make_load_profile} generates Load-TOV (take-off velocity) profile from squat jump data. When both \code{aerial_time} and
    \code{height} are forwarded to the function, only \code{height} is used to estimate TOV.
}
\examples{
require(tidyverse)

data("testing_data")

with(
  filter(testing_data, athlete == "Jack"),
  make_load_profile(
    bodyweight = bodyweight,
    external_load = external_load,
    aerial_time = aerial_time,
    plot = TRUE
  )
)
}
