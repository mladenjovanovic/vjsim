% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/profiles.R
\name{get_all_profiles}
\alias{get_all_profiles}
\title{Get All Profiles}
\usage{
get_all_profiles(profile_data)
}
\arguments{
\item{profile_data}{Data frame returned from \code{\link{vj_profile}} function}
}
\value{
List with two elements: list of profiles and long data frame
}
\description{
Generates all Force-Velocity profiles and stores them in the list and data.frame
}
\examples{
fv_profile <- vj_profile(mass = 75)
all_profiles <- get_all_profiles(fv_profile)
all_profiles$data_frame
}
