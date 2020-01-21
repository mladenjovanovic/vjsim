## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4,
  dpi = 300,
  out.width = "90%",
  auto_pdf = TRUE,
  message = FALSE,
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
# Install vjsim if you haven't already by running the following commands
# install.packages("devtools")
# devtools::install_github("mladenjovanovic/vjsim")

# Install tidyverse and cowplot packages
# install.packages(c("tidyverse", "cowplot"), dependencies = TRUE)

library(vjsim)
library(tidyverse)
library(cowplot)

## ----eval=FALSE---------------------------------------------------------------
#  vignette("introduction-vjsim")

