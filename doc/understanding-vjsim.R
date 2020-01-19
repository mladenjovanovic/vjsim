## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center",
  fig.width = 6,
  fig.height = 4,
  #out.width = "70%",
  auto_pdf = TRUE,
  message = FALSE,
  warning = FALSE
)

## ---- echo=FALSE, mechanical-model, out.width="80%", fig.cap="Mechanical model that is used to represent vertical jump"----
knitr::include_graphics(path = "mechanical-model.png")

## ---- echo=FALSE, force-generator, out.width="80%", fig.cap="Force Generator components"----
knitr::include_graphics(path = "force-generator-components.png")

## ----setup--------------------------------------------------------------------
library(vjsim)
library(tidyverse)

## -----------------------------------------------------------------------------

 x <- seq(0, 0.5, length.out = 1000)

 y1 <- fgen_force_length(
   current_distance = x,
   start_force = 0.6,
   threshold = 0.90,
   end = 0
 )

 plot(x, y1, "l")

 y2 <- fgen_force_length(
   current_distance = x,
   start_force = 0.8,
   threshold = 0.95,
   end = 0
 )

 lines(x, y2, col = "red")

