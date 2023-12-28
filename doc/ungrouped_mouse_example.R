## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(ShollBayes)
library(dplyr)

## -----------------------------------------------------------------------------
#load data
sholl_data = ShollBayes::ug3_data

## -----------------------------------------------------------------------------
m = load_model("ug3_model")

## -----------------------------------------------------------------------------
m = restructure_data(m,
                     y = sholl_data$y,
                     R = sholl_data$R,
                     Cell_ID = sholl_data$Cell_ID,
                     Image_ID = sholl_data$Image_ID,
                     Animal_ID = sholl_data$Animal_ID)

## -----------------------------------------------------------------------------
set.seed(293402)
m = initialize_model(m, n.chains = 4, n.adapt = 200)

set.seed(123901)
update_model(m, n.iter = 200)

set.seed(349058)
m = sample_model(m, n.iter = 500, new_sample = F)

