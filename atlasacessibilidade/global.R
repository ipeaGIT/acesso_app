
# load packages ---------------------------
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(mapdeck)
library(shinyBS)
# library(dplyr)
library(sf)
# library(readr)
library(data.table)
library(waiter) # remotes::install_github("JohnCoene/waiter")
library(shiny.i18n) # devtools::install_github("Appsilon/shiny.i18n")


# library(forcats)
library(highcharter)
# library(hrbrthemes) # remotes::install_github("hrbrmstr/hrbrthemes")
# library(tidyr)

# load translator data ---------------------------
translator <- Translator$new(translation_json_path = "data/translation.json")

# load functions ---------------------------
source("R/create_radio_button_custom.R")
source("R/label_with_info.R")
source("R/slider_input_acess.R")

# Use GForce Optimisations in data.table operations
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# register mapbox api key ---------------------------
# set_token("")
set_token("pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")
