library(shiny)
library(shinyWidgets)
library(mapdeck)
library(shinyBS)
library(dplyr)
library(sf)
library(readr)
library(data.table)
# library(ggplot2)
library(waiter) # remotes::install_github("JohnCoene/waiter")
library(shiny.i18n)

translator <- Translator$new(translation_json_path = "data/translation.json")

source("teste_div.R")

# Use GForce Optimisations in data.table operations
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)


# register mapbox api key
# set_token("")
set_token("pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ")



# TIMEOUT -------------------------------------------------------------------------------------

timeoutSeconds <- 20

inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)
