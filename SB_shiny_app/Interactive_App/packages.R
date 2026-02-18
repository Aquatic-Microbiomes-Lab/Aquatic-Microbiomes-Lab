# Safe packages for deployment on shinyapps.io
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(bslib)
library(DT)
library(htmltools)
library(ggrepel)
library(egg)
library(ggthemes)
library(leaflet)

# Prevent CRAN mirror errors on deploy
options(repos = c(CRAN = "https://cran.rstudio.com/"))