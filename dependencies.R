# This file allows packrat (used by rsconnect during deployment) to pick up dependencies.

# Tabular data
library(dplyr)
library(tidyr)
library(vroom)

# General tools
library(rlang)
library(purrr)
library(lubridate)
library(stringr)

# Shiny
library(rhino)
library(shiny)
library(bslib)
library(bsicons)
library(shiny.router)
library(shinyjs)
library(shinyFeedback)
library(shinyWidgets)
library(htmlwidgets)

# Data storage
library(googledrive)
library(googlesheets4)

# Path management
library(here)
library(dotenv)

# Visualisation
library(echarts4r)
# probably for export later?
# library(ggplot2)
