
# load packages -----------------------------------------------------------

library(googledrive)
library(googlesheets4)

library(shiny)
library(shinydashboard)
library(rsconnect)


library(tidyverse)
library(tidyselect)
library(lubridate)
library(ggstance)
library(janitor)
library(scales)
library(DT)

library(devtools)

# Google authentication ---------------------------------------------------

options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE)

drive_auth(cache = ".secrets", email = "maayan.morag@gmail.com")
gs4_auth(token = drive_token())


# building datasets -------------------------------------------------------

# get func code from Git
source_url("https://raw.githubusercontent.com/yoav-raskin/yoav-personal/main/mechina_budget_shiny/load_gsheets_and_transform_datasets.R")

load_gsheets_and_transform_datasets(allocation_sheet_url = "https://docs.google.com/spreadsheets/d/1oY7gzTY37DogyAAgGddAdi90WGIk97BZGctifWC6vI8/edit?ouid=105762945762190572888&usp=sheets_home&ths=true",
                                    expanses_sheet_url = "https://docs.google.com/spreadsheets/d/1Lf8JyqDOdrSA_kQmeiU1hAxFcmky4INQIjQGZKOM9Vk/edit?ouid=105762945762190572888&usp=sheets_home&ths=true",
                                    gave_the_recipt_field_name = "I gave the receipt to Gali!")


# UI ----------------------------------------------------------------------

source_url("https://raw.githubusercontent.com/yoav-raskin/yoav-personal/main/mechina_budget_shiny/build_ui.R")
# source("mechina_budget_shiny/build_ui.R") 

build_ui(app_name = "Jaffa Budget App",
         logo_png = "Jaffa logo.png")


# server ------------------------------------------------------------------

source_url("https://raw.githubusercontent.com/yoav-raskin/yoav-personal/main/mechina_budget_shiny/build_server.R")
# source("mechina_budget_shiny/build_server.R") 

build_server()


# test app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)




  










