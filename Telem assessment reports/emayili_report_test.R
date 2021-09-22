library(emayili)
library(tidyverse)
library(here)
library(rmarkdown)


# define email server on Gmail --------------------------------------------

# user_name <- *********
# pswd <- ******


smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = user_name,
               password = pswd)


# build msg object --------------------------------------------------------


msg <- envelope() %>% 
  from("yoav.raskin@gmail.com") %>%
  to("maayan.morag@reform.org.il") %>% 
  subject("Your report is here!") %>%
  emayili::render(here::here("Telem assessment reports", "telem_assessment_report_tashpab.Rmd"))



smtp(msg, verbose = TRUE)


# -------------------------------------------------------------------------


rmarkdown::render(here::here("Telem assessment reports", "telem_assessment_report_tashpab.Rmd"), encoding = "UTF-8")


