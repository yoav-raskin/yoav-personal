library(emayili)
library(tidyverse)


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
  to("yoav.r@kayma.com") %>% 
  subject("your class data is here!") %>%
  emayili::render("playground/emayili/class_report_emayili.Rmd", params = list(current_class = "D"))
  


smtp(msg, verbose = TRUE)

