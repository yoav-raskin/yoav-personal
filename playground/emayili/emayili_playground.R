library(emayili)
library(tidyverse)

# user_name <- *********
# pswd <- ******

# looping over classes to build rmds --------------------------------------

# render_one <- function(current_class) {
#   # assuming the output format of input.Rmd is PDF
#   rmarkdown::render(
#     "playground/emayili/class_report_emayili.Rmd",
#     output_file = paste0("class_", current_class, ".html"),
#     params = list(current_class = current_class),
#     envir = parent.frame()
#   )
# }
# 
# for (current_class in c("A", "B", "C", "D", "E")) {
#   render_one(current_class)
# }


# define email server on Gmail --------------------------------------------

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = user_name,
               password = pswd)


# build msg object --------------------------------------------------------

rm(params)

msg <- envelope() %>% 
  from("yoav.raskin@gmail.com") %>%
  to("yoav.r@kayma.com") %>% 
  subject("your class data is here!") %>%
  emayili::render("playground/emayili/class_report_emayili.Rmd", params = list(current_class = "D"))
  


smtp(msg, verbose = TRUE)

