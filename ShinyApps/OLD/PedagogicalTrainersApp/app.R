# Source: https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic


library(googledrive)
library(googlesheets4)
library(dplyr)
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggstance)
library(rsconnect)
library(janitor)
library(scales)



options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE)

drive_auth(cache = ".secrets", email = "yoav.raskin@gmail.com")
gs4_auth(token = drive_token())


## Force Classes 

factor <- c("MeasurementCycle")

## Read Sheets Data

raw_data <- gs4_find("Pedagogical Trainer Form") %>%
  read_sheet(col_names = FALSE)

data <- raw_data %>% 
  row_to_names(row_number = 2) %>% 
  dplyr::filter(!is.na(Teacher)) %>% 
  mutate_at(11:ncol(raw_data), as.numeric) %>% 
  mutate_at(factor, as.character)

heb_data <- raw_data %>%
  row_to_names(row_number = 1) %>%
  names()


ui <- fluidPage(
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Data Input", htmlOutput("googleSheet")),
                tabPanel("Teacher Comparison",
                         selectInput(inputId = "Item",
                                     label = "Select Item:",
                                     choices = names(data)[11:length(names(data))]),
                         
                         plotOutput("TeacherComparison",
                                    width = "75%",
                                    height = "400px"))
    )
  )
)




  
  
server <- function(input, output, session) {
  
  ## Renders
  
  googleSheet_embed_link <- "https://docs.google.com/spreadsheets/d/1wFA-T4wPiIg77DPlcjKzefMlpyJbbxyla-DCeZUdRZ0/edit?usp=sharing"
  
  output$googleSheet <- renderUI({
    tags$iframe(id = "googleSheet",
                src = googleSheet_embed_link,
                width = 1024,
                height = 768,
                frameborder = 0,
                marginheight = 0)
  })
  
  
  output$TeacherComparison <- renderPlot({
    
    
    data %>% 
      dplyr::select(1, 2, input$Item) %>% 
      rename("Measure" = 3) %>% 
      
      
      ggplot(aes(x = as.numeric(MeasurementCycle), y = Measure, col = Teacher)) +
      geom_point(size = 3) +
      geom_line(size = 3, alpha = 0.4) +
      theme_minimal() +
      labs(col = heb_data[1]) +
      xlab(heb_data[2]) +
      ylab("Implementation Level") +
      scale_x_continuous(breaks = c(1, 2, 3)) +
      theme(plot.title = element_text(size = (13.5), hjust = 0.5),
            axis.title = element_text(size = 13.5),
            axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
            axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text = element_text(size = 12),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
    
    
  })
  
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)





