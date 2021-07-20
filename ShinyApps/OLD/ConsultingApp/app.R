# Source: https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic


library(googledrive)
library(googlesheets4)
library(dplyr)
library(shiny)
library(lubridate)
library(ggplot2)
library(ggstance)
library(rsconnect)
library(janitor)
library(scales)
library(shinydashboard)
library(gt)
library(scales)

## Auth ##

options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE)

drive_auth(cache = ".secrets", email = "yoav.raskin@gmail.com")
gs4_auth(token = drive_token())

## Database ##

data <- gs4_find("Consulting Hours") %>%
  read_sheet()

app_data <- data %>%
  left_join(data.frame(
    Client = c("Mobility Partnership", "ERI", "Kayma"),
    WagePerHour = c(150, 200, 200)
  ),
  by = "Client") %>% 
  mutate(Month = month(Date),
         Year = year(Date),
         YearMonth = paste0(Year, "-", Month),
         Seconds = as.numeric(as.duration(End - Start)),
         Wage = Seconds * WagePerHour / 3600)



## ui ##


ui <- dashboardPage(skin = "purple",

  dashboardHeader(title = "Consulting Hours App"),

  dashboardSidebar(
    sidebarMenu(

      selectInput(label = "Select Year & Month",
                  inputId = "year_month",
                  choices = rev(unique(app_data$YearMonth))),
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Entries", tabName = "entries", icon = icon("table"))

    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              column(width = 12, align = "center",
                     infoBoxOutput("total_wage"),
                     box(title = "Wage per Client", width = 12, status = "info", solidHeader = TRUE,
                         plotOutput("wage_per_client_plot")
                     )
                     
              )
      ),
      tabItem(tabName = "entries",
              fluidRow(
                box(title = "List of Entries", status = "success", solidHeader = TRUE, 
                    gt_output("entries"))
              )
      )
    )
  )
)




## server ##


server <- function(input, output, session) {
  

## Renders ##
    
  output$total_wage <- renderInfoBox({
    
    ## Computations 
    
    total_wage_per_month <- as.numeric(
      app_data %>% 
        filter(YearMonth == input$year_month) %>%
        select(Wage) %>% 
        sum()
    )
    
    
    ## Infobox Spec
    
    infoBox(
      paste0("Total Wage in ", input$year_month),
      total_wage_per_month,
      icon = icon("shekel-sign"),
      color = "green"
    )
  })
    
    output$wage_per_client_plot <- renderPlot({
      
      ## Computations
      
      plot_data <- data.frame()
      
      for (client in unique(app_data$Client)) {
        
        data_i <- app_data %>% 
          filter(YearMonth == input$year_month,
                 Client == client) %>%
          arrange(Date) %>% 
          mutate(CumWage = cumsum(Wage))
        
        plot_data <- rbind(plot_data, data_i)
        
        
      }
      

       ## gg
     
      ggplot(plot_data, aes(y = CumWage, x = Date, col = Client)) +
        geom_line(alpha = 3, size = 2) +
        geom_point(color = "black", size = 2) +
        xlab("Date") +
        ylab("Amount [NIS]") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = (13.5), hjust = 0.5),
          axis.title = element_text(size = 13.5),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"),
          legend.position= "bottom"
          )
      
    })
    
    output$entries <- render_gt({
      
      app_data %>% 
        filter(YearMonth == input$year_month) %>%
        mutate(Date = as_date(Date),
               Hours = Seconds / 3600) %>% 
        select(Client, Date, Hours, Wage) %>% 
        arrange(desc(Date)) %>%
        
        gt() %>%
        fmt_currency(
          columns = vars(Wage),
          currency = "sheqel"
        ) %>% 
        fmt_date(
          columns = vars(Date),
          date_style = 12
        ) %>% 
        fmt_number(
          columns = vars(Hours),
          suffixing = TRUE,
          decimals = 1
        ) %>% 
        data_color(
          columns = vars(Client),
          colors = scales::col_factor(
            palette = "Set1",
            domain = unique(app_data$Client)
          )
        ) %>% 
        cols_align(
          "center",
          columns = TRUE
        )
      
      
    })
    


    
}

# Run the application 
shinyApp(ui = ui, server = server)







