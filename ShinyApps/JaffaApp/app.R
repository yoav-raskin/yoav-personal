# Source: https://stackoverflow.com/questions/44980757/remote-server-authentication-to-read-googlesheets-from-r-script-not-using-servic

# packages ----------------------------------------------------------------

library(googledrive)
library(googlesheets4)

library(shiny)
library(shinydashboard)
library(rsconnect)


library(tidyverse)
library(lubridate)
library(ggstance)
library(janitor)
library(scales)

# Sys.setlocale(category = "LC_ALL", locale = "he")

# Google authentication ---------------------------------------------------

options(gargle_oauth_cache = ".secrets")
# options(gargle_quiet = FALSE)

drive_auth(cache = ".secrets", email = "maayan.morag@gmail.com")
gs4_auth(token = drive_token())


# building datasets -------------------------------------------------------


data_allocations <- read_sheet("https://docs.google.com/spreadsheets/d/1oY7gzTY37DogyAAgGddAdi90WGIk97BZGctifWC6vI8/edit?ouid=105762945762190572888&usp=sheets_home&ths=true") %>%
  mutate(MonthYear = paste0(Month, "-", Year),
         DateTemp = myd(paste0(MonthYear, "-", "01")),
         DateStr = as.character(DateTemp))

data_expanses <- read_sheet("https://docs.google.com/spreadsheets/d/1Lf8JyqDOdrSA_kQmeiU1hAxFcmky4INQIjQGZKOM9Vk/edit?ouid=105762945762190572888&usp=sheets_home&ths=true") %>%
  select(-Timestamp, -`I gave the receipt to Gali!`, -`Is reimbursement required?`) %>% 
  mutate(Year = year(Date),
         Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
  mutate(MonthYear = paste0(Month, "-", Year),
         DateTemp = myd(paste0(MonthYear, "-", "01")),
         DateStr = as.character(DateTemp)) %>% 
  rename(Reimbursement = `If yes, reimbursement amount`)

data_monthly_balance <- tibble()
# date <- "2021-07-01"
# category <- "Food (320084)"

for (date in unique(data_allocations$DateStr)) {
  
  for (category in unique(data_allocations$Category)) {
    
    
    allocation <- data_allocations %>% 
      filter(Category == category,
             DateStr == date) %>%
      chuck("Allocation") 
    
    
    expanses_sum <- data_expanses %>% 
      filter(Category == category,
             DateStr == date) 
    
    expanses <- ifelse(!dim(expanses_sum)[1] == 0,
                        expanses_sum %>%
                          pluck("Amount") %>% sum(),
                        0)  
    
    
    balance <- ifelse(is.null(allocation), 0, allocation) - expanses
    
    
    data_monthly_balance <- bind_rows(data_monthly_balance,
                                      data.frame(
                                        Month = ymd(date),
                                        Category = category,
                                        Balance = balance)
                                      )
  }
}



# UI ----------------------------------------------------------------------



ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "Jaffa Budget App"),
  
  
  #  * sidebar --------------------------------------------------------------
  
  
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(label = NULL,
                  inputId = "Category",
                  choices = unique(data_allocations$Category)),
      
      selectInput(label = NULL,
                  inputId = "MonthYear",
                  choices = unique(data_allocations$MonthYear)),
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  
  
  #  * body -----------------------------------------------------------------
  
  
  
  dashboardBody(
    
    tabItems(
      
      
      #  * * dashboard tab --------------------------------------------------------
      
      tabItem(tabName = "dashboard",
              tags$style(type='text/css', '#headline_box {font-size: 20px;}'),
              
              column(width = 12, align = "center",
                     
                     box(width = 12, status = "info", solidHeader = T,
                         img(src="Jaffa logo.png",
                             width= 136.43,
                             height = 173.6)),
                     
                     # headline box
                     box(width = 12, status = "info", solidHeader = T,
                         verbatimTextOutput(outputId = "headline_box")
                         ),
                     
                     # balance in info box
                     box(width = 12, status = "success", solidHeader = T,
                         infoBoxOutput(outputId = "balance_box")),
                     
                     # balance plot
                     box(width = 12, status = "success", solidHeader = T,
                         plotOutput("balance_plot"))
              )
      ),
      
      
      #  * * tables tab -----------------------------------------------------------
      
      
      
      # specify the tables tab contents in a row
      tabItem(tabName = "tables",
              column(width = 12, align = "center",
                     
                     box(width = 12, status = "info", solidHeader = T,
                         img(src="Jaffa logo.png",
                             width= 136.43,
                             height = 173.6)),
                     
                     # table listing expanses
                     box(title = "רשימת הוצאות בקטגוריה", status = "success", solidHeader = TRUE,
                         tableOutput("category_expanses")),
                     
                     # table listing reimbursements
                     box(title = "פירוט החזרים", status = "success", solidHeader = TRUE,
                         tableOutput("reimbursement"))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # reactive computations ------------------------------------------------------------
  
  past_allocation_category <- reactive(
    
    data_monthly_balance %>%
      filter(Category == input$Category) %>% 
      mutate(Balance = ifelse(Month < dmy(paste0("01-", input$MonthYear)),
                              Balance,
                              0)) %>% 
      pluck("Balance") %>% 
      sum()
  )
  
  allocation_category_month <- reactive(
    
    data_allocations %>% 
      filter(Category == input$Category,
             MonthYear == input$MonthYear) %>% 
      chuck("Allocation")
  )
  
  
  data_expanses_category_month <- reactive(
    data_expanses %>% 
      filter(Category == input$Category,
             MonthYear == input$MonthYear)
    )
  
  
  expanses_sum <- reactive(
    ifelse(!dim(data_expanses_category_month())[1] == 0,
           data_expanses_category_month() %>%
             pluck("Amount") %>% sum(),
           0)
  )
  
  running_balance <- reactive(
    ifelse(is.null(allocation_category_month()),
           0,
           allocation_category_month() + past_allocation_category() - expanses_sum()
    )
  )
  
  # build data based on prev computations for plotting
  data_plot <- reactive(
    bind_rows(
      data.frame(
        Type = "Left",
        Amount = running_balance()),
      data.frame(
        Type = "Spent",
        Amount = expanses_sum())) %>%
      mutate(Amount = ifelse(Amount < 0, 0, Amount))
  )
  
  

# renders -----------------------------------------------------------------

# * headline_box ------------------------------------------------------------
  
  output$headline_box <- renderPrint({ 
    cat("יתרה בתאריך",
        format(today(), format="%A, %d %B %Y"),
        "בקטגוריה",
        input$Category,
        sep = "\n")
    })
  
  
# * balance_box -------------------------------------------------------------------------
  
  output$balance_box <- renderInfoBox({
    
    ## Infobox Spec
    
    infoBox(
      title = input$Category,
      value = running_balance(),
      icon = icon("shekel-sign"),
      color = "green"
    )
  })
  

# * balance_plot ------------------------------------------------------------

  output$balance_plot <- renderPlot({
    
    data_plot() %>%
      
      ggplot(aes(x = input$Category, y = Amount, fill = factor(Type, levels = c("Left", "Spent")))) +
      geom_col() +
      
      scale_fill_manual(values = c("Left" = "#32CD32", "Spent" = "#FF0000"),
                        labels = c("Left" = "יתרה",
                                   "Spent" = "הוצאות" )) +
      
      ylab("שקלים") +
      
      theme_minimal() +
      theme(
        plot.title = element_text(size = (13.5), hjust = 0.5),
        axis.title = element_text(size = 13.5),
        axis.title.y = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank()
      )
    
  })
  
# * category_expanses ------------------------------------------------------------
  
  
  
  output$category_expanses <- renderTable({
    data_expanses %>% 
      filter(Category == input$Category,
             MonthYear == input$MonthYear) %>% 
      mutate(Date = as.character(Date)) %>% 
      select(Name, Date, Amount, Details)
    
  })
  
  

# * reimbursement -----------------------------------------------------------

  
  
  output$reimbursement <- renderTable({
    
    data_expanses %>% 
      filter(MonthYear == input$MonthYear) %>% 
      group_by(Name) %>% 
      summarise(Reimbursement = sum(Reimbursement, na.rm = TRUE))
    
    
  })
  


}

# test app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)




  










