
library(devtools)


# load packages -----------------------------------------------------------

# get func code from Git
source_url("https://raw.githubusercontent.com/yoav-raskin/yoav-personal/main/mechina_budget_shiny/load_packages.R")
load_packages()

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

# source_url("https://raw.githubusercontent.com/yoav-raskin/yoav-personal/main/mechina_budget_shiny/build_ui.R")
source("mechina_budget_shiny/build_ui.R") 

build_ui(app_name = "Jaffa Budget App",
         logo_png = "Jaffa logo.png")

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
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Expanses Excel", tabName = "excel", icon = icon("file-excel"))
      
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
      ),
      #  * * excel tab -----------------------------------------------------------
      
      tabItem(tabName = "excel",
              column(width = 12, align = "center",
                     
                     box(title = "דוח קופה קטנה", width = 12, status = "info", solidHeader = T,
                         DTOutput("excel")
                     )
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
  
  # building table for excel output
  data_expanses_monthly_all_categories <- reactive(data_expanses %>% 
                                                     filter(MonthYear == input$MonthYear,
                                                            `How did you pay?` == "Kupah K'tana") %>%
                                                     pivot_wider(id_cols = c("Name", "Date", `Receipt Number`, "Details"),
                                                                 names_from = Category,
                                                                 values_from = Amount)
  )
  
  sum_row <- reactive(data_expanses_monthly_all_categories() %>%
                        select(where(is.numeric)) %>% 
                        summarise(
                          across(.fns = ~sum(.x, na.rm = TRUE))
                        ) %>%
                        mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE))

  )
  
  excel_output <- reactive(
    data_expanses_monthly_all_categories() %>% 
      bind_rows(sum_row())
    
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
  
  # * excel -----------------------------------------------------------
  
  
  output$excel <- renderDT(server = FALSE, { # print all rows
    
    
    
    datatable(
      excel_output(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('excel')
      )
    ) %>%
      formatDate(columns = c("Date"),
                 method = "toLocaleDateString",
                 params = list(
                   'en-GB',
                   list(
                     year = 'numeric',
                     month = 'numeric',
                     day = 'numeric')
                 )
      )

  })
  
  


}

# test app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)




  










