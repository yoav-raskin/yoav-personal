
build_ui <- function(app_name, logo_png) {

# UI ----------------------------------------------------------------------



ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = app_name),
  
  
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
                         img(src = logo_png,
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

}
