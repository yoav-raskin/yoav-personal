build_server <- function() {

server <<- function(input, output, session) {
  
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

}
