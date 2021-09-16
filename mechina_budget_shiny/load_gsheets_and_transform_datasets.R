load_gsheets_and_transform_datasets <- function(allocation_sheet_url, expanses_sheet_url, gave_the_recipt_field_name) {

# building datasets -------------------------------------------------------


data_allocations <- read_sheet(allocation_sheet_url) %>%
  mutate(MonthYear = paste0(Month, "-", Year),
         DateTemp = myd(paste0(MonthYear, "-", "01")),
         DateStr = as.character(DateTemp))

data_expanses <- read_sheet(expanses_sheet_url) %>%
  select(-Timestamp, -gave_the_recipt_field_name, -`Is reimbursement required?`) %>% 
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

}

