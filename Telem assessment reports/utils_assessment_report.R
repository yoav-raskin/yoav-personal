
# generic_col_names -------------------------------------------------------------------------

generic_col_names <- function(original_google_sheet) {
  

header_code <<- tibble(
  var_name = names(original_google_sheet)) %>% 
  rowid_to_column() %>% 
  mutate(rowid = paste0("var_", rowid)
  )
view(header_code)

names(original_google_sheet) <- header_code$rowid
d <<- original_google_sheet

}


# likert_recode -------------------------------------------------------------------------

likert_recode <- function(x) {
  
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Very low",
                     ifelse(x == 2, "Low",
                            ifelse(x == 3, "Neutral",
                                   ifelse(x == 4, "High",
                                          "Very high")))))
  
  y <- factor(y, levels = c("Very low", "Low", "Neutral", "High", "Very high"))
  
  return(y)
}


# make_likert_bar_plot -------------------------------------------------------------------------

plot_likert_bar <- function(data, col_names_vector) {
  
  d_plot <- data %>%
    #renaming
    rename(replace = renaming_likert_items) %>% 
    rename(replace = renaming_factors) %>% 
  
    # select items
    select(col_names_vector) %>% 
    # verify item is likert
    select(
      where(~ is_numeric(.x) && min(.x) >= 1 && max(.x) <= 5))
  
  d_plot %>% 
    mutate_all(likert_recode) %>%
    as.data.frame() %>%
    likert() %>% 
    plot( 
      # Group the items alphabetically
      group.order = names(d_plot),
      # Plot the percentages for each response category
      plot.percents = TRUE,
      # Plot the total percentage for negative responses
      plot.percent.low = T,
      # Plot the total percentage for positive responses
      plot.percent.high = T,
      # Whether response categories should be centered
      # This is only helpful when there is a middle response
      # option such as "neutral" or "neither agree nor disagree"
      centered = T,
      # Wrap label text for item labels
      wrap=30)
  
}


# plot_alluvium_by_one_variable -------------------------------------------------------------------------

plot_alluvium_by_one_variable <- function(data, comparative_factor, likert_item) {

d_plot <- data %>% 
  rename(replace = renaming_likert_items) %>%
  rename(replace = renaming_factors) %>%
  
  select({{comparative_factor}}, {{likert_item}}) %>%
  mutate_if(is.numeric, likert_recode) %>% 
  
  group_by({{comparative_factor}}, {{likert_item}}) %>%
  dplyr::summarise(
    freq = n()
    )

comparative_factor_str <- deparse(substitute(comparative_factor))
likert_item_str <- deparse(substitute(likert_item)) 

d_plot %>%
  ggplot(aes(y = freq, axis1 = {{comparative_factor}}, axis2 = {{likert_item}})) +
  geom_alluvium(aes(fill = {{comparative_factor}}), width = 1/12) +
  geom_stratum(width = 2/12, fill = "gray", color = "white") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c(comparative_factor_str, likert_item_str), expand = c(.1, .1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  ylab("Number of respondents") +
  ggtitle(paste0(likert_item_str, " item by ", comparative_factor_str))

}

.

# plot_means_and_error_by_one_variable -------------------------------------------------------------------------

plot_means_and_error_by_one_variable <- function(data, comparative_factor, likert_items_vector) {
  
  d_plot <- tibble()
  for (i in likert_items_vector) {
    
  d_plot_i <- data %>% 
    rename(replace = renaming_likert_items) %>%
    rename(replace = renaming_factors) %>%
    
    select({{comparative_factor}}, i) %>%
    
    group_by({{comparative_factor}}) %>%
    dplyr::summarise(
      mean = mean(get(i)),
      sd = sd(get(i))
    ) %>%
    mutate(item = i)
  
  d_plot <- bind_rows(d_plot, d_plot_i)
}
 
  comparative_factor_str <- deparse(substitute(comparative_factor))
  
  
  d_plot %>%
    ggplot(aes(x = {{comparative_factor}}, y = mean, ymax = mean + sd, ymin = mean - sd, color = {{comparative_factor}})) +
    geom_linerange(alpha = 0.4, size = 2) +
    geom_point(size = 2.5) +
    ylim(0, 6) +
    facet_wrap(~item) +
    ylab(paste0("Mean value of item")) +
    ggtitle(paste0("Item means by ", comparative_factor_str)) +
  
    theme_bw()
    
  
}


# plot_factor -------------------------------------------------------------------------


plot_factor <- function(data, factor) {
  

    d_plot <<- data %>% 
      rename(replace = renaming_factors) %>%

      select({{factor}}) %>%
      
      group_by({{factor}}) %>%
      dplyr::summarise(
        freq = n()
      ) %>%
      mutate(rel_freq = freq / sum(freq) * 100)
    

  d_plot %>%
    ggplot(aes(x = reorder({{factor}}, desc(rel_freq)), y = rel_freq, fill = {{factor}})) +
    geom_col() +
    geom_line(aes(group = 1), size = 1.5, alpha = 0.7, col = "gray") +
    # ylim(0, 101) +
    ylab(paste0("Share of respondents [%]")) +
    xlab(deparse(substitute(factor))) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 20))
  
  
}





