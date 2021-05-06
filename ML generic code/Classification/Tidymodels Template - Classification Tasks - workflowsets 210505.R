library(tidymodels)
library(tidyverse)
library(workflowsets)
library(recipeselectors)
library(mlbench)
library(glmnet)
library(keras)
library(baguette)
library(rules)
library(parsnip)
library(ranger)
library(randomForest)
library(kernlab)
library(discrim)

setwd("ML generic code/Classification")


# 1 Load Data (Source: UCI ML Repository) & Models' Metadata (from parsnip website) -------------------------------------------------------------------------

  data(BreastCancer)
  data <- BreastCancer %>% 
    remove_missing()
  rm(BreastCancer)
  

  model_selector <- c("logistic_reg")
  model_metadata <- readxl::read_xlsx("models.xlsx") #%>%
    # filter(!model %in% model_selector)
  

# * 4.1 Sampling Data (create rsample objects) -------------------------------------------------------------------------
  
  
  # Create data split object
  split_object <- initial_split(data,
                               prob = 3/4,
                               strata = all_of(target))
  
  # Build training data
  training <- split_object %>% 
    training()
  
  # Build test data
  test <- split_object %>% 
    testing()
  
  # Control fold number
  fold_n <- 5
  # Create cross validation folds
  set.seed(290)
  folds <- vfold_cv(training, v = fold_n,
                          strata = all_of(target))


# * 4.2 Feature Engineering (create 2 alternative recipe objects)-------------------------------------------------------------------------
  
  # Control target
  target <- "Class"
  
  # Build base_recipe
  base_recipe <- recipe(as.formula(paste0(target, "~.")),
                        data = training) %>%
    #Change outcome to Yes\No String
    step_mutate(Class = str_replace(Class, "malignant", "Yes"),
                Class = str_replace(Class, "benign", "No")) %>% 
    #Remove id
    step_rm(Id) %>% 
    
    #Transform to numeric class
    step_integer(all_nominal(), -all_outcomes())
  
  base_recipe
  
  # Build cor_recipe 
    cor_recipe <- base_recipe %>%
    # Correlation filter
    step_corr(all_numeric_predictors(), threshold = tune()) %>% 
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
    
    #View baked training dataset
    prep(cor_recipe) %>%
      bake(new_data = training)
    
  # Build pca_recipe 
    pca_recipe <- base_recipe %>% 
    # pcs step
    step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
   
    #View baked training dataset
    prep(pca_recipe) %>%
      bake(new_data = training)
    
    
    # Supervised Feature Selection
    # step_select_infgain(all_predictors(), outcome = "Class", threshold = 0.5)
  

# Build parsnip models ----------------------------------------------------

  bag_mars_model <- 
    bag_mars(num_terms = tune(),
             prod_degree = tune(),
             prune_method = tune()) %>% 
    set_engine("earth")
  
  bag_tree_model <- 
    bag_tree(cost_complexity = tune(),
             tree_depth = tune(),
             min_n = tune(),
             class_cost = tune()) %>% 
    set_engine("rpart")
  

# Build workflowset -------------------------------------------------------

  
  
  

# * 4.3 Specify a Tuning Classification Model (create parsnip object) -------------------------------------------------------------------------
  
  #Pluck specific parms and engine for the model
  parms <- model_metadata %>% 
    filter(model %in% m) %>% 
    select(parameter)
  
  engine <- model_metadata %>% 
    filter(model %in% m) %>% 
    select(engine)
  
  # Build tune model function as one string
  loop_string_i <- as.character()
  
  
  for (i in as.character(parms$parameter)) {
    
  loop_string_i <- paste(paste(i, "= tune()"),
                         ",",
                         loop_string_i)
  }
  

  loop_string <- paste(
    paste(m, "("),
    str_sub(loop_string_i, end = -3),
    ")") %>%
    
    str_replace("  ", "") %>% 
    str_replace(" ", "")
  
  # Insert string to model function  
  tune_model <- eval(parse(
    text = loop_string
    ))  %>%
    # Specify engine
    set_engine(engine$engine[1]) %>%
    # Specify mode
    set_mode('classification')
  

    

# * 4.4 Integrate to a Tune Workflow (create workflow object) -------------------------------------------------------------------------

  # Create a workflow
  tune_wf <- workflow() %>% 
    # Include the model object
    add_model(tune_model) %>% 
    # Include the recipe object
    add_recipe(recipe)
  
  tune_wf
  

# 5 Specify Tuning Helpers & Run Tuning ----------------------------------------------------------------


 

# * 5.1 Build a Grid of hyperparameters Values (create a tibble with dials) -------------------------------------------------------------------------

  # Hyperparameter tuning with grid search
  set.seed(214)
  grid <- grid_random(parameters(tune_model),
                         size = 5)
  

# * 5.2 Create a Custom Metric Function (with yardstick) -------------------------------------------------------------------------
  
  metrics <- metric_set(yardstick::roc_auc,
                        yardstick::sens,
                        yardstick::spec)
  
  
# * 5.3 Run Tuning Workflow & Select Best Model on roc_auc Metric (with tune) -------------------------------------------------------------------------

  best_model_data_m <- tune_wf %>% 
    tune_grid(resamples = folds,
              grid = grid,
              metrics = metrics) %>% 
    select_best(metric = 'roc_auc') 
  

  best_model_data[[m]] <- best_model_data_m
  
# 6 Finalize (Update) the Tune Workflow + Train (Training) & Evaluate (Test) (with tune) -------------------------------------------------------------------------

  
  # Finalize your workflow
  final_fit <- tune_wf %>% 
    finalize_workflow(best_model_data_m) %>%
    last_fit(split = split_object)
  
  rm(best_model_data_m)
  

  
# 7 Analyze Metrics (with tune) -------------------------------------------------------------------------


  # View performance metrics
  metrics_data_m <- final_fit %>% 
    collect_metrics() %>% 
    mutate(Model = m)
  
  metrics_data <- bind_rows(metrics_data, metrics_data_m)
  rm(metrics_data_m)
  
  # Create an ROC curve
  roc_curve_data_m <- final_fit %>% 
    # Collect predictions
    collect_predictions() %>%
    mutate(Class = factor(Class, levels = c("Yes", "No"))) %>% 
    # Calculate ROC curve metrics
    roc_curve(truth = Class, .pred_Yes) %>% 
    mutate(Model = m)

  
  roc_curve_data <- bind_rows(roc_curve_data, roc_curve_data_m)
  rm(roc_curve_data_m)
  
  print(paste(m, " Results Collected Sir!"))
    
}
    

# Plot Multiple Curves ----------------------------------------------------


roc_curve_data %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = Model)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()


ggsave(paste0("C:/Users/yoavr/Documents/roc curve ", as.character(Sys.Date()),".png"),
       width = 13,
       height = 8
       )

metrics_data %>% 
  write_csv(paste0("C:/Users/yoavr/Documents/metrics data ", as.character(Sys.Date()),".csv"))
