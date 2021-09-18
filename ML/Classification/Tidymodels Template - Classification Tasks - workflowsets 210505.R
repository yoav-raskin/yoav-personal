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


# Load Data (Source: UCI ML Repository) & Models' Metadata (from parsnip website) -------------------------------------------------------------------------

  data(BreastCancer)
  data <- BreastCancer %>% 
    remove_missing()
  rm(BreastCancer)
  
# Sampling Data (create rsample objects) -------------------------------------------------------------------------
  
  
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


# Feature Engineering (create 2 alternative recipe objects)-------------------------------------------------------------------------
  
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
  
  # Build cor_recipe 
    cor_recipe <- base_recipe %>%
    # Correlation filter
    step_corr(all_numeric_predictors(), threshold = tune()) %>% 
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
    

  # Build pca_recipe 
    pca_recipe <- base_recipe %>% 
    # pcs step
    step_pca(all_numeric_predictors(), num_comp = tune()) %>% 
    # Normalize numeric predictors
    step_normalize(all_numeric_predictors())
   
    
    # Supervised Feature Selection
    # step_select_infgain(all_predictors(), outcome = "Class", threshold = 0.5)
  

# Build models (create 2 alternative parsnip objects) ----------------------------------------------------

  bag_mars_model <- 
    bag_mars(num_terms = tune(),
             prod_degree = tune(),
             prune_method = tune()) %>% 
    set_engine("earth") %>% 
    set_mode('classification')
    
  
  bag_tree_model <- 
    bag_tree(cost_complexity = tune(),
             tree_depth = tune(),
             min_n = tune(),
             class_cost = tune()) %>% 
    set_engine("rpart") %>% 
    set_mode('classification')
  
  

# Build workflowset & Run on Random Grid -------------------------------------------------------

  workflowset <- workflow_set(
    
    preproc = list(
      cor = cor_recipe,
      pca = pca_recipe),
    
    models = list(
      bag_mars = bag_mars_model,
      bag_tree = bag_tree_model),
    
    cross = T) 
  
  tuned_workflowset <- workflow_map(workflowset, "tune_grid", grid = 30, 
                                    resamples = folds, 
                                    metrics = metric_set(yardstick::roc_auc),
                                    verbose = TRUE)
  #Analyze Metrics
  autoplot(tuned_workflowset, select_best = TRUE)
  
  

# Select Winning Model -------------------------------------------
    
    best_model <- rank_results(tuned_workflowset, rank_metric = "roc_auc", select_best = TRUE) %>% 
      filter(.metric == "roc_auc",
             rank == 1)
    
    best_workflow <- pull_workflow(tuned_workflowset,
                                   id = best_model$wflow_id)
    
    
# Final Model: Train (Training) & Evaluate (Test) (with tune) -------------------------------------------------------------------------

  
    final_fit <- best_workflow %>%  
      
      finalize_workflow(
        #update workflow with winning parms
        pull_workflow_set_result(tuned_workflowset,
                                 id = best_model$wflow_id) %>% 
          unnest(c(.metrics)) %>% 
          filter(.config == best_model$.config) %>% 
          slice(1)
        ) %>%
      # train with entire training-data and test with full test-data
      last_fit(split = split_object)
    
    # view metrics for final model
    final_fit$.metrics


# Plot ROC Curve for Final Model----------------------------------------------------------
    
    
    # Create an ROC curve data
    roc_curve_data <- final_fit %>% 
      # Collect predictions
      collect_predictions() %>%
      mutate(Class = factor(Class, levels = c("Yes", "No"))) %>% 
      # Calculate ROC curve metrics
      roc_curve(truth = Class, .pred_Yes) 
    
    # Create an ROC curve 
    roc_curve_data %>% 
      ggplot(aes(x = 1 - specificity, y = sensitivity)) +
      geom_path() +
      geom_abline(lty = 3) +
      coord_equal() +
      theme_bw()
    

ggsave(paste0("ROC Curve ", as.character(Sys.Date()),".png"),
       width = 13,
       height = 8
       )


# Predict Using Fresh Data ------------------------------------------------

# predict(best_workflow, #new_data)

