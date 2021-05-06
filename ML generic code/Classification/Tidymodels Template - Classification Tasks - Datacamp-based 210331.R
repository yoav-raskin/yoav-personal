library(mlbench)
library(tidymodels)
library(tidyverse)


##### Sampling Data (create rsample objects) ####

# Create data split object
loans_split <- initial_split(loans_df, 
                             strata = loan_default)

# Build training data
loans_training <- loans_split %>% 
  training()

# Build test data
loans_test <- loans_split %>% 
  testing()


# Create cross validation folds
set.seed(290)
loans_folds <- vfold_cv(loans_training, v = 5,
                        strata = loan_default)


##### Feature Engineering (create recipe object) ####


# Check for correlated predictors (pre-engineering test)
loans_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>%
  # Calculate correlation matrix
  cor()


# Build feature engineering pipeline
loans_recipe <- recipe(loan_default ~.,
                       data = loans_training) %>% 
  # Correlation filter
  step_corr(all_numeric(), threshold = 0.85) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes)

loans_recipe


##### Specify a basic Classification Model (create parsnip object) ####


dt_model <- decision_tree() %>% 
  # Specify the engine
  set_engine('rpart') %>% 
  # Specify the mode
  set_mode('classification')

##### Integrate to a Basic Workflow (create workflow object) ####

# Create a workflow
loans_dt_wkfl <- workflow() %>% 
  # Include the model object
  add_model(dt_model) %>% 
  # Include the recipe object
  add_recipe(loans_recipe)

##### Specify a Tuning Classification Model (create workflow object) ####


# Set tuning hyperparameters
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) %>% 
  # Specify engine
  set_engine('rpart') %>% 
  # Specify mode
  set_mode('classification')

##### Update Basic Workflow with a Tuning Model (update existing workflow object) ####


# Create a tuning workflow
loans_tune_wkfl <- loans_dt_wkfl %>% 
  # Replace model
  update_model(dt_tune_model)

loans_tune_wkfl

##### Build a Grid of hyperparameters Values (create a tibble with dials) ####


# Hyperparameter tuning with grid search
set.seed(214)
dt_grid <- grid_random(parameters(dt_tune_model),
                       size = 5)

##### Create a custom metric function (with yardstick) ####

loans_metrics <- metric_set(roc_auc,
                            sens,
                            spec)  

##### Run Tuning Workflow & Collect Results (with tune) ####

dt_tuning <- loans_tune_wkfl %>% 
  tune_grid(resamples = loans_folds,
            grid = dt_grid,
            metrics = loans_metrics) %>% 
  
  # # OPTIONAL: Collect detailed tuning results
  # dt_tuning_results <- dt_tuning %>% 
  # collect_metrics(summarize = FALSE)
  # 
  # # OPTIONAL: Explore detailed ROC AUC results for each fold
  # dt_tuning_results %>% 
  #   filter(.metric == "roc_auc") %>% 
  #   group_by(id) %>% 
  #   summarize(min_roc_auc = min(.estimate),
  #             median_roc_auc = mean(.estimate),
  #             max_roc_auc = max(.estimate))

##### Finalize (Update) the Tune Workflow with Best Model - Based on Training Data Folds (with tune) ####
  
  # Select based on best performance
  best_dt_model <- dt_tuning %>% 
    # Choose the best model based on roc_auc
    select_best(metric = 'roc_auc')
  
  # Finalize your workflow
  final_loans_wkfl <- loans_tune_wkfl %>% 
    finalize_workflow(best_dt_model)
  
  final_loans_wkfl
  
#### Train Best Model on Full Training Data & Evaluate on Test Data (with tune)  ####


  # Train finalized decision tree workflow
  loans_final_fit <- final_loans_wkfl %>% 
    last_fit(split = loans_split)
  
#### Analyze Metrics (with tune)  ####

  # View performance metrics
  loans_final_fit %>% 
    collect_metrics()
  
  # Create an ROC curve
  loans_final_fit %>% 
    # Collect predictions
    collect_predictions() %>%
    # Calculate ROC curve metrics
    roc_curve(truth = loan_default, .pred_yes) %>%
    # Plot the ROC curve
    autoplot()