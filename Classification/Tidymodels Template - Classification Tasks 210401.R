
# 1 Load Data (Source: UCI ML Repository)  -------------------------------------------------------------------------

  data(BreastCancer)
  data <- BreastCancer
  rm(BreastCancer)
  

# 2 Specificy Components & Integrate as Workflow ----------------------------

  
  

# * 2.1 Sampling Data (create rsample objects) -------------------------------------------------------------------------

  # Create data split object
  split_object <- initial_split(data,
                               prob = 3/4,
                               strata = Class)
  
  # Build training data
  training <- split_object %>% 
    training()
  
  # Build test data
  test <- split_object %>% 
    testing()
  
  
  # Create cross validation folds
  set.seed(290)
  folds <- vfold_cv(training, v = 3,
                          strata = Class)


# * 2.2 Feature Engineering (create recipe object)-------------------------------------------------------------------------


  # Build feature engineering pipeline
  recipe <- recipe(Class ~ .,
                         data = training) %>%
    #Change outcome to Yes\No String
    step_mutate(Class = str_replace(Class, "malignant", "Yes"),
                Class = str_replace(Class, "benign", "No")) %>% 
    #Remove id
    step_rm(Id) %>% 
  
    #Transform to numeric class
    step_integer(all_nominal(), -all_outcomes()) %>% 
  
    # Correlation filter
    # step_corr(all_numeric(), threshold = 0.85) %>% 
    # Normalize numeric predictors
    step_normalize(all_numeric()) %>% 
    # Create dummy variables
    # step_dummy(all_nominal(), -all_outcomes) 
    # Supervised Feature Selection
    step_select_infgain(all_predictors(), outcome = "Class", threshold = 0.5)
  
  recipe
  
  
  baked_data <- prep(recipe, training = training) %>%
            bake(new_data = NULL)
  
  # Check for correlated predictors (pre-engineering test)
  baked_data %>%
    # Select numeric columns
    select_if(is.numeric) %>%
    # Calculate correlation matrix
    cor()
  
  
  

# * 2.3 Specify a Tuning Classification Model (create parsnip object) -------------------------------------------------------------------------

  # Set tuning hyperparameters
  dt_tune_model <- decision_tree(cost_complexity = tune(),
                                 tree_depth = tune(),
                                 min_n = tune()) %>% 
    # Specify engine
    set_engine('rpart') %>% 
    # Specify mode
    set_mode('classification')
  

# * 2.4 Integrate to a Tune Workflow (create workflow object) -------------------------------------------------------------------------

  # Create a workflow
  tune_wf <- workflow() %>% 
    # Include the model object
    add_model(dt_tune_model) %>% 
    # Include the recipe object
    add_recipe(recipe)
  
  tune_wf
  

# 3 Specify Tuning Helpers & Run Tuning ----------------------------------------------------------------


 

# * 3.1 Build a Grid of hyperparameters Values (create a tibble with dials) -------------------------------------------------------------------------

  # Hyperparameter tuning with grid search
  set.seed(214)
  dt_grid <- grid_random(parameters(dt_tune_model),
                         size = 5)
  

# * 3.2 Create a Custom Metric Function (with yardstick) -------------------------------------------------------------------------
  
  metrics <- metric_set(yardstick::roc_auc,
                        yardstick::sens,
                        yardstick::spec)
  
  
# * 3.3 Run Tuning Workflow & Select Best Model on roc_auc Metric (with tune) -------------------------------------------------------------------------

  best_dt_model <- tune_wf %>% 
    tune_grid(resamples = folds,
              grid = dt_grid,
              metrics = metrics) %>% 
    select_best(metric = 'roc_auc')
  
  best_dt_model
  
  
  # OPTIONAL: Collect detailed tuning results

  dt_tuning_results <- tune_wf %>%
    tune_grid(resamples = folds,
              grid = dt_grid,
              metrics = metrics) %>% 
    collect_metrics(summarize = FALSE)
  
  view(dt_tuning_results)

  # OPTIONAL: Explore detailed ROC AUC results for each fold
  dt_tuning_results %>%
    filter(.metric == "roc_auc") %>%
    group_by(id) %>%
    summarize(min_roc_auc = min(.estimate),
              mean_roc_auc = mean(.estimate),
              max_roc_auc = max(.estimate))


# 4 Finalize (Update) the Tune Workflow + Train (Training) & Evaluate (Test) (with tune) -------------------------------------------------------------------------

  
  # Finalize your workflow
  final_fit <- tune_wf %>% 
    finalize_workflow(best_dt_model) %>%
    last_fit(split = split_object)
  
# 5 Analyze Metrics (with tune) -------------------------------------------------------------------------


  # View performance metrics
  final_fit %>% 
    collect_metrics()
  
  # Create an ROC curve
  final_predictions <- final_fit %>% 
    # Collect predictions
    collect_predictions() %>%
    mutate(Class = factor(Class, levels = c("Yes", "No")))

    # Calculate ROC curve metrics
    roc_curve(final_predictions, truth = Class, .pred_Yes) %>%
    # Plot the ROC curve
    autoplot()
  