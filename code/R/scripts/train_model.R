suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(catboost))

train_model <- function(dataset, selected_variables, gridsearch=NULL) {
  
  ## Control and Resampling setup
  ctrl_fast <- trainControl(
    method = "cv",
    number = 10,
    returnResamp = 'final',
    savePredictions = 'final',
    verboseIter = FALSE,
    classProbs = TRUE,
    allowParallel = TRUE,
    summaryFunction = multiClassSummary
  )
  
  ## Grid Search parameters
  grid <- expand.grid(
    depth = c(4),
    learning_rate = c(0.05),
    #iterations = c(10,50,100,500,1000),
    iterations = c(1000),
    l2_leaf_reg = c(3.0),
    #regularization parameter
    rsm = c(1),
    # percentage of features to use at each split selection
    border_count = c(64)
  )
  
  val_dataset <- dataset %>% sample_frac(.2)
  train_dataset <- setdiff(dataset, val_dataset)
  
  ## Caret
  boost_model <- caret::train(
    x = train_dataset%>% 
      select(selected_variables$variable) %>%  na.omit() %>% as.data.frame(),
    y = train_dataset %>%  select(Activity) %>% unlist() %>% 
      unname()  %>% as.factor(),
    method = catboost::catboost.caret,
    tuneGrid = grid,
    metric = 'logLoss',
    verbose = 0,
    trControl = ctrl_fast,
    auto_class_weights = "Balanced",
    use_best_model = TRUE,
    od_type = 'Iter',
    test_pool = catboost.load_pool(
      val_dataset %>%
        select(selected_variables$variable) %>%  na.omit() %>% as.data.frame(),
      label = as.numeric(as.factor(
        val_dataset$Activity
      )) -
        1
    )
  )
  boost_model
}
