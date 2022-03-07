
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