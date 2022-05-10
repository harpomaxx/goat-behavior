suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(catboost))
source("code/R/functions/caret_params.R")


#' Train a model using caret. 
#'
#' @param dataset a dataset with goat-behavior information
#' @param selected_variables  a list of selected variables 
#' @param gridsearch 
#'
#' @return a caret model
#' @export
#'
#' @examples
train_model <- function(dataset, selected_variables, gridsearch=NULL) {
  set.seed(19091974) 
 
  
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


#' Title
#'
#' @param model 
#' @param dataset 
#'
#' @return
#' @export
#'
#' @examples
predict_activity<-function(model,dataset){

  predictions<-predict(model,dataset)  
  cm <- caret::confusionMatrix(reference=dataset$Activity %>% as.factor(),
                               predictions %>% as.factor() )
  list(cm=cm$byClass,
       predictions=predictions, 
       tab=cm$table)
}

