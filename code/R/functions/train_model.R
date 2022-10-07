suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(catboost))
suppressPackageStartupMessages(library(yardstick))
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
train_model <- function(dataset, selected_variables, gridsearch = NULL, vfrac = 0.1) {
  set.seed(19091974) 

 
  
  val_dataset <- dataset %>% sample_frac(vfrac)
  train_dataset <- setdiff(dataset, val_dataset)
  
  ## Caret
  boost_model <- caret::train(
    x = train_dataset%>% 
      select(selected_variables$variable) %>%  na.omit() %>% as.data.frame(),
    y = train_dataset %>%  select(Activity) %>% unlist() %>% 
      unname()  %>% as.factor(),
    method = catboost::catboost.caret,
    tuneGrid = gridsearch,
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
  
  pred_vs_obs<-data.frame(observations=as.factor(dataset$Activity),predictions=predictions)
  overall_macro<-rbind(
        yardstick::sensitivity(pred_vs_obs,
                    estimate=predictions,
                    truth=observations,
                    estimator="macro"),
        
        
        yardstick::specificity(pred_vs_obs,
                    estimate=predictions,
                    truth=observations,
                    estimator="macro"),
        
        yardstick::precision(pred_vs_obs,
                  estimate=predictions,
                  truth=observations,
                  estimator="macro")
  )%>% tidyr::pivot_wider(names_from = ".metric",values_from = ".estimate") %>%
    select(-.estimator) %>% as.list()
  
  overall <- cm$overall[1:2] %>% as.list() 
  names(overall)<-c("Acc_macro","Kappa_macro")
  names(overall_macro)<-c("Sens_macro","Spec_macro","Prec_macro")
  list(overall=overall, 
       macro=overall_macro,
       cm=cm$byClass,
       predictions=predictions, 
       tab=cm$table)
}

