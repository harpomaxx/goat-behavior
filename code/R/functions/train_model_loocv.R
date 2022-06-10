suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(catboost))
suppressWarnings(suppressPackageStartupMessages(library(yardstick)))

source("code/R/functions/caret_params.R")


metrics <- function(cm,preds,testset) {
  macro_metrics <- (apply(cm$byClass, 2, sum) / 4) [c(5, 6, 7)]
  Precision_micro <-  (sum(diag(s)) / sum(apply(s, 1, sum)))
  Recall_micro <- (sum(diag(s)) / sum(apply(s, 2, sum)))
  micro_metrics <- c((sum(diag(s)) / sum(apply(s, 1, sum))),
                     (sum(diag(s)) / sum(apply(s, 2, sum))),
                     2 * ((Precision_micro * Recall_micro) / (Precision_micro + Recall_micro)
                     ))
  mm <- cbind(c(macro_metrics), c(micro_metrics))
  data.frame(macro = mm[, 1], micro = mm[, 2])
}
create_loocv_samples <- function(dataset) {
  loocv_sample_train <-
    vector(mode = "list",
           length = length(dataset$Anim %>% unique()))
  loocv_sample_test <-
    vector(mode = "list",
           length = length(dataset$Anim %>% unique()))
  
  n_anim <- dataset$Anim %>% unique()
  for (i in seq_along(n_anim)) {
    loocv_sample_train[[i]] <- which(dataset$Anim != n_anim[i])
    loocv_sample_test[[i]] <- which(dataset$Anim == n_anim[i])
  }
  loocv_samples<-list(n_anim=n_anim,
                      loocv_train=loocv_sample_train,
                      loocv_test=loocv_sample_test)
}

train_model_loocv <- function(dataset, selected_variables, gridsearch=NULL,vfrac=0.1) {
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  set.seed(19091974) 
  samples<-create_loocv_samples(dataset) 
  loocv_results<- vector(mode = "list", length = length( samples$n_anim))
  
  for (i in seq(1, length(samples$n_anim))) {
    train_dataset_loocv <- dataset[samples$loocv_train[[i]], ]
    train_dataset_loocv$Activity <-
      as.factor(train_dataset_loocv$Activity)
    test_dataset_loocv <- dataset[-samples$loocv_train[[i]], ]
    test_dataset_loocv$Activity <-
      factor(test_dataset_loocv$Activity,
             levels = levels(train_dataset_loocv$Activity))
  
    val_dataset <- train_dataset_loocv %>% sample_frac(0.1)
    train_dataset <- setdiff(train_dataset_loocv, val_dataset)
  
    boostFit <- caret::train(
      x = train_dataset %>%
        select(selected_variables$variable) %>%  na.omit() %>%
        as.data.frame(),
      y = train_dataset %>%  select(Activity) %>%
        unlist() %>% unname()  %>% as.factor(),
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
          select(selected_variables$variable) %>%  na.omit() %>% 
          as.data.frame(),
        label = as.numeric(as.factor(val_dataset$Activity)) - 1
      )
    )
    ## Independent Evaluation
    boostFit_eval <- predict(boostFit,
                             test_dataset_loocv  %>%
                               select(selected_variables$variable))
    cm <-
      confusionMatrix(reference = as.factor(test_dataset_loocv$Activity), boostFit_eval)
    
    loocv_results[[i]] <- list(
      overall = cm$overall,
      byclass = cm$byClass,
      tab = cm$table,
      predictions = boostFit_eval,
      observations = as.factor(test_dataset_loocv$Activity),
      model = boostFit
    )
  }
  loocv_results
}

test_model_loocv <- function(dataset, selected_variables, model) {
  suppressPackageStartupMessages(library(doMC))
  registerDoMC(cores = 8)
  set.seed(19091974) 
  samples<-create_loocv_samples(dataset) 
  loocv_results<- vector(mode = "list", length = length( samples$n_anim))
  
  for (i in seq(1, length(samples$n_anim))) {
    train_dataset_loocv <- dataset[samples$loocv_train[[i]], ]
    train_dataset_loocv$Activity <-
      as.factor(train_dataset_loocv$Activity)
    test_dataset_loocv <- dataset[-samples$loocv_train[[i]], ]
    test_dataset_loocv$Activity <-
      factor(test_dataset_loocv$Activity,
             levels = levels(train_dataset_loocv$Activity))
    
    boostFit_eval <- predict(model,
                             test_dataset_loocv  %>%
                               select(selected_variables$variable))
    cm <-
      confusionMatrix(reference = as.factor(test_dataset_loocv$Activity), boostFit_eval)
    
    loocv_results[[i]] <- list(
      overall = cm$overall,
      byclass = cm$byClass,
      tab = cm$table,
      predictions = boostFit_eval,
      observations = as.factor(test_dataset_loocv$Activity),
      model = model 
    )
  }
  loocv_results
}



#' Title
#'
#' @param loocv_results 
#'
#' @return
#' @export
#'
#' @examples
loocv_peformance_metrics<-function(loocv_results){
  overall_acc<-lapply(loocv_results,function(x) x$overall %>% t() %>% 
                    as.data.frame() %>% 
                    select(Accuracy)) %>% unlist() %>% t() 
  
  
  overall_kap<-lapply(loocv_results,function(x) x$overall %>% t() %>% 
                    as.data.frame() %>% 
                    select(Kappa)) %>% unlist() %>% t() 
  
  byclass <- lapply(loocv_results, function(x)
    x$byclass)
  
  rownames <- do.call(rbind, byclass)   %>% rownames()
  
  byclass<-do.call(rbind, byclass) %>% as.data.frame() %>% 
    tibble::add_column(class =rownames) %>% 
    group_by(class) %>% 
    summarise(mean_Sens = mean(Sensitivity, na.rm =TRUE), 
              sd_Sens=sd(Sensitivity, na.rm=TRUE),
              mean_Spec = mean(Specificity, na.rm =TRUE), 
              sd_Spec=sd(Specificity, na.rm=TRUE),
              mean_BAcc = mean(`Balanced Accuracy`, na.rm =TRUE), 
              sd_BAcc=sd(`Balanced Accuracy`, na.rm=TRUE),
              mean_Prec = mean(`Precision`, na.rm =TRUE), 
              sd_Prec=sd(`Precision`, na.rm=TRUE),
    )
  
  micro_metrics <-  cbind(
    purrr::map(
      loocv_results,
      .f = function(x)
        data.frame(preds = x$predictions, obs = x$observations)
    ) %>%
      purrr::map_df(
        .f = function(x)
          bal_accuracy(
            x,
            estimate = preds,
            truth = obs,
            estimator = "micro"
          )
      ) %>% summarise(mean_BAcc_micro = mean(.estimate, na.rm = TRUE)),
    
    purrr::map(
      loocv_results,
      .f = function(x)
        data.frame(preds = x$predictions, obs = x$observations)
    ) %>%
      purrr::map_df(
        .f = function(x)
          specificity(
            x,
            estimate = preds,
            truth = obs,
            estimator = "micro"
          )
      ) %>% summarise(mean_Spec_micro = mean(.estimate, na.rm = TRUE)),
    
    purrr::map(
      loocv_results,
      .f = function(x)
        data.frame(preds = x$predictions, obs = x$observations)
    ) %>%
      purrr::map_df(
        .f = function(x)
          sensitivity(
            x,
            estimate = preds,
            truth = obs,
            estimator = "micro"
          )
      ) %>% summarise(mean_Sens_micro = mean(.estimate, na.rm = TRUE)),
    
     
    purrr::map(
      loocv_results,
      .f = function(x)
        data.frame(preds = x$predictions, obs = x$observations)
    ) %>%
      purrr::map_df(
        .f = function(x)
          precision(
            x,
            estimate = preds,
            truth = obs,
            estimator = "micro"
          )
      ) %>% summarise(mean_Prec_micro = mean(.estimate, na.rm = TRUE))
  )
 
  
  list(
    byclass = byclass,
    overall = list(
                    Acc_mean = overall_acc %>% mean(), 
                    Acc_sd = overall_acc %>% sd(),
                    Kappa_mean = overall_kap %>% mean(), 
                    Kappa__sd = overall_kap %>% sd()
                   ),
    micro = micro_metrics %>% as.list()
  ) 
}

