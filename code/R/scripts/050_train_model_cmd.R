#!/bin/Rscript
#  separate train and test datasets

source("code/R/functions/train_model.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressWarnings(suppressPackageStartupMessages(library(yaml)))

#### MAIN 

option_list <- list(
  make_option(
    "--input",
    action = "store",
    type = "character",
    help = "Set the name of the input data file in tsv format"
  ),
  make_option(
    "--selected_variables",
    action = "store",
    type = "character",
    help = "Set the name selected variables in tsv format"
  ),
  make_option(
    "--model",
    action = "store",
    type = "character",
    default = "",
    help = "Set the basename of the generated model"
  ),
  make_option(
    "--test",
    action = "store",
    type = "character",
    default = "",
    help = "Set the name of the test data file in tsv format"
  )
    
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null()  ||
    opt$model %>% is.null() ||
    opt$selected_variables %>% is.null()||
    opt$test %>% is.null()
    ){
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
} else{
  ## Read Datasets
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  test_dataset <-
    readr::read_delim(opt$test, col_types = cols(), delim = '\t')
    
  ## Read selected Variables
  selected_variables <-
    readr::read_delim(opt$selected_variables, col_types = cols(), delim = '\t')
  
  ## Set default parameters
  params <- yaml::read_yaml("params.yaml")
  if(!  "train_model" %in% names(params)) { 
    message("[] Error: No information for training model")
    quit()
  }
  
  grid<-expand.grid(
    depth = params$train_model$depth,
    learning_rate = params$train_model$learning_rate,
    iterations = params$train_model$iterations,
    l2_leaf_reg = params$train_model$l2_leaf_reg,
    rsm = params$train_model$rsm,
    border_count = params$train_model$border_count
  )
 print(grid) 
 
  boost_model <- 
    train_model(dataset = dataset,
                        selected_variables = selected_variables,
                        gridsearch = grid,
                        vfrac = params$train_model$vfrac)
  ## Save model 
  dir.create(dirname(opt$model), showWarnings = FALSE)
  saveRDS(boost_model, file = paste0(opt$model,"_model.rds"))
  
  ## Save Metric
  #boost_model$results[c(8,10,11,12)] %>% 
  #  as.yaml() %>% write("metrics/train_model.yaml")
  
  ## Save resample metrics
  boost_model$resample %>% select(Accuracy,AUC,Mean_Sensitivity, Mean_Specificity,Mean_Precision,Kappa,Mean_Balanced_Accuracy) %>% 
    summarise(#mean_AUC=mean(AUC),sd_AUC=sd(AUC), 
              reAcc_Mean=mean(Accuracy,na.rm = TRUE),reAcc_sd=sd(Accuracy,na.rm = TRUE),
              reBAcc_Mean=mean(Mean_Balanced_Accuracy,na.rm = TRUE),reBAcc_sd=sd(Mean_Balanced_Accuracy,na.rm = TRUE),
              reSens_Mean=mean(Mean_Sensitivity,na.rm = TRUE),reSens_sd=sd(Mean_Sensitivity,na.rm = TRUE),
              rePrec_Mean=mean(Mean_Precision,na.rm = TRUE),rePrec_sd=sd(Mean_Precision,na.rm = TRUE),
              reSpec_Mean=mean(Mean_Specificity,na.rm = TRUE),reSpec_sd=sd(Mean_Specificity,na.rm = TRUE),
              reKappa_Mean=mean(Kappa,na.rm = TRUE),reKappa_sd=sd(Kappa,na.rm = TRUE)
              ) %>%
    as.yaml() %>% write("metrics/train_model_resample.yaml")
  
  
  ## Save resample metrics per class
  ### Class G
  sens<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::sensitivity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSens_G_Mean=mean(.estimate),reSens_G_sd=sd(.estimate)) %>% 
    filter(obs == "G") %>% select(-obs)
  spec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::specificity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSpec_G_Mean=mean(.estimate),reSpec_G_sd=sd(.estimate)) %>% 
    filter(obs == "G") %>% select(-obs)
  prec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::precision(obs,pred) %>% group_by(obs) %>% 
    summarise(rePrec_G_Mean=mean(.estimate),rePrec_G_sd=sd(.estimate)) %>% 
    filter(obs == "G") %>% select(-obs)
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_G.yaml")
  ### Class GM
  sens<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::sensitivity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSens_GM_Mean=mean(.estimate),reSens_GM_sd=sd(.estimate)) %>% 
    filter(obs == "GM") %>% select(-obs)
  spec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::specificity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSpec_GM_Mean=mean(.estimate),reSpec_GM_sd=sd(.estimate)) %>% 
    filter(obs == "GM") %>% select(-obs)
  prec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::precision(obs,pred) %>% group_by(obs) %>% 
    summarise(rePrec_GM_Mean=mean(.estimate),rePrec_GM_sd=sd(.estimate)) %>% 
    filter(obs == "GM") %>% select(-obs)
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_GM.yaml")
  ### Class R
  sens<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::sensitivity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSens_R_Mean=mean(.estimate),reSens_R_sd=sd(.estimate)) %>% 
    filter(obs == "R") %>% select(-obs)
  spec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::specificity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSpec_R_Mean=mean(.estimate),reSpec_R_sd=sd(.estimate)) %>% 
    filter(obs == "R") %>% select(-obs)
  prec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::precision(obs,pred) %>% group_by(obs) %>% 
    summarise(rePrec_R_Mean=mean(.estimate),rePrec_R_sd=sd(.estimate)) %>% 
    filter(obs == "R") %>% select(-obs)
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_R.yaml")
  
  ### Class W
  sens<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::sensitivity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSens_W_Mean=mean(.estimate),reSens_W_sd=sd(.estimate)) %>% 
    filter(obs == "W") %>% select(-obs)
  spec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::specificity(obs,pred) %>% group_by(obs) %>% 
    summarise(reSpec_W_Mean=mean(.estimate),reSpec_W_sd=sd(.estimate)) %>% 
    filter(obs == "W") %>% select(-obs)
  prec<-boost_model$pred %>% group_by(Resample,obs) %>% 
    yardstick::precision(obs,pred) %>% group_by(obs) %>% 
    summarise(rePrec_W_Mean=mean(.estimate),rePrec_W_sd=sd(.estimate)) %>% 
    filter(obs == "W") %>% select(-obs)
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_W.yaml")
  
  
  ## Save predictions results
  results<-predict_activity(boost_model,test_dataset)
  #write_csv(results$cm %>% as.data.frame() %>% tibble::rownames_to_column("class"), "metrics/train_model_cm.csv")
  write_csv(data.frame(predicted=results$predictions,observed=test_dataset$Activity),"metrics/train_model_predictions.csv")
  ## save metrics per class
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity=="Class: G") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_G.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: GM") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_GM.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: W") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_W.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: R") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_R.yaml")
  
  }
