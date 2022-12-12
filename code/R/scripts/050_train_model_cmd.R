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
  
  
  # Save resample metrics per class
  # The idea is to apply and 1 vs all approach. 
  # We convert each class to 1 and the rest to 0 and then we apply the performance metric.
  ### Class G
  
  G_preds<-boost_model$pred %>% 
    mutate(pred=ifelse(pred=="G",1,0), obs=ifelse(obs=="G",1,0)) %>%
    mutate(pred=as.factor(pred),obs=as.factor(obs))
  sens<-G_preds %>% group_by(Resample) %>% 
    yardstick::sensitivity(obs,pred) %>% 
    summarise(reSens_G_Mean=mean(.estimate),reSens_G_sd=sd(.estimate)) 
  prec<-G_preds %>% group_by(Resample) %>% 
    yardstick::precision(obs,pred) %>% 
    summarise(rePrec_G_Mean=mean(.estimate),rePrec_G_sd=sd(.estimate)) 
  spec<-G_preds %>% group_by(Resample) %>% 
    yardstick::specificity(obs,pred) %>% 
    summarise(reSpec_G_Mean=mean(.estimate),reSpec_G_sd=sd(.estimate))
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_G.yaml")
  
  ### Class GM
  
  GM_preds<-boost_model$pred %>% 
    mutate(pred=ifelse(pred=="GM",1,0), obs=ifelse(obs=="GM",1,0)) %>%
    mutate(pred=as.factor(pred),obs=as.factor(obs))
  sens<-GM_preds %>% group_by(Resample) %>% 
    yardstick::sensitivity(obs,pred) %>% 
    summarise(reSens_GM_Mean=mean(.estimate),reSens_GM_sd=sd(.estimate)) 
  prec<-GM_preds %>% group_by(Resample) %>% 
    yardstick::precision(obs,pred) %>% 
    summarise(rePrec_GM_Mean=mean(.estimate),rePrec_GM_sd=sd(.estimate)) 
  spec<-GM_preds %>% group_by(Resample) %>% 
    yardstick::specificity(obs,pred) %>% 
    summarise(reSpec_GM_Mean=mean(.estimate),reSpec_GM_sd=sd(.estimate)) 
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_GM.yaml")
   
  ### Class R
  
  R_preds<-boost_model$pred %>% 
    mutate(pred=ifelse(pred=="R",1,0), obs=ifelse(obs=="R",1,0)) %>%
    mutate(pred=as.factor(pred),obs=as.factor(obs))
  sens<-R_preds %>% group_by(Resample) %>% 
    yardstick::sensitivity(obs,pred) %>% 
    summarise(reSens_R_Mean=mean(.estimate),reSens_R_sd=sd(.estimate)) 
  prec<-R_preds %>% group_by(Resample) %>% 
    yardstick::precision(obs,pred) %>% 
    summarise(rePrec_R_Mean=mean(.estimate),rePrec_R_sd=sd(.estimate)) 
  spec<-R_preds %>% group_by(Resample) %>% 
    yardstick::specificity(obs,pred) %>% 
    summarise(reSpec_R_Mean=mean(.estimate),reSpec_R_sd=sd(.estimate)) 
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_R.yaml")
  
  ### Class W
  
  W_preds<-boost_model$pred %>% 
    mutate(pred=ifelse(pred=="W",1,0), obs=ifelse(obs=="W",1,0)) %>%
    mutate(pred=as.factor(pred),obs=as.factor(obs))
  sens<-W_preds %>% group_by(Resample) %>% 
    yardstick::sensitivity(obs,pred) %>% 
    summarise(reSens_W_Mean=mean(.estimate),reSens_W_sd=sd(.estimate)) 
  prec<-W_preds %>% group_by(Resample) %>% 
    yardstick::precision(obs,pred) %>% 
    summarise(rePrec_W_Mean=mean(.estimate),rePrec_W_sd=sd(.estimate)) 
  spec<-W_preds %>% group_by(Resample) %>% 
    yardstick::specificity(obs,pred) %>% 
    summarise(reSpec_W_Mean=mean(.estimate),reSpec_W_sd=sd(.estimate)) 
  cbind(sens,spec,prec) %>%  as.yaml() %>% 
    write("metrics/train_model_resample_metrics_W.yaml")
 
  ## Save predictions results
  results<-predict_activity(boost_model,test_dataset)
  #write_csv(results$cm %>% as.data.frame() %>% tibble::rownames_to_column("class"), "metrics/train_model_cm.csv")
  write_csv(data.frame(predicted=results$predictions,observed=test_dataset$Activity),"metrics/train_model_predictions.csv")
  
  
  ## Save results MACRO results for test_datataset
  c(results$overall,results$macro) %>% as.yaml %>%
    write("metrics/train_model_metrics_macro_overall.yaml")
  
  ## save metrics per class
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity=="Class: G") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    rename(testSens_G = Sensitivity, 
           testSpec_G = Specificity, 
           testPrec_G = Precision, 
           testBacc_G = `Balanced Accuracy`) %>%
    
    
    as.yaml() %>% 
    write("metrics/train_model_metrics_G.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: GM") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    rename(testSens_GM = Sensitivity, 
           testSpec_GM = Specificity, 
           testPrec_GM = Precision, 
           testBacc_GM = `Balanced Accuracy`) %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_GM.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: W") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    rename(testSens_W = Sensitivity, 
           testSpec_W = Specificity, 
           testPrec_W = Precision, 
           testBacc_W = `Balanced Accuracy`) %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_W.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: R") %>% 
    select("Sensitivity","Specificity","Precision","Balanced Accuracy") %>%
    rename(testSens_R = Sensitivity, 
           testSpec_R = Specificity, 
           testPrec_R = Precision, 
           testBacc_R = `Balanced Accuracy`) %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_R.yaml")
  
  }
