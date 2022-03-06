#!/bin/Rscript
#  separate train and test datasets

source("code/R/functions/train_model.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(yaml))

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
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null()  ||
    opt$model %>% is.null() ||
    opt$selected_variables %>% is.null()){
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
} else{
  ## Read Dataset
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  ## Read selected Variables
  selected_variables <-
    readr::read_delim(opt$selected_variables, col_types = cols(), delim = '\t')
  
  ## Set default parameters
  #params <- yaml::read_yaml("params.yaml")
  #if(!  "separate_train_test" %in% names(params)) { 
  #  message("[] Error: No information for excluding animals")
  #  quit()
  #}
  
  boost_model <- 
    train_model(dataset = dataset,
                        selected_variables=selected_variables)
  ## Save model 
  dir.create(dirname(opt$model), showWarnings = FALSE)
  saveRDS(boost_model, file = paste0(opt$model,"_model.rds"))
  
  ## Save Metric
  boost_model$results[c(8,10,11,12)] %>% as.yaml() %>% write("metrics/train_model.yaml")
  ## Save predictions results
  results<-predict_activity(boost_model,dataset)
  #write_csv(results$cm %>% as.data.frame() %>% tibble::rownames_to_column("class"), "metrics/train_model_cm.csv")
  write_csv(data.frame(predicted=results$predictions,observed=dataset$Activity),"metrics/train_model_predictions.csv")
  ## save metrics per class
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity=="Class: G") %>% 
    select("Sensitivity","Specificity","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_G.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: GM") %>% 
    select("Sensitivity","Specificity","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_GM.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: W") %>% 
    select("Sensitivity","Specificity","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_W.yaml")
  
  results$cm %>% as.data.frame() %>% tibble::rownames_to_column("Activity") %>% 
    filter(Activity =="Class: R") %>% 
    select("Sensitivity","Specificity","Balanced Accuracy") %>%
    as.yaml() %>% 
    write("metrics/train_model_metrics_R.yaml")
  
  }
