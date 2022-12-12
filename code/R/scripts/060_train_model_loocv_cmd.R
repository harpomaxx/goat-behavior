#!/bin/Rscript
#  separate train and test datasets

source("code/R/functions/train_model_loocv.R")
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
    "--results",
    action = "store",
    type = "character",
    default = "",
    help = "Set the basename of the model results"
  ),
  make_option(
    "--model",
    action = "store",
    type = "character",
    default = "",
    help = "Set the filename of the model in .rds format"
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null()  ||
    opt$selected_variables %>% is.null() ||
    opt$model%>% is.null() ||
    opt$results %>% is.null()) {
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
} else{
  ## Read Dataset
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  ## Read selected Variables
  selected_variables <-
    readr::read_delim(opt$selected_variables, col_types = cols(), delim = '\t')
  
  ## Read model
  boost_model <- readRDS(opt$model)
  
  ## Set default parameters
  params <- yaml::read_yaml("params.yaml")
  if(!  "train_model" %in% names(params)) { 
    message("[] Error: No information for training model")
    quit()
  }
  
  #grid<-expand.grid(
  #  depth = boost_model$bestTune$depth,
  #  learning_rate = boost_model$bestTune$learning_rate,
  #  iterations = boost_model$bestTune$iterations, 
  #  l2_leaf_reg = boost_model$bestTune$l2_leaf_reg, 
  #  rsm = boost_model$bestTune$rsm, 
  #  border_count = boost_model$bestTune$border_count
  #)

  grid<-expand.grid(
    depth = params$train_model$depth,
    learning_rate = params$train_model$learning_rate,
    iterations = params$train_model$iterations,
    l2_leaf_reg = params$train_model$l2_leaf_reg,
    rsm = params$train_model$rsm,
    border_count = params$train_model$border_count
  )
 print(grid) 
 results <- 
    train_model_loocv(dataset = dataset,
                        selected_variables=selected_variables,
                        gridsearch = grid,
                        vfrac = params$train_model$vfrac)
  
  metrics<-loocv_peformance_metrics(results)
  metrics$byclass %>% 
    filter(class  =="Class: R") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    rename ( looSens_R_mean = looSens_mean,
             looSpec_R_mean = looSpec_mean,
             looPrec_R_mean = looPrec_mean,
             looBAcc_R_mean = looBAcc_mean,
             looSens_R_sd   = looSens_sd,
             looSpec_R_sd   = looSpec_sd,
             looPrec_R_sd   = looPrec_sd,
             looBAcc_R_sd   = looBAcc_sd
             ) %>%
    as.yaml() %>% 
    write("metrics/train_model_loocv_metrics_R.yaml")
 
  metrics$byclass %>% 
    filter(class  =="Class: W") %>% 
    
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    rename ( looSens_W_mean = looSens_mean,
             looSpec_W_mean = looSpec_mean,
             looPrec_W_mean = looPrec_mean,
             looBAcc_W_mean = looBAcc_mean,
             looSens_W_sd   = looSens_sd,
             looSpec_W_sd   = looSpec_sd,
             looPrec_W_sd   = looPrec_sd,
             looBAcc_W_sd   = looBAcc_sd
             ) %>%
    as.yaml() %>% 
    write("metrics/train_model_loocv_metrics_W.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: GM") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
     rename (looSens_GM_mean = looSens_mean,
             looSpec_GM_mean = looSpec_mean,
             looPrec_GM_mean = looPrec_mean,
             looBAcc_GM_mean = looBAcc_mean,
             looSens_GM_sd   = looSens_sd,
             looSpec_GM_sd   = looSpec_sd,
             looPrec_GM_sd   = looPrec_sd,
             looBAcc_GM_sd   = looBAcc_sd
             ) %>%
    as.yaml() %>% 
    write("metrics/train_model_loocv_metrics_GM.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: G") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
     rename (looSens_G_mean = looSens_mean,
             looSpec_G_mean = looSpec_mean,
             looPrec_G_mean = looPrec_mean,
             looBAcc_G_mean = looBAcc_mean,
             looSens_G_sd   = looSens_sd,
             looSpec_G_sd   = looSpec_sd,
             looPrec_G_sd   = looPrec_sd,
             looBAcc_G_sd   = looBAcc_sd
             ) %>%
    as.yaml() %>% 
    write("metrics/train_model_loocv_metrics_G.yaml")
  
  metrics$byclass %>% 
    select("looSens_mean","looSpec_mean","looBAcc_mean","looPrec_mean") %>%
    summarise(looBAcc_sd=sd(looBAcc_mean,na.rm=TRUE),
              looSens_sd=sd(looSens_mean,na.rm=TRUE),
              looSpec_sd=sd(looSpec_mean,na.rm=TRUE),
              looPrec_sd=sd(looPrec_mean,na.rm=TRUE),
              looSens_mean=mean(looSens_mean,na.rm=TRUE),
              looSpec_mean=mean(looSpec_mean,na.rm=TRUE),
              looBAcc_mean=mean(looBAcc_mean,na.rm=TRUE),
              looPrec_mean=mean(looPrec_mean,na.rm=TRUE)
              ) %>%
    as.yaml() %>% 
    write("metrics/train_model_loocv_metrics_avg_all.yaml")
  
  c(metrics$overall,metrics$macro) %>% as.yaml %>%
  write("metrics/train_model_loocv_metrics_macro_overall.yaml")
  
  #metrics$overall %>% as.yaml %>%
  #write("metrics/train_model_loocv_metrics_macro_overall.yaml")
  
  ## Save results 
  dir.create(dirname(opt$results), showWarnings = FALSE)
  saveRDS(results, file = paste0(opt$results,"_model_train_loocv_results.rds"))
  
  ## Save predictions results
  
  }
