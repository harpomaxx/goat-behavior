#!/bin/Rscript
#  test model per average.

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
    opt$model %>% is.null() ||
    opt$results %>% is.null()
    ) {
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
  results <- 
    test_model_loocv(dataset = dataset,
                      selected_variables=selected_variables,
                      model = boost_model)
  
  
  metrics<-loocv_peformance_metrics(results)
  metrics$byclass %>% 
    filter(class  =="Class: R") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_R.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: W") %>% 
    
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_W.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: GM") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_GM.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: G") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_G.yaml")
  
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
    write("metrics/test_model_loocv_metrics_avg_all.yaml")
  
  c(metrics$overall,metrics$macro) as.yaml %>%
    write("metrics/test_model_loocv_metrics_macro_overall.yaml")
  
  #metrics$overall %>% as.yaml %>%
  #write("metrics/test_model_loocv_metrics_overall.yaml")
  
  ## Save model 
  dir.create(dirname(opt$results), showWarnings = FALSE)
  saveRDS(results, file = paste0(opt$results,"_model_test_loocv_results.rds"))
  
  ## Save predictions results
  
}
