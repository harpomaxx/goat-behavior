#!/bin/Rscript
#  test model 

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
    select("mean_Sens","mean_Spec","mean_BAcc", "sd_Sens","sd_Spec","sd_BAcc") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_R.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: W") %>% 
    select("mean_Sens","mean_Spec","mean_BAcc", "sd_Sens","sd_Spec","sd_BAcc") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_W.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: GM") %>% 
    select("mean_Sens","mean_Spec","mean_BAcc", "sd_Sens","sd_Spec","sd_BAcc") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_GM.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: G") %>% 
    select("mean_Sens","mean_Spec","mean_BAcc", "sd_Sens","sd_Spec","sd_BAcc") %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_G.yaml")
  
  metrics$byclass %>% 
    select("mean_Sens","mean_Spec","mean_BAcc") %>%
    summarise(sd_BAcc=sd(mean_BAcc,na.rm=TRUE),
              sd_Sens=sd(mean_Sens,na.rm=TRUE),
              sd_Spec=sd(mean_Spec,na.rm=TRUE),
              mean_Sens=mean(mean_Sens,na.rm=TRUE),
              mean_Spec=mean(mean_Spec,na.rm=TRUE),
              mean_BAcc=mean(mean_BAcc,na.rm=TRUE)
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_overall.yaml")
  
  metrics$micro %>% as.yaml %>%
    write("metrics/test_model_loocv_metrics_micro.yaml")
  
  
  
  
  ## Save model 
  dir.create(dirname(opt$results), showWarnings = FALSE)
  saveRDS(results, file = paste0(opt$results,"_model_test_loocv_results.rds"))
  
  ## Save predictions results
  
}