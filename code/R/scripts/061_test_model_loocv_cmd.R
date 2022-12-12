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
    rename ( TestlooSens_R_mean = looSens_mean,
             TestlooSpec_R_mean = looSpec_mean,
             TestlooPrec_R_mean = looPrec_mean,
             TestlooBAcc_R_mean = looBAcc_mean,
             TestlooSens_R_sd   = looSens_sd,
             TestlooSpec_R_sd   = looSpec_sd,
             TestlooPrec_R_sd   = looPrec_sd,
             TestlooBAcc_R_sd   = looBAcc_sd
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_R.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: W") %>% 
    
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    rename ( TestlooSens_W_mean = looSens_mean,
             TestlooSpec_W_mean = looSpec_mean,
             TestlooPrec_W_mean = looPrec_mean,
             TestlooBAcc_W_mean = looBAcc_mean,
             TestlooSens_W_sd   = looSens_sd,
             TestlooSpec_W_sd   = looSpec_sd,
             TestlooPrec_W_sd   = looPrec_sd,
             TestlooBAcc_W_sd   = looBAcc_sd
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_W.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: GM") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    rename (TestlooSens_GM_mean = looSens_mean,
            TestlooSpec_GM_mean = looSpec_mean,
            TestlooPrec_GM_mean = looPrec_mean,
            TestlooBAcc_GM_mean = looBAcc_mean,
            TestlooSens_GM_sd   = looSens_sd,
            TestlooSpec_GM_sd   = looSpec_sd,
            TestlooPrec_GM_sd   = looPrec_sd,
            TestlooBAcc_GM_sd   = looBAcc_sd
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_GM.yaml")
  
  metrics$byclass %>% 
    filter(class  =="Class: G") %>% 
    select("looSens_mean","looSpec_mean","looPrec_mean","looBAcc_mean",
           "looSens_sd","looSpec_sd","looPrec_sd","looBAcc_sd") %>%
    rename (TestlooSens_G_mean = looSens_mean,
            TestlooSpec_G_mean = looSpec_mean,
            TestlooPrec_G_mean = looPrec_mean,
            TestlooBAcc_G_mean = looBAcc_mean,
            TestlooSens_G_sd   = looSens_sd,
            TestlooSpec_G_sd   = looSpec_sd,
            TestlooPrec_G_sd   = looPrec_sd,
            TestlooBAcc_G_sd   = looBAcc_sd
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_G.yaml")
  
  # Average the metrics by class
  metrics$byclass %>% 
    select("looSens_mean","looSpec_mean","looBAcc_mean","looPrec_mean") %>%
    summarise(TestlooBAcc_sd=sd(looBAcc_mean,na.rm=TRUE),
              TestlooSens_sd=sd(looSens_mean,na.rm=TRUE),
              TestlooSpec_sd=sd(looSpec_mean,na.rm=TRUE),
              TestlooPrec_sd=sd(looPrec_mean,na.rm=TRUE),
              TestlooSens_mean=mean(looSens_mean,na.rm=TRUE),
              TestlooSpec_mean=mean(looSpec_mean,na.rm=TRUE),
              TestlooBAcc_mean=mean(looBAcc_mean,na.rm=TRUE),
              TestlooPrec_mean=mean(looPrec_mean,na.rm=TRUE)
    ) %>%
    as.yaml() %>% 
    write("metrics/test_model_loocv_metrics_avg_all.yaml")
  
  overall<-c(metrics$overall,metrics$macro)
  names(overall)<-names(overall) %>% gsub(pattern = "(loo)(*.)", 
                                          replacement = "Test\\1\\2", 
                                          x = names(overall))
  overall %>% as.yaml %>%
    write("metrics/test_model_loocv_metrics_macro_overall.yaml")
  
  #metrics$overall %>% as.yaml %>%
  #write("metrics/test_model_loocv_metrics_overall.yaml")
  
  ## Save model 
  dir.create(dirname(opt$results), showWarnings = FALSE)
  saveRDS(results, file = paste0(opt$results,"_model_test_loocv_results.rds"))
  
  ## Save predictions results
  
}
