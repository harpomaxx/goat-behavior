#!/bin/Rscript
# calculate SHAP values

source("code/R/functions/calculate_shap.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(ggplot2))

#### MAIN 

option_list <- list(
  make_option(
    "--input",
    action = "store",
    type = "character",
    help = "Set the name of the input data file in tsv format"
  ),
  make_option(
    "--model",
    action = "store",
    type = "character",
    help = "Set the name model to use for SHAP calculation"
  ),
  
  make_option(
    "--selected_variables",
    action = "store",
    type = "character",
    help = "Set the name selected variables in tsv format"
  ),
  
  make_option(
    "--output",
    action = "store",
    type = "character",
    default = "",
    help = "Set the basename of the SHAP values results"
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null()  ||
    opt$model %>% is.null() ||
    opt$selected_variables %>% is.null() ||
    opt$output %>% is.null()) {
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
} else{
  ## Read Dataset
  message("[] Read dataset ",opt$input)
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  
  ## Read selected Variables
  message("[] Read selected variables",opt$selected_variables)
  selected_variables <-
    readr::read_delim(opt$selected_variables, col_types = cols(), delim = '\t')
 
  dataset <- dataset %>% select(selected_variables$variable,Activity) 
  ## Load  model
  message("[] Load model ",opt$model)
  model <- readRDS(opt$model)
  
  ## Set default parameters
  params <- yaml::read_yaml("params.yaml")
  if(!  "calculate_shap" %in% names(params)) { 
    message("[] Error: No information for Monte Carlo simulations")
    quit()
  }
  
  message("[] Calculate SHAP values for ", opt$input, " with ", 
          params$calculate_shap$nsim, " simulations")
  shap_values<- 
    calculate_shap(dataset = dataset,
                        model = model,nsim=params$calculate_shap$nsim)
  
  ## Save SHAPvalues
  dir.create(dirname(opt$output), showWarnings = FALSE,recursive = TRUE)
  readr::write_delim(shap_values, file=paste0(opt$output),delim="\t")
  }
