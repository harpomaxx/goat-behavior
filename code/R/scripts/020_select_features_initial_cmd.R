#!/bin/Rscript
#  select initial features


source("code/R/functions/select_features_initial.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(yaml))

#### MAIN 

option_list <- list(
  make_option("--input", action="store", type="character", help = "Set the name of the input data file in tsv format"),
  make_option("--output", action="store", type="character", default="", help = "Set the name of the output data file in tsv format")
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null() || opt$output %>% is.null()){
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
}else{
  
  ## Set default parameters
  params <- yaml::read_yaml("params.yaml")
  if(!  "select_initial" %in% names(params)) { 
    message("[] Error: No information for selecting features.")
    quit()
  }
  ## Select features
  dataset <- readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  dataset <- select_initial_features(dataset,params$select_initial$features)
  ## Save dataset
  dir.create(dirname(opt$output), showWarnings = FALSE)
  readr::write_delim(dataset, file = opt$output, delim = '\t')
  ## save metrics
  list("info"=list("nfeat_sel"=ncol(dataset)))%>% 
    as.yaml() %>% write("metrics/select_features_initial.yaml",)
  
}
