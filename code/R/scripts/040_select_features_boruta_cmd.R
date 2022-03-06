#!/bin/Rscript
#  Select variables using Boruta algorithm

source("code/R/functions/select_features_boruta.R")
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
    "--output",
    action = "store",
    type = "character",
    default = "",
    help = "Set the basename of the output data files in tsv format"
  ),
  make_option(
    "--maxnfeat",
    action = "store",
    type = "integer",
    default = 40,
    help = "Set the max number of features to select."
  ),
  make_option(
    "--numsamp",
    action = "store",
    type = "integer",
    default = 30,
    help = "Set the number of samples in boostrap"
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null() ||
    opt$output %>% is.null()) {
  message("[] Parameters missing. Please use --help for looking at the available parameters.")
  quit()
} else{
  ## Read dataset
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  dataset$Activity<-as.factor(dataset$Activity)
  ## Set default parameters
  maxnfeat <- opt$maxnfeat
  numsamp <- opt$numsamp
  params <- yaml::read_yaml("params.yaml")
  if( "select_features_boruta" %in% names(params)){
    maxnfeat <- params$select_features_boruta$maxnfeat
    numsamp <- params$select_features_boruta$numsamp
  }
  ## Select features
  selected_features_dataframe <-
    select_maxn_features(dataset = dataset, 
                         max_num_of_feat = maxnfeat,
                         num_of_samp = numsamp)
  ## Save dataset
  dir.create(dirname(opt$output), showWarnings = FALSE)
  readr::write_delim(selected_features_dataframe, 
                     file = paste0(opt$output,"_selected_features.tsv"), 
                     delim = '\t')
  ## Save Metric
  list("info"=list("nfeat_sel"=nrow(selected_features_dataframe)))%>% 
    as.yaml() %>% write("metrics/select_features_boruta.yaml",)
  #list("number_of_features_selected"=nrow(selected_features_dataframe))%>% rjson::toJSON() %>% write("select_features_boruta-nfeat.json")
  
} 
