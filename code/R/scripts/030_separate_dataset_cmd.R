#!/bin/Rscript
#  separate into two datasets (namely train and test datasets)
# trainset is used for feature  analysis and test for LOOCV results

source("code/R/functions/separate_dataset.R")
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
    "--output",
    action = "store",
    type = "character",
    default = "",
    help = "Set the basename of the output data files in tsv format"
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null() ||
    opt$output %>% is.null()) {
  message("[] Parameters missing. Please use --help for look at available parameters.")
  quit()
} else{
  ## Read Dataset
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  ## Set default parameters
  params <- yaml::read_yaml("params.yaml")
  if(!  "separate_dataset" %in% names(params)) { 
    message("[] Error: No information for excluding animals")
    quit()
    }
  split_datasets <- 
    separate_datasets(dataset = dataset,
                        excluded_anim=params$separate_dataset$excluded_anim)
  ## Save dataset
  dir.create(dirname(opt$output), showWarnings = FALSE)
  readr::write_delim(split_datasets$trainset, file = paste0(opt$output,"_features.tsv"), delim = '\t')
  readr::write_delim(split_datasets$testset, file = paste0(opt$output,"_loocv.tsv"), delim = '\t')
  ## Save Metric
  list("info" = list(
    "nrow_features" = nrow(split_datasets$trainset),
    "nrow_loocv" = nrow(split_datasets$testset)
  )) %>% as.yaml() %>% write("metrics/separate_dataset.yaml")
}
