#!/bin/Rscript
#  select initial features


source("code/R/scripts/select_initial_features.R")
suppressPackageStartupMessages(library(optparse))

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
  
  ## Select features
  dataset <- readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  dataset <- select_initial_features(dataset)
  ## Save dataset
  dir.create(dirname(opt$output), showWarnings = FALSE)
  readr::write_delim(dataset, file = opt$output, delim = '\t')
}