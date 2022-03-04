#!/bin/Rscript
#  separate train and test datasets

source("code/R/scripts/separate_train_test.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

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
  ## Select features
  dataset <-
    readr::read_delim(opt$input, col_types = cols(), delim = '\t')
  split_datasets <- separate_train_test(dataset = dataset)
  ## Save dataset
  dir.create(dirname(opt$output), showWarnings = FALSE)
  readr::write_delim(split_datasets$trainset, file = paste0(opt$output,"_trainset.tsv"), delim = '\t')
  readr::write_delim(split_datasets$testset, file = paste0(opt$output,"_testset.tsv"), delim = '\t')
}
