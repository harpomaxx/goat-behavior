#!/bin/Rscript
# calculate SHAP values

source("code/R/functions/plot_shap.R")
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(yaml))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(caret))

#### MAIN 

option_list <- list(
  make_option(
    "--input",
    action = "store",
    type = "character",
    help = "Set the name of the input data file in tsv format"
  ),
  make_option(
    "--shapvalues",
    action = "store",
    type = "character",
    help = "Set the name of the file with  SHAP values"
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
    help = "Set the basename for storing SHAP plots"
  )
)
opt <- parse_args(OptionParser(option_list=option_list))

if (opt$input %>% is.null()  ||
    opt$shapvalues %>% is.null() ||
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
  message("[] Read SHAP values ",opt$shapvalues)
  shap_values <- 
    readr::read_delim(opt$shapvalues, col_types = cols(), delim = '\t')
  
  ## Save summary_plots
  summary_plot<-shap_summary_plot(shap_values) 
  beeswarm_plot<-shap_beeswarm_plot(shap_values,dataset) 
  
  dir.create(opt$output, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave( path =  opt$output, 
                   filename="shap_values_summary_plot.png",
                   device = "png",
                   plot = summary_plot)
  ggplot2::ggsave( path =  opt$output, 
                   filename="shap_values_beeswarm_plot.png",
                   device = "png",
                   plot = beeswarm_plot)
  
  
  }