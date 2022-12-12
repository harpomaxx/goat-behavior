suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

#' select  features based on disciplinar knowledge
#'
#' @param dataset 
#' @param initial_features
#'
#' @return dataset with selected features
#' @export
#'
#' @examples

select_initial_features<-function(dataset, initial_features){
  dataset$Anim <- as.factor(dataset$Anim)
  #dataset$collar <- as.factor(dataset$collar)
  #dataset$Tag <- as.factor(dataset$Tag) 
  print(initial_features)
  dataset_numeric <- dataset %>% select(all_of(initial_features))
  ## Selecting only numeric features
  #dataset_numeric<-dataset %>% select(-'Date/Time',-Date,-epoch) %>% select_if(~class(.) == 'numeric')
  #dataset_numeric<-dataset_numeric %>% tibble::add_column(Anim=dataset$Anim)
  dataset_numeric %>% tibble::add_column(Activity=as.factor(dataset$Activity))    
}    
    
    
  