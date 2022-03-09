suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

#' select  features based on disciplinar knowledge
#'
#' @param dataset 
#'
#' @return dataset with selected features
#' @export
#'
#' @examples


select_initial_features<-function(dataset){
  dataset$Anim <- as.factor(dataset$Anim)
  dataset$collar <- as.factor(dataset$collar)
  dataset$Tag <- as.factor(dataset$Tag) 
  dataset_numeric <- dataset %>% select(
    "Anim",           
    "Standing",       
    "Active",         
    "Lying",         
#    "Steps",          
#    "distance(m)",    
#    "X_Act",          
#    "Y_Act",          
    "%HeadDown",      
    "DFL1",          
    "DFL2",           
    "DFL3",           
    "DFA12",          
    "DFA123",         
    "DBL1",           
    "DBL2",           
    "DBL3",           
    "DBA12",         
    "DBA123",         
#    "DiffXY",         
#    "MeanXY",         
#    "VarXY",          
    "X_Actlog",       
    "Y_Actlog",       
    "Stepslog",       
    "distance(m)log",
    "VarXYlog",       
    "DiffXYlog",      
    "MeanXYlog",      
    "prev_Active1",   
    "prev_Active2",   
    "prev_Active3",  
    "prev_Standing1", 
    "prev_Standing2", 
    "prev_Standing3", 
    "prev_Lying1",    
    "prev_Lying2",    
    "prev_Lying3",    
    "prev_steps1",    
    "prev_steps2",   
    "prev_steps3")   
  ## Selecting only numeric features
  dataset_numeric<-dataset %>% select(-'Date/Time',-Date,-epoch) %>% select_if(~class(.) == 'numeric')
  dataset_numeric<-dataset_numeric %>% tibble::add_column(Anim=dataset$Anim)
  dataset_numeric %>% tibble::add_column(Activity=as.factor(dataset$Activity))    
}    
    
    
    
    #-`distance(m)`,
#    -`distance(m)log`,
    #-Stepslog,
#    -Steps,
#    -X_Act,
#    -Y_Act,
    #-Active,
    #-Lying,
    #-Standing,
#    -VarXY,
#    -DiffXY,
#    -MeanXY,
    #-tag_activity,
    #-DOP
  
