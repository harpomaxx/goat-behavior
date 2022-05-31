suppressPackageStartupMessages(library(dplyr))

#' Separate the dataset  separate into two datasets 
#'
#' @param dataset a dataset with goat behavior
#' @param excluded_animals the list of animals to include on each dataset
#'
#' @return a list with train and test set
#' @export
#'
#' @examples
separate_datasets<-function(dataset,excluded_animals) {
  test_dataset <-
    #dataset %>% filter(!(Anim %in% c(1553, 1635, 1779, 505081, 505075, 505080)))
    dataset %>% filter(!(Anim %in% excluded_animals))
  train_dataset <-
    #dataset %>% filter((Anim %in% c(1553, 1635, 1779, 505081, 505075, 505080)))
    dataset %>% filter((Anim %in% excluded_animals))
  return( list(trainset=train_dataset,testset=test_dataset))
}