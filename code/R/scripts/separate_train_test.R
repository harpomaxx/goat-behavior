suppressPackageStartupMessages(library(dplyr))

#' Title
#'
#' @param dataset 
#'
#' @return a list with train and test set
#' @export
#'
#' @examples
separate_train_test<-function(dataset) {
  test_dataset <-
    dataset %>% filter(!(Anim %in% c(1553, 1635, 1779, 505081, 505075, 505080)))
  train_dataset <-
    dataset %>% filter((Anim %in% c(1553, 1635, 1779, 505081, 505075, 505080)))
  return( list(trainset=train_dataset,testset=test_dataset))
}