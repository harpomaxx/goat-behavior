require(Boruta)
require(dplyr)
require(tibble)

#' Feature Selection using Boruta
#'
#' @param train_dataset_boruta a dataset with the features to select
#' @param category the name of column to be used as category/class
#' @param num_of_feat the top number of features to be selected 
#'
#' @return a dataframe with top `num_of_feat` features
#' @export
#'
#' @examples
#' 
#' trainset <- matrix( runif(100), nrow = 30, ncol = 10 )
#' category <- sample( 0:1,30, replace = T )
#' trainset <- cbind(trainset,class=as.factor(category)) %>% as.data.frame()
#" features <- select_features_boruta(trainset, 
#'                             category = "class",num_of_feat = 5)
#' 

select_features_boruta <- function(train_dataset_boruta, 
                                   category ,
                                   num_of_feat = 20) {
  boruta_formula<-as.formula(paste0(category,"~."))
  var_importance_boruta_raw <-
    Boruta(boruta_formula, data = train_dataset_boruta %>% na.omit())
  var_importance_boruta <-
    attStats(var_importance_boruta_raw) %>%
    filter(decision == 'Confirmed') %>%
    select(meanImp) %>%
    arrange(desc(meanImp)) %>% top_n(num_of_feat)
  var_importance_boruta <- var_importance_boruta %>%
    tibble::rownames_to_column('variable')
  var_importance_boruta
}