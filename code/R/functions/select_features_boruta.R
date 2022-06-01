suppressPackageStartupMessages(library(Boruta))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tibble))

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
  set.seed(19091974) 
  boruta_formula<-as.formula(paste0(category,"~."))
  var_importance_boruta_raw <-
    Boruta(boruta_formula, data = train_dataset_boruta %>% na.omit(),num.threads=2, doTrace =2, ntree = 30 )
  var_importance_boruta <-
    attStats(var_importance_boruta_raw) %>%
    filter(decision == 'Confirmed') %>%
    select(meanImp) %>%
    arrange(desc(meanImp)) %>% top_n(num_of_feat,meanImp)
  var_importance_boruta <- var_importance_boruta %>%
    tibble::rownames_to_column('variable')
  var_importance_boruta
}


#' Select the Max N features observed during all the bootstrap resampling.
#' Since for selecting a variable, it has to be present Boruta output,
#' It could be possible to obtain a value less than `num_of_feat`
#' 
#' NOTICE that the algorithm depends on feature's order. If we use two 
#' datasets with the same features but different order, the selected features
#' could be different
#' 
#' @param dataset 
#' @param num_of_feat the max number of features to be selected 
#' @param num_of_samp the number of samples used by bootstrap
#' @return dataframe with top N features
#' @export
#'
#' @examples
select_maxn_features <- function(dataset,
                                 max_num_of_feat = 20,
                                 num_of_samp = 30){
  suppressPackageStartupMessages(library(doMC))
  suppressPackageStartupMessages(library(rsample))
  registerDoMC(cores = 4)
  message("[] Selecting best features.",appendLF = FALSE )
  set.seed(19091974) 
  resamples <-
    rsample::bootstraps(dataset, strata = Activity, times = num_of_samp)
  var_importance <-
    foreach(i = 1:length(resamples$splits), .combine = rbind) %dopar% {
      trainset <- rsample::analysis(resamples$splits[[i]])
      partial <- select_features_boruta(trainset, "Activity",max_num_of_feat)
      partial <- cbind(partial, rsample = i)
      message(".",appendLF = FALSE)
      partial
    }
  message("\n[] Done.")
  var_always_present <-
    var_importance %>% group_by(variable) %>% summarise(n = n()) %>% 
    filter(n == num_of_samp) %>% ungroup() %>% select(variable)
  v <- var_always_present %>% unlist() %>% unname()
  var_importance_boruta_final <-
    var_importance %>% filter(variable %in% v) %>% group_by(variable) %>% 
    summarise(grandmeanImp = mean(meanImp)) %>% 
    arrange(grandmeanImp %>% desc()) %>% ungroup() %>% select(variable)
  
  var_importance_boruta_final <- 
    apply(var_importance_boruta_final$variable %>% t() , 2,
          function(x)
            stringr::str_replace_all(x,
                                     pattern = "`",
                                     replacement = "")) %>% as.data.frame()
  names(var_importance_boruta_final) <- "variable"
  var_importance_boruta_final
}
