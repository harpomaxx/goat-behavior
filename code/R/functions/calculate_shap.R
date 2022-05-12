suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fastshap)) # for fast (approximate) Shapley values
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(doMC))

registerDoMC(cores = 10)


p_function_G <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "G"]
p_function_GM <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "GM"]
p_function_R <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "R"]
p_function_W <-
  function(object, newdata)
    caret::predict.train(object, newdata = newdata, type = "prob")[, "W"]

#' Title
#'
#' @param dataset 
#' @param model 
#' @param nsim number of monte carlo simulation
#'
#' @return
#' @export
#'
#' @examples
calculate_shap <- function(dataset,model,nsim=10) {
#  library(doParallel)
#  registerDoParallel(8)
  
  trainset <- dataset %>%  na.omit() %>%
    as.data.frame()
  trainset_y <- dataset %>%
    select(Activity) %>%
    na.omit() %>%
    unlist() %>%
    unname()
  trainset <- trainset %>% select(-Activity)
  trainset_G <- trainset[which(trainset_y == "G"), ]
  trainset_GM <- trainset[which(trainset_y == "GM"), ]
  trainset_R <- trainset[which(trainset_y == "R"), ]
  trainset_W <- trainset[which(trainset_y == "W"), ]
  
  
  # Compute fast (approximate) Shapley values using 50 Monte Carlo repetitions
  message(" - Calculating SHAP values for class G")
  shap_values_G <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_G,
      nsim = nsim,
      newdata = trainset_G,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class GM")
  shap_values_GM <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_GM,
      nsim = nsim,
      newdata = trainset_GM,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class R")
  shap_values_R <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_R,
      nsim = nsim,
      newdata = trainset_R,
      .parallel = TRUE
    )
  message(" - Calculating SHAP values for class W")
  shap_values_W <-
    fastshap::explain(
      model,
      X = trainset,
      pred_wrapper = p_function_W,
      nsim = nsim,
      newdata = trainset_W,
      .parallel = TRUE
    #  adjust = TRUE
    )
  
 shap_values_GM$class<-"GM"
 shap_values_G$class<-"G"
 shap_values_R$class<-"R"
 shap_values_W$class<-"W" 
  
 shap_values<-rbind(shap_values_G,
                     shap_values_GM,
                     shap_values_R,
                     shap_values_W)
 shap_values
}


shap_summary_plot<-function(shap_values){
  summary_plot <-
    shap_values %>% reshape2::melt() %>% group_by(class, variable) %>% 
    summarise(mean = mean(abs(value))) %>% 
    arrange(desc(mean)) %>%
    ggplot() +
    ggdark::dark_theme_classic() +
    geom_col(aes(
      y = variable,
      x = mean,
      group = class,
      fill = class
    ), position = "stack") +
    xlab("Mean(|Shap Value|) Average impact on model output magnitude")
  summary_plot
  
}

shap_beeswarm_plot<-function(shap_values,dataset){
  
  shap_values <- shap_values %>% reshape2::melt()
  dataset<-dataset %>% mutate(class=Activity) %>% select(-Activity) %>% 
    reshape2::melt() %>% group_by(variable) %>% 
    mutate(value_scale=range01(value))
  
  beeswarm_plot<-cbind(shap_values, feature_value=dataset$value_scale) %>% # filter(class=="GM") %>%
    ggplot()+
    facet_wrap(~class)+
    #ggdark::dark_theme_bw()+
    theme_classic()+
    geom_hline(yintercept=0, 
               color = "red", size=0.5)+
    ggforce::geom_sina(aes(x=variable,y=value,color=feature_value),size=2.2,bins=4,alpha=0.9,shape=15)+
    scale_colour_gradient(low = "yellow", high = "red", na.value = NA)+
    scale_colour_gradient(low = "skyblue", high = "orange", na.value = NA)+
    xlab("Feature")+ylab("SHAP value")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  beeswarm_plot
  
  
}