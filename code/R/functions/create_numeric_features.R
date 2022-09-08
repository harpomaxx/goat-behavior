suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(lubridate))


#' 
#' Remove unwanted features and create new ones.
#' 
#' @param input_file 
#'
#' @return null
#' @export
#'
#' @examples
create_numeric_features <- function(dataset) {
  dataset$Anim <- as.factor(dataset$Anim)
  dataset$collar <- as.factor(dataset$collar)
  dataset$Tag <- as.factor(dataset$Tag)
  dataset <- dataset %>% na.omit()
  
  names(dataset)[14] <- "distance(m)"
  names(dataset)[17] <- "%HeadDown"
  
  ####  (RL and RS) vs the rest of the classes
  dataset <- dataset %>%
    mutate(Activity = ifelse(Activity %in% c("RS", "RL"), "R", Activity))
  
  ## Removing problematic registers
  ## - Activity labeled as OTHER
  ## - Animals with very few activities
  dataset <- dataset %>% filter(Activity != "OTHER")
  dataset <- dataset %>% filter(Anim != "505067")
  
  dataset <- dataset %>% ungroup() %>% select(
    -lat,
    -long,
    -logx,
    -logy,
    -logdiffxy,
    -logmeanxy,
    -logvarxy,
    -Diffxy,
    -Meanxy,
    -Varxy,
    -logdistance,
    -SMA,
    -DOP
  )
  dataset <-
    dataset %>% mutate(DiffXY = abs(X_Act - Y_Act))
  dataset <-
    dataset %>% rowwise() %>% mutate(MeanXY = mean(c(X_Act, Y_Act))) %>% ungroup()
  dataset <-
    dataset %>% rowwise() %>% mutate(VarXY = sd(c(X_Act, Y_Act))) %>% ungroup()
  
  # Scale distance and steps
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  dataset %>% mutate(Steps=range01(Steps),`distance(m)`=range01(`distance(m)`))
  
  dataset <- dataset %>%  mutate(
    X_Actlog = log10(X_Act + 1e-10),
    Y_Actlog = log10(Y_Act + 1e-10),
    Stepslog = log10(Steps + 1e-10),
    `distance(m)log` = log10(`distance(m)` + 1e-10),
    VarXYlog = log10(VarXY + 1e-10),
    DiffXYlog = log10(DiffXY + 1e-10),
    MeanXYlog = log10(MeanXY + 1e-10)
    #DOPlog = log10(DOP + 1e-10)
  )
  
  dataset %>% ungroup()
  #dataset$tag_activity<- dataset %>% select(Active,Lying,Standing) %>% 
  #   apply( MARGIN=1, FUN=which.max)# %>% as.factor()
}

rename_animals <- function(dataset){

  dataset$Anim <- as.character(dataset$Anim)
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1553"), "a1"))			#a1
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1635"), "a15"))			#a2
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1636"), "a3"))			#a3
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1646"), "a14"))			#a4
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1677"), "a11"))			#a5
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1713"), "a6"))			#a6
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"1779"), "a8"))			#a7
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505001"), "a7"))			#a8
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505019"), "a9"))			#a9
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505035"), "a10"))			#a10
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505048"), "a5"))			#a11
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505058"), "a12"))			#a12
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505071"), "a13"))			#a13
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505075"), "a4"))			#a14
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505080"), "a2"))			#a15
  dataset<- dataset %>% mutate(Anim = replace(Anim, stringr::str_detect(Anim,"505081"), "a16"))			#a16
  dataset
}
