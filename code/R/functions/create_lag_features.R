suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

#' create lag features based on previous values of ICETAG
#'
#' @param dataset 
#'
#' @return dataset plus lag features
#' @export
#'
#' @examples
create_lag_features <- function(dataset) {
  act_timeseries <- dataset  %>%
    mutate(epoch = paste(Date, `Date/Time`) %>% mdy_hms())
  act_timeseries <-
    act_timeseries %>% mutate(day = cut(
      as.POSIXct(act_timeseries$epoch, origin = "1950-01-01", tz = "GMT"),
      breaks = "1 day"))
  
  dataset <- act_timeseries %>% arrange(epoch) %>% group_by(Anim) %>%
    mutate(
      prev_Active1 = lag(
        Active,
        order_by = Anim,
        n = 1,
        default = 0
      ),
      prev_Active2 = lag(
        Active,
        order_by = Anim,
        n = 2,
        default = 0
      ),
      prev_Active3 = lag(
        Active,
        order_by = Anim,
        n = 3,
        default = 0
      )
    ) %>%
    mutate(
      prev_Standing1 = lag(
        Standing,
        order_by = Anim,
        n = 1,
        default = 0
      ),
      prev_Standing2 = lag(
        Standing,
        order_by = Anim,
        n = 2,
        default = 0
      ),
      prev_Standing3 = lag(
        Standing,
        order_by = Anim,
        n = 3,
        default = 0
      )
    ) %>%
    mutate(
      prev_Lying1 = lag(
        Lying,
        order_by = Anim,
        n = 1,
        default = 0
      ),
      prev_Lying2 = lag(
        Lying,
        order_by = Anim,
        n = 2,
        default = 0
      ),
      prev_Lying3 = lag(
        Lying,
        order_by = Anim,
        n = 3,
        default = 0
      )
    ) %>%
    mutate(
      prev_steps1 = lag(
        Steps,
        order_by = Anim,
        n = 1,
        default = 0
      ),
      prev_steps2 = lag(
        Steps,
        order_by = Anim,
        n = 2,
        default = 0
      ),
      prev_steps3 = lag(
        Steps,
        order_by = Anim,
        n = 3,
        default = 0
      ) 
    ) %>%
    mutate(
      prev_headdown1 = lag(
        `%HeadDown`,
        order_by = Anim,
        n = 1,
        default = 0
      ),
      prev_headdown2 = lag(
        `%HeadDown`,
        order_by = Anim,
        n = 2,
        default = 0
      ),
      prev_headdown3 = lag(
        `%HeadDown`,
        order_by = Anim,
        n = 3,
        default = 0
      )
    )
  dataset<- dataset %>% ungroup()
  
  ## Selecting only numeric features
  #dataset_numeric<-dataset %>% select(-'Date/Time',-Date,-epoch) %>% select_if(~class(.) == 'numeric')
  #dataset<-dataset_numeric %>% tibble::add_column(Anim=dataset$Anim)
}