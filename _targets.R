library(targets)
library(upulR) # personal R package for creating Table-1

library(future)
library(future.callr)



# Define custom functions and other global objects -----------------------------
source("R/functions.R")
# source("R/helper_functions.R")



# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse",
                            "lmtp" ))
# assign study variables

# parms <- list(trt="poverty", 
#            outcome= "pain", 
#            baseline= c("age", "sex_male"),
#            outcome_type="binomial", 
#            svy = TRUE, 
#            wt_var = "int_wt")
# 
# parms2 <- list(trt="poverty", 
#            outcome= "pain", 
#            baseline= c("age", "sex_male", "educ_high_school", 
#                        "educ_high_school_college", "educ_college", 
#                        "race_white", "race_black", "race_hispanic", 
#                        "race_other_mixed"),
#            outcome_type="binomial", 
#            svy = TRUE, 
#            wt_var = "int_wt")



# shift functions to control exposure ------


d0 <- NULL

# increase income level by 10% (who are below poverty line)
# d1 <- function(data, trt) { 
#   
#   obs <- data[[trt]]
#   
#  ((obs< 1)* (obs*1.1)) +
#     ((obs>=1)* obs)
#     
# }


# increase income level by 25% (who are below poverty line)-----
d1 <- function(data, trt) {

  obs <- data[[trt]]

  ((obs< 1)* (obs*1.25)) +
    ((obs>=1)* obs)

}


# increase income level by 50% (who are below poverty line)----
d2 <- function(data, trt) {

  obs <- data[[trt]]

 ( (obs< 1)* (obs*1.50) )+
    ((obs>=1)* obs)

}

# increase income level by 75% (who are below poverty line)
d3 <- function(data, trt) {

  obs <- data[[trt]]

  ((obs< 1)* (obs*1.75)) +
    ((obs>=1)* obs)

}


# move everyone who are below poverty line to above poverty line
# d4 <- function(data, trt) { 
#   obs <- data[[trt]]
#   ((obs<1)* 1 )+
#     ((obs>=1)* obs)
#     
# }

  
# move everyone who are below 25th quantile income index to 25th income
d4 <- function(data, trt) {
  obs <- data[[trt]]
  q1 <- quantile(data[[trt]], na.rm = T)[["25%"]]

  ((obs<  q1)* q1) +
    ((obs>= q1)* obs)

}




# move everyone who are below median income index to median
d5 <- function(data, trt) {

  med <- median(data[[trt]], na.rm = T)

  (data[[trt]]< med)* med +
    (data[[trt]]>= med)* data[[trt]]

}

# move everyone who are below median income index to 75th quantile
# d7 <- function(data, trt) {
# 
#   q3 <- quantile(data[[trt]], na.rm = T)[["75%"]]
#   med <- median(data[[trt]], na.rm = T)
# 
#   (data[[trt]]< med)* med +
#     (data[[trt]]>= med)* data[[trt]]
# 
# }


out <- "pain1"

expo <- "poverty"

cov <- c( "age3c", "sex", "education", "ethnicity","insurance",
          "marital", "hh_size", "teeth_num", "caries_num")


plan(callr)
set.seed(198511110)

list(
  # load working data
  tar_target(df_file,
             "data/nhanes_extracted.rds",
             format = "file")
  ,
  
  # ----------------------------------------------------------------Working data 
  tar_target(working_df,
             readRDS(file=df_file))
  ,
  
  # -----------------------------------create a dataset for descriptive analysis
  tar_target(descriptive_data,
             get_desc_df(working_df))
  
  ,
  tar_target(imputed_df, mice::mice(descriptive_data, m = 10, 
                                    method = "rf",
                                    seed = 19851111) %>% 
               mice::complete("long") %>%
               as_tibble())
  
  ,
  tar_target(tmle_df,
             get_tmle_df(imputed_df, cov),
             format= "rds")
  ,
  
  # Shift functions----------------------------
  
  tar_target(d0,
             d0<- function(data, trt) { 
               
               data[[trt]]
             })
  ,
  
  tar_target(d1,
             function(data, trt) { 
               
               obs <- data[[trt]]
               
               ((obs< 1)* (obs*1.25)) +
                 ((obs>=1)* obs)
               
             })
  ,
  
  tar_target(d2,
             function(data, trt) { 
               
               obs <- data[[trt]]
               
               ((obs< 1)* (obs*1.50)) +
                 ((obs>=1)* obs)
               
             })
  ,
  
  tar_target(d3,
             function(data, trt) { 
               
               obs <- data[[trt]]
               
               ((obs< 1)* (obs*1.75)) +
                 ((obs>=1)* obs)
               
             })
  ,
  
  tar_target(d4,
             function(data, trt) {
               obs <- data[[trt]]
               q1 <- quantile(data[[trt]], na.rm = T)[["25%"]]
               
               ((obs<  q1)* q1) +
                 (obs>= q1)* obs
               
             })
             
  ,
  
  tar_target(d5,
             function(data, trt) {
               med <- median(data[[trt]], na.rm = T)
               
               (data[[trt]]< med)* med +
                 (data[[trt]]>= med)* data[[trt]]
               
             })
             
  ,
  
  
  
  
  # Set-up TMLE ----------------------------------------------------------------
  tar_target(a, expo)  # time varying exposure (2010 & 2013)
  ,
  
  tar_target(y, out)
  
  ,
  
  
  tar_target(w,
             tmle_df %>% 
               dplyr::select(sex,insurance, teeth_num, caries_num,
                             contains(c("age_3c",
                                        "education",
                                        "ethnicity",
                                        "marital",
                                        "hh_size"
                             ))) %>% colnames())
  
  ,
  
  tar_target(sl_lib, c("SL.glm", "SL.xgboost", "SL.nnet"))
  
  ,
  tar_target(sl_lib2, c("SL.glm","SL.gam", 
                        "SL.xgboost"
                        # ,
                        # "SL.randomForest", 
                        # "SL.mean"
                        ))
  
  ,
  
  tar_target(params,
             list(trt = a,
                  outcome = y ,
                  baseline = w ,
                  outcome_type = "binomial",
                  k=0,
                  svy=T,
                  wt_var = "int_wt"
             ))
  ,
  
  tar_target(params2,
             params %>% 
               modifyList(list(learners_outcome = sl_lib2,
                               learners_trt = sl_lib2))
             )
  ,
  
  tar_target(d, paste0("d",0:5))
  
  ,
  
  tar_target(m, 1:10)
  
  ,
  
  tar_target(nested_data_df, 
             tribble(~d,~m,~data,~params1,~params2,
               d,m,tmle_df,params,params2) %>% 
               unnest(cols = d) %>% 
               unnest(cols = m) %>% 
               mutate(data=map2(data,m, imp_filter)),
             
             format= "rds")
  ,
  # tar_target(nested_results_df,
  #            nested_data_df %>%
  #              # filter(m==1  & d=="d0") %>% 
  #              mutate(results= pmap(list(params,data, d),
  #                                   function (x,y,z) do.call(run_lmtp,
  #                                            c(x, 
  #                                              list(data= y ,
  #                                                   shift= eval(as.symbol(z)))))))
  #                                   apt install /tmp/chrome-remote-desktop_current_amd64.deb                %>%
  #              dplyr::select(-data,-params),
  #            format= "rds")
  # ,
  # 
  tar_target(nested_results_df2,
             nested_data_df %>%
               # filter(m==1  & d=="d0") %>% 
               mutate(results= pmap(list(params2,data, d),
                                    function (x,y,z) do.call(run_lmtp,
                                             c(x, 
                                               list(data= y ,
                                                    shift= eval(as.symbol(z)))))))
                                                    %>%
               dplyr::select(-data,-params2, -params1),
             format= "rds")
  ,

  tar_target(contrast_results_df2,
             nested_results_df2 %>%group_by(m,d) %>%

               pivot_wider(names_from = d, values_from = results ) %>%
               ungroup() %>%
               mutate(
                 d0_vs_d1= map2(d1,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 d0_vs_d2= map2(d2,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 d0_vs_d3= map2(d3,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 d0_vs_d4= map2(d4,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 d0_vs_d5= map2(d5,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 # d0_vs_d6= map2(d6,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 # d0_vs_d7= map2(d7,d0,~lmtp::lmtp_contrast(.x,ref = .y, type="or")),
                 )
             %>%
               dplyr::select(m,contains("vs")) %>%
               pivot_longer(!m, names_to = "contrast", values_to = "results") %>%
               mutate(results= map(results,~.$vals)) %>%
               unnest(cols = results),
             format= "rds")
  ,

  tar_target(results_main_2,
             contrast_results_df2 %>%
               pool_estimates(mi=10) %>%
               round_uc(),
             format= "rds")
 
  )








