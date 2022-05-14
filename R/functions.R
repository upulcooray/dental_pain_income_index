

get_desc_df <- function(data){
  
  svy_vars<- c(id= "SEQN", 
               cycle= "SDDSRVYR", 
               psu= "SDMVPSU", 
               strata= "SDMVSTRA",
               int_wt= "WTINT2YR", 
               exam_wt= "WTMEC2YR")
  
  # Exposure
  expo <- c(poverty= "INDFMPIR")
  
  # Outcome
  # OHQ620 - How often last yr had aching in mouth?
  out <- c(den_pain= "OHQ620")
  
  # covariates
  c_vars <-   c(age= "RIDAGEYR", 
                sex= "RIAGENDR",
                education= "DMDEDUC2", 
                ethnicity= "RIDRETH1", 
                income= "INDFMIN2", 
                married= "DMDMARTL", 
                hh_size= "DMDHHSIZ",
                insurance= "HIQ011")
  
  
  teeth_count_vars<- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"TC")
  caries_count_vars <- paste0("OHX",sprintf("%02d",c(2:15,18:31)),"CTC")
  
  data %>%                    # n= 29,400
    # only who under go dental examination
    dplyr::filter(OHDDESTS =="Complete") %>%   # n= 25,921
    # select variables
    dplyr::select(all_of(c(svy_vars,
                           expo,out,
                           c_vars,
                           teeth_count_vars,
                           caries_count_vars))) %>%
    
    # only adults 20yr to 70
    dplyr::filter(age>20 & age<70 & poverty!=5 ) %>%     # n= 13,109
    #count number of teeth & caries
    mutate_at(vars(all_of(teeth_count_vars)), 
              ~if_else(.=="Permanent tooth present",1,0)) %>% 
    mutate_at(vars(all_of(caries_count_vars)), 
              ~if_else(.=="Permanent tooth with a dental ca",1,0)) %>% 
    mutate(teeth_num= rowSums(select(.,teeth_count_vars))) %>% 
    mutate(caries_num= rowSums(select(.,caries_count_vars))) %>% 
    # filter out edentulous people
    dplyr::filter(teeth_num>0)   %>%           # n= 12,505
    # removing tooth level variables
    select(-all_of(teeth_count_vars)) %>% 
    select(-all_of(caries_count_vars)) %>% 
    # creating two outcome varibles with (different cut points)
    mutate( pain1= if_else(den_pain=="Very often" |
                             den_pain== "Fairly often", 1,0),
            pain2= if_else(den_pain=="Very often" |
                             den_pain== "Fairly often"|
                             den_pain== "Occasionally", 1,0)) %>% 
    # cleaning education variable
    mutate(education= fct_recode(education,
                                 less_than_hs= "Less than 9th grade",
                                 less_than_hs= "9-11th grade (Includes 12th grad",
                                 hs_or_college= "High school graduate/GED or equi",
                                 hs_or_college= "Some college or AA degree",
                                 college_or_above= "College graduate or above",
                                 NULL= "Don't Know",
                                 NULL= "Refused")) %>% 
    # cleaning income variable
    mutate(income= fct_recode(income,
                              less_than_25k= "$ 0 to $ 4,999",
                              less_than_25k= "$ 5,000 to $ 9,999",
                              less_than_25k= "$10,000 to $14,999",
                              less_than_25k= "$15,000 to $19,999",
                              less_than_25k= "$20,000 to $24,999",
                              less_than_25k= "Under $20,000",
                              less_than_25k= "$20,000 and Over",
                              `25k_to_75K`= "$25,000 to $34,999",
                              `25k_to_75K`= "$35,000 to $44,999",
                              `25k_to_75K`= "$45,000 to $54,999",
                              `25k_to_75K`= "$55,000 to $64,999",
                              `25k_to_75K`= "$65,000 to $74,999",
                              `75k_or_over`= "$75,000 to $99,999",
                              `75k_or_over`= "$100,000 and Over",
                              NULL= "Refused",
                              NULL= "Don't know" )) %>% 
    
    # cleaning marital status
    mutate(marital= fct_recode(married,
                               married_or_with_partner= "Married",
                               married_or_with_partner= "Living with partner",
                               widowed_divorced_seperated= "Widowed",
                               widowed_divorced_seperated= "Divorced",
                               widowed_divorced_seperated= "Separated",
                               never_married= "Never married",
                               NULL= "Refused")) %>% 
  
    mutate(hh_size= fct_recode(hh_size,
                               `6_or_more`= "6",
                               `6_or_more`= "7 or more people in the Househol")) %>% 
    
    # create age categories
    mutate(age3c= case_when(age< 36 ~ "<=35",
                            age> 35 & age< 56 ~ "36-55",
                            TRUE ~ ">55") %>% 
             forcats::fct_relevel(c("<=35","36-55",">55"))) %>% 
    # Cleaning race
    mutate(ethnicity = fct_recode(ethnicity,
                                  hispanic= "Mexican American",
                                  hispanic= "Other Hispanic",
                                  other_mixed= "Other Race - Including Multi-Rac",
                                  white= "Non-Hispanic White",
                                  black= "Non-Hispanic Black") %>% 
             fct_relevel(c("white",
                             "black",
                             "hispanic",
                             "other_mixed"))) %>%
    # Cleaning status of medical insurance
    mutate(insurance= fct_recode(insurance,
                                 NULL= "Refused", 
                                 NULL= "Don't know"))
}



get_tmle_df <- function(data,cov){
  
  dums_for <- data %>% 
    select(cov) %>% 
    dplyr::select_if(function(x)
    length(unique(x))< 7 & length(unique(x))>2) %>%
    colnames()
  
  binary <- data %>% 
    select(cov) %>% 
    dplyr::select_if(function(x) length(unique(x))==2) %>%
    colnames()
  
  tmle_df <- data %>% 
    # create dummies for categorical variables
    fastDummies::dummy_cols(dums_for,
                            remove_first_dummy = T,
                            ignore_na = T,
                            remove_selected_columns = T) %>%
    janitor::clean_names() %>% 
    mutate_all(as.numeric) %>% 
    mutate_at(vars(binary), function(x) x-1) 
}



# ==============================================================================


run_lmtp <- function(data,
                     shift=NULL, 
                     svy=FALSE, 
                     wt_only=FALSE, 
                     wt_var="",
                     ...){
  
  if (svy==TRUE){
    
    svy <- survey::svydesign(~psu, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    psu <- svy$psu
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt,
                         id = psu
      ))
  }
  
  else if (wt_only==TRUE){
    
    svy <- survey::svydesign(~1, weights = data[[wt_var]], data = data)
    wt <- svy$prob
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(...,
                         data=data,
                         shift=shift,
                         weights = wt
      ))
    
  } 
  
  else {
    
    progressr::with_progress(
      m<-lmtp::lmtp_tmle(..., 
                         data=data,
                         shift=shift))
    
  }
  
  return(m)
}
# ============================================================================= 
  
imp_filter <- function(data, m){
  
  data %>% 
    dplyr::filter(imp==m)
  
}


pool_estimates <- function(df,mi=5){
  
  # from https://rdrr.io/cran/mice/src/R/barnard.rubin.R
  barnard.rubin <- function(m, b, t, dfcom = Inf) {
    lambda <- (1 + 1 / m) * b / t
    lambda[lambda < 1e-04] <- 1e-04
    dfold <- (m - 1) / lambda^2
    dfobs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    ifelse(is.infinite(dfcom), dfold, dfold * dfobs / (dfold + dfobs))
  }
  
  df %>%
    group_by(contrast,.groups = 'keep') %>%
    dplyr::mutate(variance= std.error^2,
                  p.z = qnorm(p.value)) %>%
    dplyr::summarise(
      p.z.mean= mean(p.z),
      p.den= sqrt(1 + var(p.z)),
      p.combined= pnorm( p.z.mean / p.den),
      qbar = mean(theta),
      ubar = mean(variance), # Within imputation variance
      b = var(theta), # Between imputation variance
      t = ubar + (1 + 1 / mi) * b, # Total variance
      SE.combined = sqrt(t),
      df = barnard.rubin(mi, b, t), #df correction
      conf.low = qbar - qt(0.975, df)*SE.combined,
      conf.high = qbar + qt(0.975, df)*SE.combined) %>%
    dplyr::select(contrast, theta= qbar,conf.low,
                  conf.high, p.value= p.combined) %>%
    ungroup()
}


get_combined_results <- function(results_df, type, ref) {
  
  m <- 5 # number of imputes data sets
  
  ref_name <- rlang::sym(ref) %>% as.character()
  
  # Select only the columns with tmle results: col2=grouping variable, col2= data
  contrast_df <-  results_df%>% dplyr::ungroup() %>% dplyr::select(2:ncol(.))
  # columns other than the reference
  comp_df <- contrast_df %>% dplyr::select(-all_of(ref_name))
  # reference
  
  name_list <- list() # to hold names of comparison columns
  
  for (i in 1:ncol(comp_df)) {
    c <- rlang::sym((names(comp_df)[i]))
    name_list[[i]] <- c   # collect names of comaprison columns
    
  }
  
  comp_list <- list() # to hold the names of comparison estimands
  
  for (i in 1:ncol(comp_df)) {
    name <- name_list[[i]]
    c <- glue::glue({{ref_name}},"_vs_",{{name}})
    comp_list[[i]] <- c
  }
  
  
  contrast_results <- data.frame() # to append extracted cleaned results
  
  for (i in 1:ncol(comp_df)) {
    comp_name <- comp_list[[i]]
    comp_var <- name_list[[i]]
    
    
    for (j in 1:m) {
      contrast<- lmtp::lmtp_contrast( contrast_df[comp_var][[1]][[j]],
                                      ref=contrast_df[ref_name][[1]][[j]] ,
                                      type = type)
      
      cont_df <- contrast$vals %>%
        
        dplyr::mutate(estimand= {{comp_name}},
                      reference= {{ref_name}},
                      imp_set= j)
      
      contrast_results <- rbind(contrast_results,cont_df)
      
      
    }
  }
  combined_results <- contrast_results %>%
    dplyr::group_by(estimand) %>%
    dplyr::mutate(variance= std.error^2) %>%
    dplyr::summarise(theta.combined = mean(theta),
                     shift= mean(shift), # to be used in plots
                     ref= mean(ref),
                     p.combined = round(mean(p.value),4) ,
                     Vw = sum(variance)/m, # Within imputation variance
                     Vb = sum((theta - mean(theta))^2/(m-1)), # Between imputation variance
                     Vt = Vw + Vb + Vb/m, # Total variance
                     SE.combined = sqrt(Vt),
                     vm = (m-1)*(1 + (Vw/((1+1/m)*Vb)))^2, #df correction
                     conf.low = theta.combined - qt(0.975, vm)*SE.combined,
                     conf.high = theta.combined + qt(0.975, vm)*SE.combined) %>%
    dplyr::select(estimand, theta= theta.combined,conf.low,
                  conf.high, p.value= p.combined, ref, shift) %>%
    dplyr::mutate_if(is.numeric, ~round(.,digits = 3)) %>%
    as.data.frame()
  
  return(combined_results)
  
}

round_uc <- function(data){
  
  data %>% 
  mutate_at(vars(theta,conf.low, conf.high), ~format(round(.,2),nsmall= 2)) %>%
  mutate(`P value`= format(round(p.value,3),nsmall= 3),
         est_ci = glue::glue("{theta} [{conf.low}-{conf.high}]")) %>% 
  
  dplyr::select(contrast, 
                `OR [95% CI]`= est_ci, 
                `P value`)
}





# reorder factors
# impute
# create numeric df






