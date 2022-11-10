#---------------------------
# BXS CECL - Final Dependent Variable Construction
#---------------------------
source("04_PeerGroupAverage.R")
source("05_EconomicData.R")

CorrelateNCOs <- function(df) {
  df %>% 
    select(-date) %>% 
    correlate(quiet = TRUE) %>% 
    focus(ends_with("NCO"))
}

CalcCorrSum <- function(df) {
  df %>% 
    summarize(across(ends_with("NCO"),
                     .fns = ~sum(abs(.x))))
}

#--------------------------------------
# Final Dependent Variable Construction

depvar_calc <- 
  newpeeravg_portfolio %>%
  filter(PeerId == 20) %>%
  unnest(data) %>% 
  union_all(combined_cadence_portfolio) %>% 
  select(date, ends_with("NCO")) 

# Create a potential dependent variable using all weights from 10-50%
depvar_weightedavg <- NULL
for(i_weight in c(2:20)/20) {
  i_depvar <- 
    depvar_calc %>% 
    mutate(weight = ifelse(PeerId == 0, i_weight, 1 - i_weight)) %>% 
    ungroup() %>% 
    select(-PeerId, -Name) %>% 
    group_by(date) %>% 
    summarize_all(~hablar::sum_(.x * weight)) %>% # using the hablar::sum_ to return NA if all values are NA. Normal sum() returned zero if all values are NA
    mutate(across(ends_with("NCO"), ApplyMovingAverage)) %>% # Applying the moving average
    mutate(weight = i_weight)
  
  depvar_weightedavg <- 
    depvar_weightedavg %>% 
    union_all(i_depvar)
  
  rm(i_depvar)
}

# Correlate with all economic variables
depvar_corrtest <- 
  depvar_weightedavg %>% 
  left_join(indvar, by = "date") %>% 
  nest_by(weight) %>%
  ungroup() %>% 
  mutate(corr_matrix = map(.x = data,
                           .f = CorrelateNCOs)) %>% 
  mutate(corr_sum = map(.x = corr_matrix, 
                        .f = CalcCorrSum)) %>% 
  unnest(corr_sum) %>% 
  mutate(sumTotal = rowSums(across(ends_with("NCO"))))

# We are proceeding with the 10% DepVar weight
depvar_final <- 
  depvar_weightedavg %>% 
  filter(weight == 0.1) %>% # 10% weight
  select(-weight)

