#---------------------------------
# Cadence Bank CECL
# General Functions used to calculate NCOs
#---------------------------------

# Calculate Line Item NCOs
CalculateLineItemNCOs <- function(df) {
  i_mapping <- portfolio_mapping %>% 
    pivot_longer(cols = everything(), names_to = "portfolio") %>% 
    arrange(portfolio) %>% 
    na.omit() %>% 
    pull(value)
  
  for(j in i_mapping) {
    df <-
      df %>%
      # Replace 0 NLLs with NA as an error correcting measure
      mutate(!!paste0(j, "_NLL2") := 
               ifelse(get(paste0(j, "_NLL")) == 0 , NA, get(paste0(j, "_NLL")))) %>% 
      mutate(!!paste0(j, "_NCO") :=
               (get(paste0(j, "_CO")) - get(paste0(j, "_REC"))) / get(paste0(j, "_NLL2"))
      ) %>% 
      select(-paste0(j, "_NLL2"))
  }
  
  return(df)
}


#--------------------------------------
# We create a function for this since its called repeatedly
CalculatePortfolioNCOs <- function(df) {
  df <- 
    df %>% 
    mutate(
           CREIP_NCO = (CREIP_CO - CREIP_REC) / CREIP_NLL,
           Construction_NCO = (Construction_CO - Construction_REC) / Construction_NLL,
           Mortgage_NCO = (Mortgage_CO - Mortgage_REC) / Mortgage_NLL,
           OtherConsumer_NCO = (OtherConsumer_CO - OtherConsumer_REC) / OtherConsumer_NLL,
           CnIFarmOther_NCO = (CnIFarmOther_CO - CnIFarmOther_REC) / CnIFarmOther_NLL
    )
  return(df)
}

#--------------------------------------------------------                                         ##
#   General Linear Regression Function
#--------------------------------------------------------
macro_model <- function(df) {
  df <- 
    df %>% 
    select(-date) %>% 
    na.omit()
  
  lm(depvar ~ ., data = df)
}