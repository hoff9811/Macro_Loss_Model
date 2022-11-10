
# Correlation Testing Function
TestCorrelation <- function(var_combos, indvar, cor_limit) {
  accept_results <- NULL
  # Test correlation for all combinations 
  for( i in c(1:nrow(var_combos))) {
    # df correlation matrix
    i_vars <- as.character(var_combos[i, ])
    i_corr <- indvar %>% 
      select(all_of(i_vars)) %>% 
      corrr::correlate(quiet = TRUE) 
    # Calculate Max Corr
    i_max_corr <- 
      i_corr %>% 
      select(-term) %>% 
      max(na.rm = TRUE)
    # test whether max corr exceeds limit
    i_accept       <- ifelse(i_max_corr < cor_limit, "accept", "reject")
    accept_results <- c(accept_results, i_accept)
  }
  
  output <- tibble(var_combos, accept_results)
  return(output)
  
}

# Test Coefficient Directionality
TestCoefDirectionality <- function(coef_df) {
  coef_df <- 
  coef_df %>% 
    left_join(variable_sign, by = "term") %>% 
    mutate(sign_match = ifelse(estimate > 0 & direction == "positive", 
                               "match",
                               ifelse(estimate < 0 & direction == "negative", 
                                      "match",
                                      ifelse(direction == "either", 
                                             "match", 
                                             "no match")
                                      )
                               )
           ) %>% 
    mutate(sign_match_value = ifelse(sign_match == "match",0 ,1)) %>% 
    select(term, estimate, direction, sign_match, sign_match_value)
  
  return(coef_df)
}

TestCoefSignificance <- function(coef_df) {
  coef_df <-
    coef_df %>% 
    filter(term != "(Intercept)") %>%  # We don't care about the intercept
    summarize(max_p.value = hablar::max_(p.value))
  
}

# Calculate Coefficient Weights
CalcCoefWeights <- function(coef_df, fitted_df) {
  coef_sd <- 
    fitted_df %>% 
    summarize(across(everything(), hablar::sd_)) %>% 
    pivot_longer(cols = everything(), names_to = "term", values_to = "sd")
  
  df_output <- 
    coef_df %>% 
    left_join(coef_sd, by = "term") %>% 
    mutate(weight = abs(estimate * sd) / hablar::sum_(abs(estimate * sd))) %>% 
    select(-sd)
    
  return(df_output)
}

# Forecast CCAR Scenarios
ForecastCCAR <- function(model, indvar_scenario) {
  df_output <- 
    indvar_scenario %>% 
    mutate(scenario = predict(model, newdata = indvar_scenario))
  return(df_output)
}

# Test Residuals for Stationarity
TestStationarity <- function(df_model_fitted) {
  pp   <- broom::tidy(tseries::pp.test(df_model_fitted$.resid)) 
  adf  <- broom::tidy(tseries::adf.test(df_model_fitted$.resid)) 
  kpss <- broom::tidy(tseries::kpss.test(df_model_fitted$.resid))
    
  df_output <- 
    adf %>% 
    union_all(pp) %>% 
    union_all(kpss)
  
  return(df_output)
}