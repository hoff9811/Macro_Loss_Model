# Structure the data for the Recalibration

source("08_SingleFactorAnalysis.R")
source("09_ModelTestingFunctions.R")

## Track the runtime of this function by first recording the start time.
starttime <- Sys.time()

#-----------------------------------------------------


cor_limit = 0.7 

## Specify every single combination
var_names   <- colnames(select(indvar, -date)) # exclude date from colnames
var_combos2 <- data.frame(t(combn(var_names, 2)))
var_combos3 <- data.frame(t(combn(var_names, 3)))
var_combos4 <- data.frame(t(combn(var_names, 4)))
var_combos5 <- data.frame(t(combn(var_names, 5)))


# Test correlations for all combinations of 2, 3, 4, and 5 variables
corr_accept2 <- TestCorrelation(var_combos2, indvar, cor_limit) 
corr_accept3 <- TestCorrelation(var_combos3, indvar, cor_limit) 
corr_accept4 <- TestCorrelation(var_combos4, indvar, cor_limit) 
corr_accept5 <- TestCorrelation(var_combos5, indvar, cor_limit) 

# Create the final set of candidate models 
# Based on BXS feedback, we are not looking at 4 and 5 factor models
model_list <- 
  corr_accept2 %>% 
  union_all(corr_accept3) %>% 
  # union_all(corr_accept4) %>% 
  # union_all(corr_accept5) %>% 
  mutate(X4 = NA, X5 = NA) %>% # Hardcode Var4 and Var5 == NA 
  filter(accept_results == "accept") %>% 
  select(-accept_results) %>% 
  mutate(modelId = row_number()) %>% 
  relocate(modelId, .before = everything())

rm(list = c("var_names", "var_combos2", "var_combos3", "var_combos5",
            "corr_accept2", "corr_accept3", "corr_accept4", "corr_accept5"))
#-------------------------------------
# Gather the Multifactor Data
#-------------------------------------

CreatePortfolioModels <- function(indvar, depvar_final, i_portfolio){
  multifactor_data = NULL
  for(i in c(1:nrow(model_list))) {  
      # Get the Variables for i_portfolio 
      i_model          <- slice(model_list, i)
      i_variables      <- na.omit(unlist(select(i_model, -modelId), use.names = FALSE)) # Remove NAs from the variable list
      i_indvar         <- select(indvar, c(date, all_of(i_variables)))
      i_depvar         <- select(depvar_final, date, depvar = starts_with(i_portfolio))
      i_indvar_SA      <- select(macrodata_SA, c(date, all_of(i_variables)))
      i_indvar_baseline <- select(macrodata_baseline, c(date, all_of(i_variables)))
      
      # Aggregate the Data into a single data frame
      i_modeldata <- 
        i_indvar %>% 
        left_join(i_depvar, by = "date") %>% 
        mutate(i_model) %>% 
        nest(data = -c("modelId", "X1", "X2", "X3", "X4", "X5")) 
      
      i_indvar_SA <- 
        i_indvar_SA %>% 
        mutate(modelId = i) %>% 
        nest(indvar_SA = -modelId)
      
      i_indvar_baseline <- 
        i_indvar_baseline %>% 
        mutate(modelId = i) %>% 
        nest(indvar_baseline = -modelId)
      
      # Attach the CCAR scenarios as separate nested tibbles
      i_modeldata <- 
        i_modeldata %>% 
        left_join(i_indvar_SA, by = "modelId") %>% 
        left_join(i_indvar_baseline, by = "modelId")
      
      
      # Export all Data out of the For Loop
      multifactor_data <- 
        multifactor_data %>% 
        union_all(i_modeldata)
      
      rm(list = c("i_depvar", "i_variables", "i_modeldata", "i_indvar", "i_model"))
  }  
  # Apply Models
  multifactor_data  <- 
    multifactor_data  %>% 
    mutate(
      model         = map(data, macro_model),
      summary_stats = map(model, broom::glance),
      model_coefs2  = map(model, ~broom::tidy(lmtest::coeftest(.x ,vcov = NeweyWest))),
      model_coefs   = map(model, broom::tidy),
      model_fitted  = map(model, broom::augment)
    ) %>% 

    # Apply Filtering Tests
    mutate(
      direction_test   = map(model_coefs, TestCoefDirectionality),
      sign_match_value = map(direction_test, ~summarize(.x, sign_match_value = hablar::sum_(sign_match_value))),
      max_p_value      = map(model_coefs, TestCoefSignificance)
    ) %>%  
    
    unnest(c(sign_match_value, max_p_value)) %>% 
    filter(sign_match_value == 0 , max_p.value < 0.1) %>%

    # Test for Stationary, Calculate Weights, and forecast CCAR
    mutate(stationarity_test = map(model_fitted, TestStationarity),
           model_coefs       = map2(model_coefs, model_fitted, CalcCoefWeights),
           model_fitted      = map2(model_fitted, data, ~left_join(select(.y, c("depvar", "date")), .x,  by = "depvar")),
           forecast_baseline = map2(model, indvar_baseline, ForecastCCAR),
           forecast_SA       = map2(model, indvar_SA, ForecastCCAR)) %>% 
    
    unnest(summary_stats) %>% 
    arrange(desc(adj.r.squared)) 
  
  return(multifactor_data)
}

multifactor_CREIP        <- CreatePortfolioModels(indvar, depvar_final, "CREIP")
multifactor_CnIFarmOther <- CreatePortfolioModels(indvar, depvar_final, "CnIFarmOther")
multifactor_Mortgage     <- CreatePortfolioModels(indvar, depvar_final, "Mortgage")
multifactor_Construction <- CreatePortfolioModels(indvar, depvar_final, "Construction")
multifactor_OtherConsumer<- CreatePortfolioModels(indvar, depvar_final, "OtherConsumer")

endtime = Sys.time() - starttime
print(endtime)

