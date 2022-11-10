#---------------------------
# BXS CECL - single Factor
#---------------------------

source("06_FinalDepVar.R")

# Remove everything but our one depvar and indvar files
rm(list=setdiff(ls(),
                c("depvar_final",
                "indvar",
                "macrodata_SA",
                "macrodata_baseline")))

source("01_ImportPackages_MappingTables.R")
source("03_CalculateNCOFunctions.R")


singlefactor_results <- 
  depvar_final %>% 
  pivot_longer(cols = ends_with("NCO"), values_to = "depvar", names_to = "portfolio") %>% 
  left_join(indvar, by = "date") %>% 
  pivot_longer(cols = -c("depvar", "date", "portfolio"), names_to = "indvar") %>% 
  nest_by(portfolio, indvar) %>% 
  separate(col = portfolio, sep = "_", into = c("portfolio", "series")) %>%

  # Regression
  mutate(
    model         = map(data, macro_model),
    summary_stats = map(model, broom::glance),
    model_coefs   = map(model, broom::tidy),
    model_fitted  = map(model, broom::augment),
    summary_HAC   = map(model, ~broom::tidy(lmtest::coeftest(.x ,vcov = NeweyWest)))
  ) 

#---------------------
# Output Matrices
singlefactor_p_values <-   
  singlefactor_results %>% 
  select(-data, -series, -model, -summary_stats, -model_coefs, -model_fitted) %>% 
  unnest(summary_HAC) %>%
  filter(term == "value") %>% 
  select(-term, -std.error, -statistic, -estimate) %>% 
  pivot_wider(names_from = portfolio, values_from = p.value)

singlefactor_coefs <-   
  singlefactor_results %>% 
  select(-data, -series, -model, -summary_stats, -model_coefs, -model_fitted) %>% 
  unnest(summary_HAC) %>%
  filter(term == "value") %>% 
  select(-term, -std.error, -statistic, -p.value) %>% 
  pivot_wider(names_from = portfolio, values_from = estimate) %>% 
  mutate(indvar = gsub("_", " ", indvar, fixed = TRUE))


# write.csv(SingleFactor_coefs, "SingleFactor_coefs.csv")
# write.csv(SingleFactor_p_values, "SingleFactor_pvalues.csv")
