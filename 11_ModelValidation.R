# Final Model Estimation & Validation
source("10_ModelSelection.R")
set.seed(10392)

# List of Final Models
model_CREIP        <- filter(multifactor_CREIP, modelId == 355)
model_CnIFarmOther <- filter(multifactor_CnIFarmOther, modelId == 169)
model_Mortgage     <- filter(multifactor_Mortgage, modelId == 535)
model_Construction <- filter(multifactor_Construction, modelId == 558)
model_OtherConsumer<- filter(multifactor_OtherConsumer, modelId == 524)


final_models <- 
  model_CREIP %>% 
  union_all(model_Construction) %>% 
  union_all(model_Mortgage) %>% 
  union_all(model_OtherConsumer) %>% 
  union_all(model_CnIFarmOther) %>% 
  mutate(portfolio = portfolio_colors$portfolio) %>% 
  mutate(vif = map(model, ~tibble(vif = car::vif(.x)))) %>% 
  mutate(dwtest = map(model, ~tidy(car::durbinWatsonTest(.x))))

stationarity <-
  final_models %>% 
    select(portfolio, stationarity_test) %>% 
    unnest(stationarity_test)


# K-fold Analysis
#---------------------

for(i_portfolio in portfolio_colors$portfolio) {
  
  kfold_result <-
    get(paste0("model_", i_portfolio)) %>% 
    select(data) %>% 
    unnest(data) %>% 
    select(-date) %>% 
    na.omit() %>% 
    modelr::crossv_kfold(5) %>% 
    mutate(model = map(train, ~lm(depvar ~ ., data = .x))) %>% 
    mutate(
      summary_stats = map(model, broom::glance),
      model_coefs   = map(model, broom::tidy),
      model_fitted  = map(model, broom::augment)
    ) %>% 
    mutate(pred = map2(model, test, ~tibble(pred = predict(.x, .y)))) %>% 
    mutate(test = map2(test, pred, ~tibble(data.frame(.x), .y))) %>% 
    mutate(
      direction_test   = map(model_coefs, TestCoefDirectionality),
      sign_match_value = map(direction_test, ~summarize(.x, sign_match_value = hablar::sum_(sign_match_value))),
      max_p_value      = map(model_coefs, TestCoefSignificance),
      adj.r.squared    = map(summary_stats, ~select(.x, adj.r.squared)),
      test_rmse        = map2_dbl(model, test, modelr::rmse)
    ) %>% 
    unnest(c(sign_match_value, max_p_value, adj.r.squared)) 
  
  kfold_output <- 
    kfold_result %>% 
    select(.id, sign_match_value, max_p.value, adj.r.squared)
  
  kfold_pred <-
    kfold_result %>% 
    select(test) %>% 
    unnest(test) %>% 
    select(depvar, pred)
  
  model_fitted <-
    get(paste0("model_", i_portfolio)) %>% 
    select(model_fitted) %>% 
    unnest(model_fitted) %>% 
    left_join(kfold_pred, by = "depvar") %>% 
    mutate(Kfold_fitted = pred,
           PortfolioModel = .fitted)
  
  i_color = pull(subset(portfolio_colors, portfolio == i_portfolio, color))
  i_fill  = pull(subset(portfolio_colors, portfolio == i_portfolio, fill))
  
  print(
    model_fitted %>% 
      pivot_longer(cols = c(PortfolioModel, Kfold_fitted), names_to = "Series") %>% 
      ggplot(aes(x = as.Date(date), y = depvar)) +
      geom_area(position = "identity", fill = i_fill, color = i_color) +
      geom_line(aes(y = value, color = Series)) +
      theme_bw() +
      xlab("Quarter") +
      scale_y_continuous(name = "Loss Rate (%)",
                         labels = scales::percent) +
      ggtitle("Kfold Out-of-sample vs. Model Fitted", paste(i_portfolio, "Portfolio"))
  )
} 