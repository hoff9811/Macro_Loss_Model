#---------------------------
# BXS CECL - Macroeconomic Variables
#---------------------------

# Macroeconomic Data Functions
# 1. Rolling Average
ApplyMovingAverage <- function(x) {
  tibble(
    v1 = lag(x, 2),
    v2 = lag(x, 1),
    v3 = x,
    v4 = lead(x , 1),
    v5 = lead(x , 2),
  ) %>% 
    mutate(mean1 = rowMeans(across(v1:v4), na.rm = TRUE),
           mean2 = rowMeans(across(v2:v5), na.rm = TRUE),
           roll_avg = rowMeans(across(mean1:mean2), na.rm = TRUE)
    ) %>% 
    pull(roll_avg)
}

# 2. Annualized Quarterly Growth Rate
AnnualizeQGrowth <- function(x) {
  100*((x / lag(x, 1))^4 -1)
}

#-----------------------------------------
# Master Function
ProcessFedEconData <- function(df) {
  df %>% 
    rename_with(~ tolower(gsub("..", "_", .x, fixed = TRUE))) %>% 
    rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>% 
    rename_with(~ gsub("level_", "level", .x, fixed = TRUE)) %>% 
    
    # 1: GDP, Income Growth (Real and Nominal) - Apply Moving Average
    mutate(across(real_gdp_growth:nominal_disposable_income_growth, ApplyMovingAverage)) %>% 
    
    # 2: CPI Inflation - Apply Moving Average
    mutate(across(cpi_inflation_rate, ApplyMovingAverage)) %>% 
    
    # 3: Dow Jones, HPI, CREPI - Annualize Q Growth then Apply Moving Average
    mutate(across(dow_jones_total_stock_market_index_level:
                    commercial_real_estate_price_index_level , 
                  AnnualizeQGrowth,
                  .names = "{col}_growth")) %>% 
    rename_with(~ gsub("level_growth", "growth", .x, fixed = TRUE)) %>% 
    mutate(across(dow_jones_total_stock_market_index_growth:
                    commercial_real_estate_price_index_growth, ApplyMovingAverage)) %>% 
    
    #4. Add the Spread Variables
    mutate(x3_month_to_10_year_treasury_spread = x10_year_treasury_yield - x3_month_treasury_rate,
           bbb_to_10_year_corporate_yield_spread = bbb_corporate_yield - x10_year_treasury_yield) %>% 
    
    filter(date >= '1990 Q1')
}


#--------------------------------------
# Data Input from CSV using Fed Historical
fed_historical <- read.csv("2022-table_1a_historic_domestic.csv") %>% 
  tibble() %>% 
  mutate(Date = as.yearqtr(Date))

fed_baseline <- read.csv("2022-table_2a_supervisory_baseline_domestic.csv") %>% 
  tibble() %>% 
  mutate(Date = as.yearqtr(Date))

fed_SA     <- read.csv("2022-table_3a_supervisory_severely_adverse_domestic.csv") %>% 
  tibble() %>% 
  mutate(Date = as.yearqtr(Date))

# Data Processing
macrodata_historical  <- ProcessFedEconData(fed_historical) 
macrodata_baseline    <- ProcessFedEconData(fed_baseline)
macrodata_SA          <- ProcessFedEconData(fed_SA) 

# Final IndVar
# Finalize the IndVar Object
indvar <- 
  macrodata_historical %>% 
  select(-scenario_name) %>%  
  select(-dow_jones_total_stock_market_index_level,
         -house_price_index_level,
         -commercial_real_estate_price_index_level) # Remove DJI, HPI, CREIP level variables

rm(list = c("fed_historical", 
            "fed_baseline", 
            "fed_SA"))
