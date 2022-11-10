#---------------------------
# BXS CECL - Import Packages
#---------------------------
require(hablar)
require(tidyverse)
require(readxl)
require(lubridate)
require(reshape2)
require(broom)
require(corrr)
require(zoo)
require(officer)
require(rvg)
require(lmtest)
require(sandwich)

# Defining the colors for our plots
cc <- scales::div_gradient_pal(
                                low = "#B9D989",
                                mid = "#4BCD3E",
                                high = "#012834",
                                "Lab"
                              )(seq(0, 1, length.out = 9))

cc2 <- scales::div_gradient_pal(
                                low = "#B9D989",
                                mid = "#B3E9FE",
                                high = "#015B7E",
                                "Lab"
                              )(seq(0, 1, length.out = 9))

#----------------------------------------
# Defining of NCO colors
portfolio_colors <- tibble(
                  portfolio = c(
                    "CREIP",
                    "Construction",
                    "Mortgage",
                    "OtherConsumer",
                    "CnIFarmOther"), 
                  color = c(
                    "#285BC5",
                    "#015B7E",
                    "#009775",
                    "#FF1F3E",
                    "#4C12A1"),
                  fill = c(
                    "#D2DDF6",
                    "#B3E9FE",
                    "#B7FFEF",
                    "#FFD2D8",
                    "#D9C3F8"))

#--------------------------------------
# Portfolio Mapping Table
portfolio_mapping <- tibble(
  CnIFarmOther = c("CnI", 
                   "OwnOcc", 
                   "FarmLand", 
                   "FarmLoans",
                   "LoanstoDepInst",
                   "OtherLoans",
                   "LeaseFinancingRec"),
  CREIP = c("CREMultifamily",
            "CRENonFarmNonResi", 
            rep(NA, 5)),
  Construction = c("Construction", 
                   rep(NA, 6)),
  Mortgage = c("Mortgage1stLien",
               "MortgageHELOC",
               "MortgageJuniorLien",
               rep(NA,4)),
  OtherConsumer = c("Consumer",
               "CreditCard",
               rep(NA,5))
)

modelvariables2020 <- 
  tibble(
    CnIFarmOther = c("real_gdp_growth", 
                     "unemployment_rate", 
                     "bbb_corporate_yield"
    ),
    CREIP = c("unemployment_rate", 
              "commercial_real_estate_price_index_growth",
              "bbb_to_10_year_corporate_yield_spread"),
    Construction = c("unemployment_rate", 
                     "house_price_index_growth", 
                     "bbb_to_10_year_corporate_yield_spread"),
    Mortgage = c("unemployment_rate", 
                 "mortgage_rate", 
                 "house_price_index_growth"),
    OtherConsumer = c("nominal_disposable_income_growth", 
                      "unemployment_rate", 
                      "mortgage_rate")
  ) 

#-------------------------------------
# Direction Hypothesis
variable_sign <- read.csv("Variable_Direction.csv")