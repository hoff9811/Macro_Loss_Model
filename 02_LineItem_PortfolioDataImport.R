#--------------------------------
# BXS CECL: Import CSV Files 
# 
# The CVS files stem from the SNL Data Pull
# There are two types for two sets of peers:
# 1) Line Item Inputs and 2) Portfolio inputs
# Next we have: 1) New Peer Group, 2) BXS and Old Peer Group

source("01_ImportPackages_MappingTables.R")
source("03_CalculateNCOFunctions.R")


#-------------------------------
# Function to Aggregate the Line Item tibble into a portfolio-level tibble
ConvertToPortfolio <- function(df_lineitem, portfolio_mapping) {
  df_portfolio <- 
    df_lineitem %>% 
    select(c("date", "PeerId", "Name"))
 
  # Use a for loop to aggregate line item data into each portfolio
  # The aggregation is done using the portfolio_mapping table 
  
  for(i_portfolio in colnames(portfolio_mapping)) {
    i_mapping <- pull(portfolio_mapping, i_portfolio)
    
    if (i_portfolio != "OtherConsumer") {
      portfolio_data <-
        df_lineitem %>% 
        select(c("date", "PeerId", "Name"), starts_with(i_mapping)) %>%
        mutate(!!paste0(i_portfolio, "_CO")  := apply(across(ends_with("CO")), 1 , hablar::sum_), # Hack way of applying rowSums_
               !!paste0(i_portfolio, "_REC") := apply(across(ends_with("REC")), 1 , hablar::sum_),
               !!paste0(i_portfolio, "_NLL") := apply(across(ends_with("NLL")), 1 , hablar::sum_)) 
    } else {
      portfolio_data <-
        df_lineitem %>% 
        select(c("date", "PeerId", "Name"), starts_with(i_mapping)) %>%
        mutate(!!paste0(i_portfolio, "_CO")  := Consumer_CO - CreditCard_CO,
               !!paste0(i_portfolio, "_REC") := Consumer_REC - CreditCard_REC,
               !!paste0(i_portfolio, "_NLL") := Consumer_NLL - CreditCard_NLL) 
    }
    
    # Remove all lineitem series. Only the portfolio series remain 
    portfolio_data <-
      portfolio_data %>% 
      select(c("date", "PeerId", "Name"),
             paste0(i_portfolio, "_CO"), 
             paste0(i_portfolio, "_REC"),
             paste0(i_portfolio, "_NLL"))
    
    df_portfolio <- 
      df_portfolio %>% 
      left_join(portfolio_data, by = c("date", "PeerId", "Name"))
  }
  
  return(df_portfolio)
}

# Line Item Input Files
bxsoldpeer_lineitem <- read.csv("BXSandOldPeer_LineItemInput.csv")
newpeer_lineitem    <- read.csv("NewPeerGroup_LineItemInput.csv")

# Convert Quarter to an R-format Date
bxsoldpeer_lineitem <- 
  tibble(bxsoldpeer_lineitem) %>% 
  mutate(date = as.yearqtr(Quarter))

newpeer_lineitem <- 
  tibble(newpeer_lineitem) %>% 
  mutate(date = as.yearqtr(Quarter))


#----------------------------------------
# Aggregate Cadence Bank Line Item Data
#----------------------------------------

cadencecharters_lineitem <- 
  bxsoldpeer_lineitem %>% 
  filter(PeerId < 0) %>% 
  
  # 1. Combining Other and Political NLLs 
  mutate(OtherLoans_NLL = OtherLoans_NLL + PoliticalLoans_NLL) %>% 
  select(-PoliticalLoans_NLL) %>% 
  
  # 2. Defining Other CRE as NFNR post2007 and CRE Total pre2007
  mutate(CRENonFarmNonResi_CO = ifelse(date < "2007 Q1", CRE_Total_CO, CRENFNR_CO),
         CRENonFarmNonResi_REC = ifelse(date < "2007 Q1", CRE_Total_REC, CRENFNR_REC),
         CRENonFarmNonResi_NLL = ifelse(date < "2007 Q1", CRE_Total_NLL, CRENFNR_NLL)) %>% 
  
  # 3. At the Bank's request, we are filtering out Cadence Bank prior to 2013Q1
  mutate(filter_x = ifelse(PeerId > -4 & date < "2013 Q1", 1,0)) %>% 
  mutate(legacyBank = ifelse(PeerId > -4, "legacyCadence", "legacyBXS")) %>% 
  subset(filter_x == 0, select = -filter_x) 

#-----------------------------------------------------------------
# Use the 'ConvertToPortfolio' function to aggregate the line items
cadencecharters_portfolio <-
  ConvertToPortfolio(cadencecharters_lineitem, portfolio_mapping) %>% 
  CalculatePortfolioNCOs() 

#--------------------------------------------
# Aggregate Cadence into a single Institution
#--------------------------------------------
combined_cadence_lineitem <- 
  cadencecharters_lineitem %>% 
  # 4. Sum across all Cadence Bank charters
  group_by(date) %>%
  summarize_at(vars(CnI_CO:CRENonFarmNonResi_NLL), hablar::sum_) %>%  # Hablar::sum returns NA if all sum(x..x) = NA
  mutate(PeerId = 0, Name = "Combined Cadence Bank") 

combined_cadence_portfolio <-
  ConvertToPortfolio(combined_cadence_lineitem, portfolio_mapping) %>% 
  CalculatePortfolioNCOs()

#--------------------------------------
#  Peer Group Line Item Data
#--------------------------------------
newpeer_lineitem <- 
  newpeer_lineitem %>%
  
  # 1. Combining Other and Political NLLs 
  mutate(OtherLoans_NLL = OtherLoans_NLL + PoliticalLoans_NLL) %>% 
  select(-PoliticalLoans_NLL) %>% 
  group_by(date) %>% 
  
  # 2. Defining Other CRE as NFNR post2007 and CRE Total pre2007
  mutate(CRENonFarmNonResi_CO = ifelse(date < "2007 Q1", CRE_Total_CO, CRENFNR_CO),
         CRENonFarmNonResi_REC = ifelse(date < "2007 Q1", CRE_Total_REC, CRENFNR_REC),
         CRENonFarmNonResi_NLL = ifelse(date < "2007 Q1", CRE_Total_NLL, CRENFNR_NLL))

#-----------------------------------------------------------------
# Use the 'ConvertToPortfolio' function to aggregate the line items
newpeer_portfolio <-
  ConvertToPortfolio(newpeer_lineitem, portfolio_mapping) %>% 
  CalculatePortfolioNCOs()
  





