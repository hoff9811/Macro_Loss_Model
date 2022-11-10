#---------------------------------
# Cadence Bank CECL
# Peer Group Average
#---------------------------------

source("02_LineItem_PortfolioDataImport.R")

# Calculate Peer Group average
newpeeravg <- 
  newpeer_portfolio %>% 
  group_by(date) %>% 
  summarize(across(ends_with("NCO"), hablar::mean_)) %>% 
  mutate(PeerId = 20, Name = "New Peer Group Average")

# For OtherConsumer **only** we are removing seven peers from the peer group
#       4 East West Bank                                   174694.
#      10 SouthState Bank, National Association            130619 
#      12 Pinnacle Bank                                    121604.
#      13 Prosperity Bank                                   96159.
#      16 Texas Capital Bank                                36926.
#      15 BankUnited, National Association                  33472.
#       6 Western Alliance Bank                              8585.

newpeeravg_consumer <-
  newpeer_portfolio %>% 
  filter(!PeerId %in% c(4, 6, 10, 12, 13, 15, 16)) %>% 
  group_by(date) %>% 
  summarize(across(OtherConsumer_NCO, hablar::mean_)) 

# Append the consumer peer group average
newpeeravg <-
  newpeeravg %>% 
  select(-OtherConsumer_NCO) %>%  # drop the old variable
  left_join(newpeeravg_consumer, by = "date")

# Include Peer Group average with the peer group data
newpeeravg_portfolio <- 
  newpeer_portfolio %>%
  ungroup() %>% 
  union_all(newpeeravg) %>% 
  nest_by(PeerId, Name)

rm(newpeeravg)
rm(newpeeravg_consumer)
