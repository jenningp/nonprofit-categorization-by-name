library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(readr)
library(readxl)

may_non_profit_name_list_for_open_data_portal <- read_excel("may-non_profit_name_list_for_open_data_portal.xlsx")
View(may_non_profit_name_list_for_open_data_portal)


X2338_Calgary_Charities_CRA2020 <- read_csv("2338_Calgary_Charities_CRA2020.csv")
View(X2338_Calgary_Charities_CRA2020)


dfabdata <- data.frame(distinct(may_non_profit_name_list_for_open_data_portal)) %>%
  filter(City == "CALGARY") %>%
  rename(Name = "Legal.Entity.Name") %>%
  mutate(source = "Alberta Open Data nonprofit Listing")
dfcradata <- data.frame(distinct(X2338_Calgary_Charities_CRA2020)) %>%
  mutate(Name = toupper(Legal.name)) %>%
  select(-City, -Account.name)
  mutate(source = "CRA charity")

allnprecords <- full_join(dfabdata, dfcradata, by="Name")

# to get a table of all cases, the status coming from the larger Alberta list, it was noted that (sunflower organization)
allnprecords %>% 
  rename(registered = Count.of.Account.name) %>%
  mutate(intersect_flag = replace_na(registered,0)) %>%
  mutate(registered = ifelse(intersect_flag == 0, "AB", "CRA")) %>%
  count(Status, registered, sort= T) %>%
   bind_rows(summarize(., across(where(is.numeric), sum), across(where(is.character), ~"Total")))


allnprecords %>% 
  rename(registered = Count.of.Account.name) %>%
  mutate(intersect_flag = replace_na(registered,0)) %>%
  mutate(registered = ifelse(intersect_flag == 0, "AB", "CRA")) %>%
  count(Status, registered, sort= T) %>%
  mutate(cases = replace_na(Status, "not found in AB list")) %>%
  filter(cases == "Active"| cases == "not found in AB list") %>%
  rename(count = n) %>%
  select(-Status) %>%
  bind_rows(summarize(., across(where(is.numeric), sum), across(where(is.character), ~"Total")))

nplist <- allnprecords %>% 
  rename(registered = Count.of.Account.name) %>%
  mutate(intersect_flag = replace_na(registered,0)) %>%
  filter(Status == "Active")

nplist %>%
  mutate(Organization_type = replace_na(Description_E, "Not in CRA thereby not defined")) %>%
  group_by(Legal.Entity.Type.Description, Organization_type) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = Legal.Entity.Type.Description, values_from= n)

nplist %>%
  mutate(Organization_type = replace_na(Description_E, "Not in CRA thereby not defined")) %>%
  group_by(Legal.Entity.Type.Description) %>%
  count(Organization_type) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x=Legal.Entity.Type.Description, y=proportion, fill = Organization_type)) +
  geom_bar(stat = "identity") +
  ggtitle("Are certain Alberta Legal Entity Descriptions have\n organizations that do not need to report to the CRA?") +
  coord_flip()






CRAnotAB <- allnprecords %>% 
  rename(registered = Count.of.Account.name) %>%
  mutate(test = replace_na(registered,0)) %>%
  mutate(registered = ifelse(test == 0, "AB", "CRA")) %>%
  mutate(cases = replace_na(Status, "not found in AB list")) %>%
  filter(cases == "not found in AB list")
          

allnprecords %>% count(Status, Category.English.Desc, sort = T) 