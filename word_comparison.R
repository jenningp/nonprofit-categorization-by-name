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


abdata <- data.frame(distinct(may_non_profit_name_list_for_open_data_portal)) %>%
  filter(City == "CALGARY") %>%
  filter(Status == "Active") %>%
  select("Legal.Entity.Name") %>%
  rename(Name = "Legal.Entity.Name") %>%
  unnest_tokens(word, Name, drop = F) %>%
  anti_join(stop_words) %>%
  mutate(source = "Alberta Open Data nonprofit Listing")
cradata <- data.frame(distinct(X2338_Calgary_Charities_CRA2020)) %>%
  mutate(Name = toupper(Legal.name)) %>%
  select(Name) %>%
  unnest_tokens(word, Name, drop = F) %>%
  anti_join(stop_words) %>%
  mutate(source = "CRA charity")

frequency_table <- bind_rows(abdata,cradata) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(sum = sum(n)) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n, -sum) %>%
  spread(source, proportion)

frequency_table_common <- bind_rows(abdata,cradata) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(sum = sum(n)) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n, -sum) %>%
  spread(source, proportion) %>%
  filter(`CRA charity` >= 0 & `CRA charity`<= .001) %>%
  filter(`Alberta Open Data nonprofit Listing` >= 0 & `Alberta Open Data nonprofit Listing` <= .001)


 

frequency_long <- bind_rows(abdata,cradata) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(sum = sum(n)) %>%plot(frequency_table$`CRA charity`,frequency_table$`Alberta Open Data nonprofit Listing`)
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(source, proportion) %>%
  gather(source, proportion, `Alberta Open Data nonprofit Listing`: `CRA charity`)



ggplot(frequency_table, aes(x = `CRA charity`, y = `Alberta Open Data nonprofit Listing`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") 


ggplot(frequency_table_common, aes(x = `CRA charity`, y = `Alberta Open Data nonprofit Listing`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") 


  