library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(readr)
library(readxl)
library(tm)
library(igraph)
library(ggraph)
library(widyr)


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
head(frequency_table[order(frequency_table$`CRA charity`, decreasing=T),])

frequency_table_common <- bind_rows(abdata,cradata) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(sum = sum(n)) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n, -sum) %>%
  spread(source, proportion) %>%
  filter(`CRA charity` >= 0 & `CRA charity`<= .001) %>%
  filter(`Alberta Open Data nonprofit Listing` >= 0 & `Alberta Open Data nonprofit Listing` <= .001)
head(frequency_table_common[order(frequency_table_common$`CRA charity`, decreasing=T),])

frequency_table_common_list <- distinct(bind_rows(abdata,cradata)) %>%
  count(source, word) %>%
  group_by(word) %>%
  spread(source, n) %>%
  filter(`CRA charity` == 1 & `Alberta Open Data nonprofit Listing` == 1)



ggplot(frequency_table, aes(x = `CRA charity`, y = `Alberta Open Data nonprofit Listing`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") 




frequency_table_stemmed <- bind_rows(abdata,cradata) %>%
  mutate(stemmed = stemDocument(word)) %>%
  count(source, stemmed) %>%
  group_by(source) %>%
  mutate(sum = sum(n)) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n, -sum) %>%
  spread(source, proportion)

frequency_table_stemmed_zoom <- frequency_table_stemmed %>%
  filter(`CRA charity` < .001) %>%
  filter(`Alberta Open Data nonprofit Listing` < .001)


ggplot(frequency_table_stemmed, aes(x = `CRA charity`, y = `Alberta Open Data nonprofit Listing`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = stemmed), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") 

ggplot(frequency_table_stemmed_zoom, aes(x = `CRA charity`, y = `Alberta Open Data nonprofit Listing`)) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = stemmed), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") 

title_word_pairs <- bind_rows(abdata,cradata) %>%
  mutate(stemmed = stemDocument(word)) %>%
  select(word, stemmed) %>%
  mutate(merged = paste(word,stemmed)) %>%
  count(merged) %>%
  separate(merged, sep = " ", into = c("word", "stemmed"))
head(title_word_pairs[order(title_word_pairs$n, decreasing=T),], 20)

write.csv(title_word_pairs, "word_pairs.csv")


set.seed(1234)
title_word_pairs %>%
  filter(n >= 200) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point( size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

title_word_pairs %>%
  bind_rows(abdata,cradata) %>%
  mutate(stemmed = stemDocument(word)) %>%
  select(word, stemmed) %>%
  count(stemmed) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = n)) +
  geom_freqpoly(binwidth = 1)

title_word_pairs %>%
  bind_rows(abdata,cradata) %>%
  mutate(stemmed = stemDocument(word)) %>%
  select(word, stemmed) %>%
  count(stemmed) %>%
  arrange(desc(n)) %>%
  filter(n < 5) %>%
  ggplot(aes(x = n)) +
  geom_freqpoly(binwidth = 1)


  
  
  