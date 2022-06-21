library(dplyr)
library(tidytext)
library(ggplot2)
library(readr)
library(readxl)

may_non_profit_name_list_for_open_data_portal <- read_excel("may-non_profit_name_list_for_open_data_portal.xlsx")
View(may_non_profit_name_list_for_open_data_portal)


X2338_Calgary_Charities_CRA2020 <- read_csv("2338_Calgary_Charities_CRA2020.csv")
View(X2338_Calgary_Charities_CRA2020)


abdata <- data.frame(may_non_profit_name_list_for_open_data_portal) %>%
  filter(City == "CALGARY") %>%
  filter(Status == "Active") %>%
  select("Legal.Entity.Name") %>%
  rename(Name = "Legal.Entity.Name")
cradata <- data.frame(X2338_Calgary_Charities_CRA2020) %>%
  mutate(Name = toupper(Legal.name)) %>%
  select(Name)

allcalgarynp <- full_join(cradata,abdata)
npnames <- distinct(allcalgarynp) %>%
  unnest_tokens(word, Name, drop = F) %>%
  anti_join(stop_words)

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 50) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 30) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 20) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 15) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 10) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

npnames %>%
  count(word, sort = TRUE) %>%
  filter(n <= 5) %>%
  filter(n > 4) %>%  
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()