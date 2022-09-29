library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)

df <- read.csv("https://raw.githubusercontent.com/GuilleDiaz7/Automatic-Web-Scraping-of-A-Local-Cinema/main/data/films_van_golem.csv",
                fileEncoding = "UTF-8")
sapply(df, class)
df$date <- as_datetime(df$date)

sessions_per_film <- df %>% 
  rowwise() %>% 
  mutate(
    Sessions = sum(!is.na(c_across(3:6)))
  ) %>% 
  select(c(1, 2, 7)) %>% 
  group_by(films) %>% 
  mutate(count = sum(Sessions)) %>% 
  select(c(2, 4)) %>% 
  distinct()


sessions_per_film <- sessions_per_film %>% 
  mutate(
    version = if_else(str_match_all(films, "(?<=\\().*(?=\\))") == "V.O.S.E.", "V.O.S.E.", "V.E.")
  ) %>% 
  mutate(
    films = gsub("[()]", "", films)
  ) %>% 
  mutate(
    films = gsub("V.O.S.E.", "", films)
  ) %>% 
  arrange(desc(count))
sessions_per_film

