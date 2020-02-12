# notes ----
# Scallop Data Availability
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov

# load ----
library(scallopr)
library(tidyverse)
library(lubridate)
library(FNGr)
theme_set(FNGr::theme_sleek())

# data ---- 

survey_events <- bind_rows(read_csv("./output/2016/tows.csv"),
                           read_csv("./output/2017/tows.csv"),
                           read_csv("./output/2018/tows.csv"),
                           read_csv("./output/2019/tows.csv") %>%
                             mutate_at(1, as.character))
        
# eda ----

# number of survey tows by bed
survey_events %>%
  mutate(year = substring(tow_id, 1, 4),
         year = ifelse(year == "1901", 2019, as.numeric(year))) %>%
  count(year, Bed) %>%
  ggplot()+
  geom_point(aes(x = year, y = Bed, size = n))+
  labs(x = "Year")
