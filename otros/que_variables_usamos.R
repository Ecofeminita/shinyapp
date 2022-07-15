library(tidyverse)
library(fs)
library(eph)

scripts <- fs::dir_ls('R')

scripts_text <- map(scripts,read_file)

single_text <- paste0(scripts_text,collapse = '\n\n')

single_text

variables <- c(names(eph::toybase_hogar_2016_04),names(eph::toybase_individual_2016_04))

tibble(variables) %>% 
  mutate(presente= str_detect(single_text,variable)) %>% 
  filter(presente) %>% 
  pull(variables)



