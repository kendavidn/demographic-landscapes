library(pacman)

p_load(char = c("tidyverse", 
                "readxl",
                "openxlsx",
                "here"
                ))


#~~~~~~~~~~~~~~~~~~~~~~
#~ Which dil ----
#~~~~~~~~~~~~~~~~~~~~~~

# Survey years original dataset table was copied and pasted from DHS website

df <- 
  read_xlsx(here("data/survey_years.xlsx"))%>% 
  filter(str_detect(type, "DHS"), 
         survey_datasets == "Data Available") %>% 
  mutate(country = str_replace_all(country, "\\(.*\\)", ""), 
         country = str_replace_all(country, "[:digit:]", "" ), 
         country = str_replace_all(country, "[:punct:]", ""),
         country = str_trim(country), 
         country = if_else(country == "Cote dIvoire", "Cote d'Ivoire", country)) %>% 
  mutate(dates =  str_replace_all(dates, ".*-", ""),
          dates =  str_replace_all(dates, ".*/", "")) %>% 
  mutate(recode = if_else(recode == phase, "", paste("*recode", recode) )) %>% 
  select(country, phase, dates, recode) %>% 
  mutate(dates_recode = paste(dates, recode)) %>% 
  pivot_wider(id_cols = country, names_from = phase, values_from = dates_recode ) %>%  
  select(sort(current_vars()))

write.xlsx(df, here("results/survey_years_clean.xlsx"))





