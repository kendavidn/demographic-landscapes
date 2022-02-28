#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Import packages----
#~~~~~~~~~~~~~~~~~~~~~~~~


library(pacman)
p_load(char = c("tidyverse",
                "inspectdf",
                "here",
                "microbenchmark",
                "mdhs",
                "survival",
                "MortalitySmooth",
                "reshape2",
                "viridis",
                "rayshader",
                "plotly", 
                "rdhs",
                "haven",
                "countrycode",
                "janitor",
                "survey",
                "purrr",
                "paletteer",
                "flexdashboard"
))



#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Set theme ----
#~~~~~~~~~~~~~~~~~~~~~~~~
source(here("scripts/ggplot_theme.R" ))

#~~~~~~~~~~~~~~~~~~~~~~
#~  Make target folders for downloaded IR files ----
#~~~~~~~~~~~~~~~~~~~~~~
# 
#countries <- df %>% select(country) %>% pull() %>% as.character()
#sapply(paste0(here(), "/data/ind_recode_files/", countries), dir.create)

#~~~~~~~~~~~~~~~~~~~~~~
#~  Download data ----
#~~~~~~~~~~~~~~~~~~~~~~
# 
## set up your credentials                       
set_rdhs_config(email = "kendavidn@gmail.com" ,
                project = "The effect of birth spacing on maternal and child health outcomes in SSA")

# password is 16Liverpool.

## Use to work out countryIds when downloading many countries
# two_letter_codes <- countrycode(sourcevar = countries, origin = "country.name", destination = "iso2c" )

## select datasets
# datasets <-
#   dhs_datasets(countryIds = c("MW", "BJ"), fileFormat = "flat", 
#                fileType = "IR", surveyType = "DHS", surveyYear = c("2004", "2000", 
#                                                                    "1996", "2001"))

# used in several places!
countrycodes <- c("MW")
country_fullname <- countrycode(sourcevar = countrycodes, origin = "iso2c" , destination = "country.name" )

dhs_datasets_df <-
  dhs_datasets(countryIds = countrycodes, fileFormat = "flat", 
               fileType = "IR", surveyType = "DHS",
               surveyYearStart = "1990" # sibling questions were introduced in phase 2, 1990s
               )

# download datasets (store them in cache)
dhs_downloads_cache <- get_datasets(dhs_datasets_df$FileName)

# columns to keep
sib_vars <- paste0("mm", c(1, 2, 4, 8))
sib_nums <- str_pad(1:20, 2, pad = "0")
sib_cols <- paste(rep(sib_vars, each = length(sib_nums)), sib_nums, sep = "_")
individual_cols <- c("caseid", paste0("v", str_pad(0:25, 3, pad = "0")))
select_cols <- c(individual_cols, sib_cols)


#for (countrycode in countrycodes) {
  
  # subset the list of downloads for given country
  download_sub <- dhs_downloads_cache[which(str_detect(toupper(names(dhs_downloads_cache)), countrycode))]
  
  # initialize list
  country_svy_list <- list()
  ### loop over each survey year for that country
  for (i in names(download_sub)) {
    
    # read in file, keep wanted cols
    countryfile <- readRDS(download_sub[[i]]) %>%
      as_tibble() %>%
      select(any_of(select_cols)) %>%
      # convert labelled columns to factors (number keys replaced by corresponding text)
      haven::as_factor() %>%
      # then convert EVERYTHING to characters. Needed to allow combination of two factor columns with different levels
      # or combination of factor and integer columns. (different encoding from survey year to survey year)
      mutate(across(.cols = everything(),
                    .fns = as.character)) %>%
      # unique identifier for each survey. Paste together survey phase and last year of survey
      mutate(surveyid = paste(v000,
                              (max(v007, na.rm = T) %>% str_sub(-2, -1)),
                              sep = "_")) %>%
      relocate(surveyid, .after = "v000") %>%
      mutate(across(.fns = str_trim ))
    
    # append each year to list
    country_svy_list <- append(country_svy_list, list(countryfile))
  }
  
  # after list is filled by loop, set list element names
  country_svy_list <- country_svy_list %>% set_names(map_chr(country_svy_list, function(x) x %>% .[1,"surveyid"] %>% pull()   ))
  
  # bind list into a single df
  bound_dhs <- bind_rows(country_svy_list) %>% type_convert()

  # extract country's name from the dataframe (first list element, first row, column v000, first two letters)
  country_2dig <- country_svy_list[[1]][1, "v000"] %>% str_sub(1, 2)
  # assign country's name as the new name for the bound dataframe
  assign(country_2dig, bound_dhs, envir = globalenv())
  
  
  # delete intermediate objects
  rm(list = c("download_sub",
              "countryfile",
              "bound_dhs"
              #,"list_dhs"
              ))
  
#}





#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Package intro----
#~~~~~~~~~~~~~~~~~~~~~~~~
# load data
# load(here("/data/malawi.RData"))

# reshape to long
sib_long. <- mdhs::mdhs_reshape(data = get(country_2dig), sib_cols = c(1,2,4,8)) 
# add in interview date variable. NEEDED FOR MORTALITY CALC
sib_long <- mdhs::mdhs_merge(sib_long., get(country_2dig), respondent_vars = c(individual_cols, "surveyid") ) 
# Calculate observation time
sib_long <- 
  sib_long %>% 
  mutate(mm2 = case_when( mm2 == "alive" ~ 1,
                          mm2 == "dead" ~ 0,
                          TRUE ~ NA_real_)) %>% 
  mutate(mm4 = as.numeric(mm4)) %>% 
  mutate(mm8 = as.numeric(mm8)) %>% 
  as_tibble() %>% 
  mutate(region = case_when(str_detect(v024, "south") ~ "south", 
                            str_detect(v024, "north") ~ "north",
                            str_detect(v024, "central") ~ "central",
                            TRUE ~ NA_character_), 
         region = as.factor(region)) %>% 
  mutate(sex = case_when(mm1 == "male" ~ "male", 
                         mm1 == "female" ~ "female",
                         TRUE ~ NA_character_), 
         sex = as.factor(sex))

sib_long$obs_time <- mdhs::mdhs_obs_time(sib_long)


# # check region recoding
# sib_long %>%
#   ggplot(aes(x = surveyid, color = region, fill = region)) +
#   geom_bar(stat = "count") + 
#   scale_fill_paletteer_d("ggthemes::Tableau_20") +
#   scale_color_paletteer_d("ggthemes::Tableau_20")
# 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#              SPLIT PROGRAMMATICALLY         ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~ Import new functions ----------------------------

source(here("functions/key_functions_rework.R" ))

#~~~ deaths and exposure columns --------------------

deaths <- 
  sib_long %>% 
  sample_n(200000) %>% 
  filter(!is.na(sex) & !is.na(region)) %>% 
  mutate(region_sex = paste(region, sex, sep = "_")) %>% 
  split(.$region_sex) %>% 
  map(~ age_year_deaths(.x, 0, 49, 2000, 2015)) %>% 
  map(~ t(.x) ) %>% 
  map(~ melt(.x)) %>% 
  map(~ rename(.x, deaths = value, age = Var1, year = Var2) ) %>% 
  bind_rows(.id = "region_sex")

exposure <- 
  sib_long %>% 
  sample_n(200000) %>% 
  filter(!is.na(sex) & !is.na(region)) %>% 
  mutate(region_sex = paste(region, sex, sep = "_")) %>% 
  split(.$region_sex) %>% 
  map(~ age_year_exposure(.x, 0, 49, 2000, 2015)) %>% 
  map(~ t(.x) ) %>% 
  map(~ melt(.x)) %>% 
  map(~ rename(.x, exposure = value, age = Var1, year = Var2) ) %>% 
  bind_rows(.id = "region_sex")


#~~ Summarise separately for each sex ---------------------------

mort <- 
  deaths %>%
  left_join(exposure) %>%
  separate("region_sex", into = c("region", "sex"), sep = "_") %>% 
  filter(year <= 2013) %>% 
  mutate(age_cat = case_when( age >=0 & age <= 4 ~ "0 - 4 yrs",
                              age >= 5 & age <= 14 ~ "5 - 14 yrs",
                              age >= 15 & age <= 49 ~ "15 - 49 yrs")) %>% 
  mutate(age_cat = factor(age_cat,levels =  c("0 - 4 yrs", "5 - 14 yrs", "15 - 49 yrs"))) %>%  
  # group_by(age_cat, year, region, sex) %>% 
  group_by(age_cat, year, region) %>% 
  mutate(deaths = sum(deaths), 
         exposure = sum(exposure)) %>% 
  slice_head() %>% select(-c("age")) %>% 
  mutate(death_prob = deaths/exposure) %>% 
  ungroup()


# plot
mort_p <- 
  mort %>% 
  ggplot( aes(x=year, y = death_prob, color = region, fill = region)) +
  facet_wrap(~ age_cat, ncol = 1, scales = "free") +
  # males first, through line
  #geom_point(alpha = 0.2) + 
  #geom_line(alpha = 0.2) + 
  geom_smooth(alpha = 0.2) + 
  scale_fill_paletteer_d("Redmonder::qPBI") +
  scale_color_paletteer_d("Redmonder::qPBI") + 
  labs(x = "Year", y = "Annual Death Probability") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01))


mort_plotly <- 
  ggplotly(mort_p) %>% 
  layout(yaxis = list( automargin = TRUE, 
                       titlefont = list(size=30)))

mort_plotly[['x']][['layout']][['annotations']][[2]][['x']] <- -0.13

mort_plotly


mort %>%
  ggplot(aes(x = year , y = age)) +
  geom_tile(aes(fill = death_prob)) +
  scale_fill_viridis(option = "B",
                     limits = c(-3, -1 ),
                     breaks = c(-1, -2, -3, -4),
                     labels = c("1/10", "1/100", "1/1,000", "1/10,000")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust=0,size=12, face = "bold"),
        plot.subtitle = element_text(size=10 ),
        legend.title = element_text(size = 11)) +
  labs(title = "Raw Lexis",
       subtitle = "Mortality for Malawians aged 0 to 15 in the years 1975 to 2000",
       fill = "Probability\nof death\nthat year",
       x = "Year",
       y = "Age") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 60, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")
  )


# Uncomment the below (and add sex to group by variable above) to subset by sex as well
  # # males first, through line
  # geom_point(data = subset(mort, sex == "male"), alpha = 0.2) + 
  # geom_line(data = subset(mort, sex == "male"), linetype = 1, alpha = 0.2) + 
  # geom_smooth(data = subset(mort, sex == "male"),linetype = 1, alpha = 0.2) + 
  # # females next, dashed line
  # geom_point(data = subset(mort, sex == "female"), alpha = 0.2) + 
  # geom_line(data = subset(mort, sex == "female"),linetype = 2, alpha = 0.2) + 
  # geom_smooth(data = subset(mort, sex == "female"),linetype = 2, alpha = 0.2) + 
  # scale_fill_paletteer_d("Redmonder::qPBI") +
  # scale_color_paletteer_d("Redmonder::qPBI")

# mort_p

# ggplotly(mort_p)


#~~ Summarise separately for each sex ---------------------------


# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #              SEPARATE REGIONS         ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~ Facet----
# #~~~~~~~~~~~~~~~~~~~~~~~~
# north <- sib_long %>% filter(region == "north")
# south <- sib_long %>% filter(region == "south")
# central <- sib_long %>% filter(region == "central")
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~ Import new functions----
# #~~~~~~~~~~~~~~~~~~~~~~~~
# 
# source(here("functions/key_functions_rework.R" ))
# 
# 
# # deaths for north
# deaths_north <- age_year_deaths(north, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(deaths = value, age = Var1, year = Var2)
# # exposure for north
# exposure_north <- age_year_exposure(north, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(exposure = value, age = Var1, year = Var2)
# # combine into single df
# final_north <- deaths_north %>% left_join(exposure_north) %>% mutate(death_prob = deaths/exposure, log_death_prob = log10(death_prob), region = "north") 
# 
# 
# # deaths for south
# deaths_south <- age_year_deaths(south, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(deaths = value, age = Var1, year = Var2)
# # exposure for south
# exposure_south <- age_year_exposure(south, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(exposure = value, age = Var1, year = Var2)
# # combine into single df
# final_south <- deaths_south %>% left_join(exposure_south) %>% mutate(death_prob = deaths/exposure, log_death_prob = log10(death_prob), region = "south")
# 
# 
# # deaths for central
# deaths_central <- age_year_deaths(central, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(deaths = value, age = Var1, year = Var2)
# # exposure for central
# exposure_central <- age_year_exposure(central, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(exposure = value, age = Var1, year = Var2)
# # combine into single df
# final_central <- deaths_central %>% left_join(exposure_central) %>% mutate(death_prob = deaths/exposure, log_death_prob = log10(death_prob), region = "central")
# 
# 
# 
# # combine and compute aggregate statistics
# final_regions <- 
#   final_central %>% 
#   bind_rows(final_south) %>% 
#   bind_rows(final_north) %>% 
#   #filter(year <= 2014) %>% 
#   mutate(age_cat = case_when( age >=0 & age <= 4 ~ "0-4",
#                               age >= 5 & age <= 14 ~ "5-14",
#                               age >= 15 & age <= 49 ~ "15-49")) %>% 
#   mutate(age_cat = factor(age_cat,levels =  c("0-4", "5-14", "15-49"))) %>%  
#   group_by(age_cat, year, region) %>% 
#   mutate(deaths = sum(deaths), 
#          exposure = sum(exposure)) %>% 
#   group_by(age_cat, year, region) %>% 
#   slice_head() %>% select(-c("age")) %>% 
#   mutate(death_prob = deaths/exposure) %>% 
#   ungroup()
# 
# 
# # plot
# final_regions_p <- 
#   final_regions %>% 
#   filter(year <= 2014) %>% 
#   ggplot(aes(x=year, y = death_prob, group = region, color = region)) +
#   facet_wrap(~ age_cat, ncol = 1, scales = "free") +
#   geom_point(alpha = 0.2) + 
#   geom_line(alpha = 0.2) +
#   geom_smooth(aes(fill = region), alpha = 0.3) + 
#   scale_fill_paletteer_d("Redmonder::qPBI") +
#   scale_color_paletteer_d("Redmonder::qPBI")
#   #+
#   #geom_text(aes(label = deaths), size = 3, position = position_nudge(y = 0.002))
# 
# ggplotly(final_regions_p)
# 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #              SEPARATE SEXES             ----
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~ Facet----
# #~~~~~~~~~~~~~~~~~~~~~~~~
# m <- sib_long %>% filter(mm1==1)
# f <- sib_long %>% filter(mm1==2)
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~
# #~~~ Import new functions----
# #~~~~~~~~~~~~~~~~~~~~~~~~
# 
# source(here("functions/key_functions_rework.R" ))
# 
# 
# # deaths for m
# deaths_m <- age_year_deaths(m, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(deaths = value, age = Var1, year = Var2)
# # exposure for m
# exposure_m <- age_year_exposure(m, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(exposure = value, age = Var1, year = Var2)
# # combine into single df
# final_m <- deaths_m %>% left_join(exposure_m) %>% mutate(death_prob = deaths/exposure, log_death_prob = log10(death_prob), sex = "m") 
# 
# # deaths for f
# deaths_f <- age_year_deaths(f, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(deaths = value, age = Var1, year = Var2)
# # exposure for f
# exposure_f <- age_year_exposure(f, 0, 49, 1975, 2016) %>% t() %>% melt() %>% rename(exposure = value, age = Var1, year = Var2)
# # combine into single df
# final_f <- deaths_f %>% left_join(exposure_f) %>% mutate(death_prob = deaths/exposure, log_death_prob = log10(death_prob), sex = "f")
# 
# # combine and compute aggregate statistics
# final_sex <- final_m %>% 
#   bind_rows(final_f) %>% 
#   filter(year <= 2014) %>% 
#   mutate(age_cat = case_when( age >=0 & age <= 4 ~ "0-4",
#                               age >= 5 & age <= 14 ~ "5-14",
#                               age >= 15 & age <= 49 ~ "15-49")) %>% 
#   mutate(age_cat = factor(age_cat,levels =  c("0-4", "5-14", "15-49"))) %>%  
#   group_by(age_cat, year, sex) %>% 
#   mutate(deaths = sum(deaths), 
#          exposure = sum(exposure)) %>% 
#   group_by(age_cat, year, sex) %>% 
#   slice_head() %>% select(-c("age")) %>% 
#   mutate(death_prob = deaths/exposure) %>% 
#   ungroup()
#   
# # plot
# final_sex_p <- 
#   final %>% 
#   ggplot(aes(x=year, y = death_prob, group = sex, color = sex)) +
#   facet_wrap(~ age_cat, ncol = 1, scales = "free") +
#   geom_point() + 
#   geom_line() +
#   geom_text(aes(label = deaths), size = 3, position = position_nudge(y = 0.002))
# 
# final_sex_p
# 
# ggplotly(final_sex_p)
#   
# 
# #~~~~~~~~~~~~~~~~~~~~~~
# #~  Rate ratio calculation function  ----
# #~~~~~~~~~~~~~~~~~~~~~~
# 
# glm.RR <- function(GLM.RESULT, digits = 2) {
# 
#   COEF      <- stats::coef(GLM.RESULT)
#   CONFINT   <- stats::confint(GLM.RESULT)
#   TABLE.EXP <- round(exp(cbind(coef=COEF, CONFINT)), digits)
#   
#   TABLE.EXP
# }
# 
# #~~~~~~~~~~~~~~~~~~~~~~
# #~  Model  ----
# #~~~~~~~~~~~~~~~~~~~~~~
# 
# ## Including offset(log(n)) in the right hand side
# model_1 <- glm(deaths ~ age_cat * as.factor(year) + offset(log(exposure)), family = poisson(link = "log"), data = final)
# 
# ## Results from regular Poisson
# summary(model_1)
# 
# ## get relative risks
# glm.RR(model_1)
# 
# 
# 
# df <- data.frame(
#   x=rnorm(25),
#   y=rnorm(25),
#   g=rep(factor(LETTERS[1:5]), 5)
# )
# 
# X <- split(df, df$g)
# str(X)
# 
# 






