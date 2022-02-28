###########
# Iterate flexdashboard_mortDHS.Rmd over several countries
###########

library("knitr")
library("rmarkdown")
library("here")
library("tidyverse")
library("lubridate")

# countries_to_run <- c("BF") Done Nov 1 at 23:17
# countries_to_run <- c("CM") Done Nov 1 at 23:33
# countries_to_run <- c("KM") Done Nov 1 at 23:45
# countries_to_run <- c("NG") Done Nov 2 at 00:00
# countries_to_run <- c("MW") Done Nov 2 at 00:00



# countries_to_run <- c("BF", "CM", "KM", "MW", "NG", "RW", "ZA", "ZW")

countries_to_run <- c("MW")

# countries_to_run <- c("BF", "CM", "KM")

# countries_to_run <- c("NG", "RW", "ZA", "ZW")

lower_age <-  0
upper_age <-   49
surveyYearStart <-  1984

# for PDF
for (country_code_DHS in countries_to_run) {
  render(  input = here("scripts/Rmd/dashboard_mortDHS.Rmd"),
           params = list(lower_age = lower_age, 
                         upper_age = upper_age,
                         surveyYearStart = surveyYearStart, 
                         country_code_DHS = country_code_DHS
                         ),
           output_file = paste0(here("results/dashboards/"), country_code_DHS, ".html"))
}