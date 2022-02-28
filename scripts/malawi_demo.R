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
                "GGally",
                "ggfortify",
                "MortalitySmooth",
                "reshape2",
                "viridis",
                "rayshader",
                "gridExtra",
                "janitor"
))

#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Package intro----
#~~~~~~~~~~~~~~~~~~~~~~~~

load(here("/data/malawi.RData"))

# reshape to long
malawi_long <- mdhs_reshape(data = malawi)

# add in interview date variable. NEEDED FOR SURVIVAL CURVE
malawi_long <- mdhs_merge(malawi_long, malawi, respondent_vars = 'v008')

# Calculate observation time
malawi_long$obs_time <- mdhs_obs_time(malawi_long)

table(malawi$v026, malawi$v025)
#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Import new functions----
#~~~~~~~~~~~~~~~~~~~~~~~~

source(here("functions/key_functions_rework.R" ))

#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Smoothing attempt for age cohort data----
#~~~~~~~~~~~~~~~~~~~~~~~~
# This is inappropriate, as I am using this for age--cohort data instead of age--calendar-year data

# Cohorts born between 1975 and 2000, childhood years (0-15)
# Test it  
deaths <-
  age_cohort_deaths(malawi_long, 0, 15, 1975, 2000) %>%
  t()

# Cohorts born between 1975 and 2000, childhood years (0-15)
# Test it  
exposure <-
  age_cohort_exposure(malawi_long, 0, 15, 1975, 2000) %>%
  t()

ages <- 0:15
cohorts <- 1975:2000

## fit with BIC
fitBIC <- Mort2Dsmooth(x=ages, y=cohorts, Z=deaths,
                       offset=log(exposure))

## fitted log death rates from fitBIC
# empty df
grid. <- expand.grid(list(ages=ages, cohorts=cohorts))
# add in death probs from fitted model
grid.$lmx <- c(fitBIC$logmortality)

# lattice plot
levelplot(lmx ~ cohorts * ages, data = grid.)

# ggplot of same data
ggplot(grid., aes(x = cohorts , y = ages)) +
  geom_tile(aes(fill = lmx)) +
  scale_fill_viridis()


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ What does it look like without smoothing?----
#~~~~~~~~~~~~~~~~~~~~~~~~
# Here I replicate the same plots as above, but without the smoothing.

deaths_melt <-
  deaths %>%
  melt() %>%
  rename(deaths = value,
         ages = Var1,
         cohorts = Var2)

exposure_melt <-
  exposure %>%
  melt() %>%
  rename(exposure = value,
         ages = Var1,
         cohorts = Var2)

final_melt <-
  deaths_melt %>%
  left_join(exposure_melt) %>%
  mutate(death_prob = deaths/exposure) %>%
  mutate(log_death_prob = log(death_prob))

# lattice plot
levelplot(log_death_prob ~ cohorts * ages, data = final_melt)


# ggplot of same data
ggplot(final_melt, aes(x = cohorts , y = ages)) +
  geom_tile(aes(fill = log_death_prob)) +
  scale_fill_viridis()


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Smoothing attempt for age year data----
#~~~~~~~~~~~~~~~~~~~~~~~~

# Deaths between 1975 and 2000 to children (0-15)
deaths <-
  age_year_deaths(malawi_long, 0, 49, 1975, 2014) %>%
  t()

# Exposure between 1975 and 2000 of children (0-15) (Kids who could have died in the given period)
exposure <-
  age_year_exposure(malawi_long, 0, 49, 1975, 2014) %>%
  t()

ages <- 0:49
years <- 1975:2014

## fit with BIC
fitBIC <- Mort2Dsmooth(x=ages, y=years, Z=deaths,
                       offset=log(exposure))

## fitted log death rates from fitBIC
# empty df
grid. <- expand.grid(list(ages=ages, years=years))
# add in death probs from fitted model
grid.$lmx <- c(fitBIC$logmortality)

# lattice plot
levelplot(lmx ~ years * ages, data = grid.)

# ggplot of same data
p<- ggplot(grid., aes(x = years , y = ages)) +
  geom_tile(aes(fill = lmx)) +
  scale_fill_viridis()

p

# 3d plot
# plot_gg(p)

# What does it look like without smoothing?
# Here I try to replicate the same plots as above, but without the smoothing.

deaths_melt <-
  deaths %>%
  melt() %>%
  rename(deaths = value,
         ages = Var1,
         years = Var2)

exposure_melt <-
  exposure %>%
  melt() %>%
  rename(exposure = value,
         ages = Var1,
         years = Var2)

final_melt <-
  deaths_melt %>%
  left_join(exposure_melt) %>%
  mutate(death_prob = deaths/exposure) %>%
  mutate(log_death_prob = log10(death_prob))

# neater table to show olivia
deaths_melt %>%
  left_join(exposure_melt) %>%
  mutate(`mortality(%)` = round ((100* deaths/exposure), 1) )

# lattice plot
levelplot(log_death_prob ~ years * ages, data = final_melt)


# ggplot of same data
ggplot(final_melt, aes(x = years , y = ages)) +
  geom_tile(aes(fill = log_death_prob)) +
  scale_fill_viridis()


# saveRDS(final_melt, file = here("results/malawi_matrix_raw.RDS"))

# saveRDS(grid., file = here("results/malawi_matrix_smooth.RDS"))


#~~~~ Compare smoothed and non-smoothed plots  ----

# raw
p1 <- final_melt %>%
  ggplot(aes(x = years , y = ages)) +
  geom_tile(aes(fill = log_death_prob)) +
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

# smoothed
p2 <- grid. %>%
  # convert from ln to log10
  mutate(log_death_prob = log10(exp(lmx)) ) %>%
  ggplot(aes(x = years , y = ages)) +
  geom_tile(aes(fill = log_death_prob)) +
  scale_fill_viridis(option = "B",
                     limits = c(-3, -1 ),
                     breaks = c(-1, -2, -3, -4),
                     labels = c("1/10", "1/100", "1/1,000", "1/10,000")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust=0,size=12, face = "bold"),
        plot.subtitle = element_text(size=10 ),
        legend.title = element_text(size = 11)) +
  labs(title = "Smoothed Lexis",
       subtitle = "Mortality for Malawians aged 0 to 15 in the years 1975 to 2000",
       fill = "Probability\nof death\nthat year",
       x = "Year",
       y = "Age") +
  #scale_x_continuous(limits = c(1982,2013)) +
  #scale_y_continuous(limits = c(0,48)) +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 60, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")
  )



grid.arrange(p1, p2, nrow =1 )







