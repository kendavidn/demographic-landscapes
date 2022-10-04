
# List of functions for harmonizing region coding (variable v024) between survey years

remap_regions <- list()


## BF for Burkina Faso

## Used this: https://dhsprogram.com/pubs/pdf/FR110/FR110.pdf
## and this: https://en.wikipedia.org/wiki/Provinces_of_Burkina_Faso
remap_regions$BF <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    v024 == "centre" ~ "central and southern",
                    v024 == "centre-nord" ~ "central and southern",
                    v024 == "centre-sud" ~ "central and southern",
                    v024 == "plateau central" ~ "central and southern",
                    v024 == "plateau central" ~ "central and southern",
                    v024 == "centre (sans ouagadougou)" ~ "central and southern",
                    v024 == "ouagadougou" ~ "central and southern",



                    v024 == "centre-est" ~ "eastern",
                    v024 == "est" ~ "eastern",
                    v024 == "east" ~ "eastern",


                    v024 == "nord" ~ "northern",
                    v024 == "north" ~ "northern",
                    v024 == "sahel" ~ "northern",


                    v024 == "sud-ouest" ~ "central and southern",
                    v024 == "hauts basins" ~ "central and southern",
                    v024 == "hauts bassins" ~ "central and southern",

                    v024 == "central/south" ~ "central and southern",


                    v024 == "boucle de mouhoun" ~ "western",
                    v024 == "cascades" ~ "western",
                    v024 == "centre-ouest" ~ "western",
                    v024 == "west" ~ "western",

                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}




## CM for Cameroon
remap_regions$CM <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    surveyid %in% c("CM2_91", "CM3_98") ~ "dropped_region",
                    v024 == "adamaoua" ~ "adamawa",
                    v024 == "centre (without yaounde)|centre (sans yaoundé)" ~ "centre",
                    v024 == "est" ~ "east",
                    v024 == "extreme nor" ~ "far north",
                    v024 == "far-north" ~ "far north",
                    v024 == "extrême-nord" ~ "far north",
                    v024 == "littoral (without douala)" ~ "littoral",
                    v024 == "nord" ~ "north",
                    v024 == "nord-ouest" ~ "north west",
                    v024 == "nord ouest" ~ "north west",
                    v024 == "north-west" ~ "north west",
                    v024 == "sud" ~ "south",
                    v024 == "sud ouest" ~ "south west",
                    v024 == "sud-ouest" ~ "south west",
                    v024 == "south-west" ~ "south west",
                    v024 == "ouest" ~ "west",
                    v024 == "yaoundé" ~ "yaounde",
                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}

# ## KM for Comoros


remap_regions$KM <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    str_detect(string = v024, pattern = "moh") ~ "moheli",
                    v024 == "grande comore" ~ "ngazidja",
                    v024 == "anjouan" ~ "ndzouani",
                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}


## MW for Malawi
remap_regions$MW <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    str_detect(v024, "south") ~ "southern",
                    str_detect(v024, "north") ~ "northern",
                    str_detect(v024, "central") ~ "central",
                    TRUE ~ "dropped_region")) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}

## NG for Nigeria
remap_regions$NG <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    v024 == "northeast" ~ "dropped_region",
                    v024 == "northwest" ~ "dropped_region",
                    v024 == "southeast" ~ "dropped_region",
                    v024 == "southwest" ~ "dropped_region",
                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}

## RW for Rwanda
remap_regions$RW <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    region == "kibungo" ~ "eastern",
                    region == "umutara" ~ "eastern",
                    region == "east" ~ "eastern",
                    region == "est" ~ "eastern",
                    region == "byumba" ~ "northern",
                    region == "ruhengeri" ~ "northern",
                    region == "north" ~ "northern",
                    region == "nord" ~ "northern",
                    region == "butare" ~ "southern",
                    region == "gikongoro" ~ "southern",
                    region == "gitarama" ~ "southern",
                    region == "south" ~ "southern",
                    region == "sud" ~ "southern",
                    region == "cyangugu" ~ "western",
                    region == "gisenyi" ~ "western",
                    region == "kibuye" ~ "western",
                    region == "west" ~ "western",
                    region == "ouest" ~ "western",
                    region == "kigali city" ~ "kigali",
                    region == "city of kigali" ~ "kigali",
                    region == "kigali ville (pvk)" ~ "kigali",
                    region == "ville de kigali" ~ "kigali",
                    is.na(region) ~ "NA",
                    TRUE ~ "dropped_region")) %>%
    mutate(region = str_to_title(region))

  # dropped regions from 1992 survey:
  ## central, south
  ## kigali
  ## northeast
  ## northwest
  ## southwest

  # dropped regions from 2000 survey:
  ## kigali rurale

  # dropped regions from 2005 survey:
  ## kigali ngali

  return(df_remapped)

}


# ## TD for CHad
#
# ## Used this: https://dhsprogram.com/pubs/pdf/FR170/FR170-TD04.pdf
# ## and this: https://en.wikipedia.org/wiki/Regions_of_Chad
# remap_regions$TD <- function(df){
#
#   df_remapped <-
#     df %>% mutate(region = str_to_lower(v024),
#                   region = case_when(
#                     v024 == "borkou/tibesti" ~ "BET",
#                     v024 == "borkou/tibesti" ~ "BET",
#                     v024 == "ennedi" ~ "BET",
#                     v024 == "ennedi" ~ "BET",
#
#                     v024 == "ouaddai" ~ "BET",
#
#
#                     TRUE ~ region)) %>%
#     mutate(region = str_to_title(region))
#
#   return(df_remapped)
#
# }

## ZA for South Africa
remap_regions$ZA <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    v024 == "western  cape" ~ "western cape",
                    v024 == "kwazulu-natal" ~ "kwazulu natal",
                    v024 == "limpopo" ~ "dropped_region",
                    v024 == "northern province" ~ "dropped_region",
                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}

## ZW for Zimbabwe
remap_regions$ZW <- function(df){

  df_remapped <-
    df %>% mutate(region = str_to_lower(v024),
                  region = case_when(
                    str_detect(v024, "harare") ~ "harare",
                    v024 == "matebeleland south" ~ "matabeleland south",
                    v024 == "matebeleland north" ~ "matabeleland south",
                    TRUE ~ region)) %>%
    mutate(region = str_to_title(region))

  return(df_remapped)

}






