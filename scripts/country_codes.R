#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Import packages----
#~~~~~~~~~~~~~~~~~~~~~~~~


library(rdhs)
library(countrycode)

## what are the countryIds
dhs_id <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))
iso_id <- countrycode::countrycode(ids$CountryName, 
                         origin =  "country.name", 
                         destination =  "iso2c")

DHStoISO <- data.frame(dhs_id, iso_id)




