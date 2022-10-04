
#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Death prob cohort numerator function----
#~~~~~~~~~~~~~~~~~~~~~~~~

age_cohort_deaths_single <- function(data, age, cohort , surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  ### see Chapter 2 of Demography by Samuel H Preston (2001) for further explanation of the formula used

  # convert cohort to CMC
  lower_lim_spec_age_in_months <- age * 12
  upper_lim_spec_age_in_months <- (age * 12) + 11

  cohort_lower_limit_CMC <- ((cohort - 1900) * 12 ) + 1
  cohort_upper_limit_CMC <-  ((cohort - 1900) * 12) + 12

  # count deaths
  numerator <- sum(
    # people from the relevant cohort (who were born between January and December of the specified year)
    (
      data[[DOB_CMC]] >= cohort_lower_limit_CMC
      & data[[DOB_CMC]] <= cohort_upper_limit_CMC
    )
    & (
      # who are dead
      data[[surv_stat]] == 0 &
        # and whose observation time is within the specified range (died at the specified age)
        data[[obs_time]] >= lower_lim_spec_age_in_months &
        data[[obs_time]] <= upper_lim_spec_age_in_months
    ),
    na.rm = TRUE
  )
  return(numerator)

}


age_cohort_deaths <- function(data, lower_age, upper_age, lower_cohort, upper_cohort, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  # create a dataframe with the requisite number of rows and columns
  df <- data.frame(matrix(NA, nrow = (upper_cohort - lower_cohort + 1) , ncol = (upper_age - lower_age + 1)))

  # name each row and column
  rownames(df) <- c(lower_cohort:upper_cohort)
  colnames(df) <- c(lower_age:upper_age)

  # define cohort and age ranges for the loop
  cohort_range <- lower_cohort:upper_cohort
  age_range <- lower_age:upper_age

  # iterate death_prob_cohort function over each of the ages and cohorts, populating the data frame
  for (i in cohort_range){
    for (j in age_range){
      df[which(rownames(df)==i), which(colnames(df)== j)] <- age_cohort_deaths_single(data = data, age = j, cohort = i,
                                                                                      surv_stat = surv_stat,
                                                                                      DOB_CMC = DOB_CMC,
                                                                                      obs_time = obs_time )
    }
  }
  return(df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Death prob cohort denominator function----
#~~~~~~~~~~~~~~~~~~~~~~~~

age_cohort_exposure_single <- function(data, age, cohort , surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  ### see Chapter 2 of Demography by Samuel H Preston (2001) for further explanation of the formula used

  # convert cohort to CMC
  lower_lim_spec_age_in_months <- age * 12
  upper_lim_spec_age_in_months <- (age * 12) + 11

  cohort_lower_limit_CMC <- ((cohort - 1900) * 12 ) + 1
  cohort_upper_limit_CMC <-  ((cohort - 1900) * 12) + 12

  denominator <- sum(# count of individuals
    (data[[DOB_CMC]] >= cohort_lower_limit_CMC
     &data[[DOB_CMC]] <= cohort_upper_limit_CMC)
    # from the relevant cohort
    & (( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months)
       # who are alive and lived past the specified age
       |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months)), na.rm = TRUE)
  # or deceased but died past the specified age

  return(denominator)
}


age_cohort_exposure <- function(data, lower_age, upper_age, lower_cohort, upper_cohort, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  # create a dataframe with the requisite number of rows and columns
  df <- data.frame(matrix(NA, nrow = (upper_cohort - lower_cohort + 1) , ncol = (upper_age - lower_age + 1)))

  # name each row and column
  rownames(df) <- c(lower_cohort:upper_cohort)
  colnames(df) <- c(lower_age:upper_age)

  # define cohort and age ranges
  cohort_range <- lower_cohort:upper_cohort
  age_range <- lower_age:upper_age

  # iterate death_prob_cohort function over each of the ages and cohorts, populating the data frame
  for (i in cohort_range){
    for (j in age_range){
      df[which(rownames(df)==i), which(colnames(df)== j)] <- age_cohort_exposure_single(data = data, age = j, cohort = i,
                                                                                        surv_stat = surv_stat,
                                                                                        DOB_CMC = DOB_CMC,
                                                                                        obs_time = obs_time )
    }
  }
  return(df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Death prob year numerator function----
#~~~~~~~~~~~~~~~~~~~~~~~~

age_year_deaths_single <- function(data, age, year, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){


  ## using the formula (sDxY + pDxY) /   ((BxY + BxYminus1 - sDxYminus1)/2),
  ## which is a modification of the formula  sDxY/BxY  +  ((BxY-sDxY)/BxY)   *   (pDxY/(BxYminus1 - sDxYminus1))
  ## see Chapter 2 of Demography by Samuel H Preston (2001) for further explanation of the formula

  # convert specified age to CMC range
  lower_lim_spec_age_in_months <- age * 12
  upper_lim_spec_age_in_months <- (age * 12) + 11

  leading_cohort <- year - age         # the cohort that turned x in specified year, where x is the specified age
  # convert leading cohort year of birth to CMC range
  leading_cohort_lower_limit_CMC <- ((leading_cohort - 1900) * 12 ) + 1
  leading_cohort_upper_limit_CMC <-  ((leading_cohort - 1900) * 12) + 12

  lagging_cohort <- leading_cohort - 1 # the cohort that turned x in the year prior to the specified year
  # convert lagging cohort year of birth to CMC range
  lagging_cohort_lower_limit_CMC <- ((lagging_cohort - 1900) * 12 ) + 1
  lagging_cohort_upper_limit_CMC <-  ((lagging_cohort - 1900) * 12) + 12

  # define function that converts years to CMC
  yr_to_CMC_start <- function(yr){
    CMC <- ((yr  - 1900) * 12) + 1
    return(CMC)
  }

  sDxY <-    sum(data[[DOB_CMC]] >= leading_cohort_lower_limit_CMC
                 & data[[DOB_CMC]] <= leading_cohort_upper_limit_CMC
                 # counts individual in the leading cohort,
                 &  (data[[surv_stat]] == 0
                     & (data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year)
                        & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year) + 11 ))
                 # who died in the specified year,
                 &  (data[[surv_stat]] == 0
                     & (data[[obs_time]] >= lower_lim_spec_age_in_months &
                          data[[obs_time]] <= upper_lim_spec_age_in_months)), na.rm = TRUE)
  # at the specified age


  pDxY <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
              & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
              # counts individuals in the lagging cohort,
              &  (data[[surv_stat]] == 0 & (
                data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year)
                & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year) + 11 ))
              # who died in the specified year,
              &  (data[[surv_stat]] == 0
                  & (data[[obs_time]] >= lower_lim_spec_age_in_months &
                       data[[obs_time]] <= upper_lim_spec_age_in_months)), na.rm = TRUE)
  # at the specified age


  numerator <- (sDxY + pDxY )
  return(numerator)
}


age_year_deaths <- function(data, lower_age, upper_age, lower_year, upper_year, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  # create a dataframe with the requisite number of rows and columns
  df <-  data.frame(matrix(NA, nrow = (upper_year - lower_year + 1) , ncol = (upper_age - lower_age + 1)))

  # name each row and column
  rownames(df) <- c(lower_year:upper_year)
  colnames(df) <- c(lower_age:upper_age)

  # define year and age ranges
  year_range <- lower_year:upper_year
  age_range <- lower_age:upper_age

  for (i in year_range){
    for (j in age_range){
      df[which(rownames(df)==i), which(colnames(df)== j)] <- age_year_deaths_single(data = data, age = j, year = i,
                                                                                    surv_stat = surv_stat,
                                                                                    DOB_CMC = DOB_CMC,
                                                                                    obs_time = obs_time )
    }
  }
  return(df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~
#~~~ Death prob year denominator function----
#~~~~~~~~~~~~~~~~~~~~~~~~

age_year_exposure_single <- function(data, age, year, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  ## using the formula (sDxY + pDxY) /   ((BxY + BxYminus1 - sDxYminus1)/2),
  ## which is a modification of the formula  sDxY/BxY  +  ((BxY-sDxY)/BxY)   *   (pDxY/(BxYminus1 - sDxYminus1))
  ## see Chapter 2 of Demography by Samuel H Preston (2001) for further explanation of the formula

  # convert specified age to CMC range
  lower_lim_spec_age_in_months <- age * 12
  upper_lim_spec_age_in_months <- (age * 12) + 11

  leading_cohort <- year - age         # the cohort that turned x in specified year, where x is the specified age
  # convert leading cohort year of birth to CMC range
  leading_cohort_lower_limit_CMC <- ((leading_cohort - 1900) * 12 ) + 1
  leading_cohort_upper_limit_CMC <-  ((leading_cohort - 1900) * 12) + 12

  lagging_cohort <- leading_cohort - 1 # the cohort that turned x in the year prior to the specified year
  # convert lagging cohort year of birth to CMC range
  lagging_cohort_lower_limit_CMC <- ((lagging_cohort - 1900) * 12 ) + 1
  lagging_cohort_upper_limit_CMC <-  ((lagging_cohort - 1900) * 12) + 12

  # define function that converts years to CMC
  yr_to_CMC_start <- function(yr){
    CMC <- ((yr  - 1900) * 12) + 1
    return(CMC)
  }

  BxY <- sum(data[[DOB_CMC]] >= leading_cohort_lower_limit_CMC
             & data[[DOB_CMC]] <= leading_cohort_upper_limit_CMC
             # counts individuals in the leading cohort,
             & (( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                # who are EITHER alive and at or above the specified age,
                |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months)), na.rm = TRUE)
  # OR deceased but died at or above the specified age


  BxYminus1 <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
                   & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
                   # counts individuals in the lagging cohort,
                   &(( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                     # who are EITHER alive and at or above the specified age
                     |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months)), na.rm = TRUE)
  # OR deceased but died at or above the specified age

  sDxYminus1 <- sum(data[[DOB_CMC]] >= lagging_cohort_lower_limit_CMC
                    & data[[DOB_CMC]] <= lagging_cohort_upper_limit_CMC
                    # counts individuals in the lagging cohort,
                    & (( data[[surv_stat]] == 1 & data[[obs_time]] >= lower_lim_spec_age_in_months )
                       # who are EITHER alive and at or above the specified age
                       |(data[[surv_stat]] == 0 & data[[obs_time]] >= lower_lim_spec_age_in_months))
                    # OR deceased but died at or above above the specified age
                    &  (data[[surv_stat]] == 0 & (
                      data[[DOB_CMC]] + data[[obs_time]] >=  yr_to_CMC_start(year - 1)
                      & data[[DOB_CMC]] + data[[obs_time]] <=  yr_to_CMC_start(year - 1) + 11 )), na.rm = TRUE)
  # AND who died in the year prior to the specified one

  denominator <- (BxY + BxYminus1 - sDxYminus1)/2
  return(denominator)
}



age_year_exposure <- function(data, lower_age, upper_age, lower_year, upper_year, surv_stat = 'mm2', DOB_CMC ='mm4', obs_time = 'obs_time'){

  # create a dataframe with the requisite number of rows and columns
  df <-  data.frame(matrix(NA, nrow = (upper_year - lower_year + 1) , ncol = (upper_age - lower_age + 1)))

  # name each row and column
  rownames(df) <- c(lower_year:upper_year)
  colnames(df) <- c(lower_age:upper_age)

  # define year and age ranges
  year_range <- lower_year:upper_year
  age_range <- lower_age:upper_age

  for (i in year_range){
    for (j in age_range){
      df[which(rownames(df)==i), which(colnames(df)== j)] <- age_year_exposure_single(data = data, age = j, year = i,
                                                                                      surv_stat = surv_stat,
                                                                                      DOB_CMC = DOB_CMC,
                                                                                      obs_time = obs_time )
    }
  }
  return(df)
}

