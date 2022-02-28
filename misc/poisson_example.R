library(tidyverse)


#~~~~~~~~~~~~~~~~~~~~~~
#~  Rate ratio calculation function  ----
#~~~~~~~~~~~~~~~~~~~~~~

glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

#~~~~~~~~~~~~~~~~~~~~~~
#~  Melanoma Data  ----
#~~~~~~~~~~~~~~~~~~~~~~

## Create a dataset manually
nonmel <- read_table2("
cases      n        city age.range
   1 172675 Minneapolis     15_24
  16 123065 Minneapolis     25_34
  30  96216 Minneapolis     35_44
  71  92051 Minneapolis     45_54
 102  72159 Minneapolis     55_64
 130  54722 Minneapolis     65_74
 133  32185 Minneapolis     75_84
  40   8328 Minneapolis       85+
   4 181343      Dallas     15_24
  38 146207      Dallas     25_34
 119 121374      Dallas     35_44
 221 111353      Dallas     45_54
 259  83004      Dallas     55_64
 310  55932      Dallas     65_74
 226  29007      Dallas     75_84
  65   7583      Dallas       85+
")

## It describes the number of cases of non-melanoma skin cancer among residents of two cities in each age category.

#~~~~~~~~~~~~~~~~~~~~~~
#~  Model  ----
#~~~~~~~~~~~~~~~~~~~~~~

## Including offset(log(n)) in the right hand side
model.1 <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)

## Results from regular Poisson
summary(model.1)

## get relative risks
glm.RR(model.1)

#~~~~~~~~~~~~~~~~~~~~~~
#~  Prediction  ----
#~~~~~~~~~~~~~~~~~~~~~~

## Predict case per person (n = 1) for oldest people in the Minneapolis
exp(predict(model.1, newdata = data.frame(city = "Minneapolis", age.range = "85+", n = 1)))

## Create dataset to predict for
newdat1 <- nonmel[c("city","age.range")]
newdat2 <- newdat1

## Predicted number of cases per person
newdat1$n <- 1
nonmel$pred.cases.per.one <- exp(predict(model.1, newdat1))

## Predicted number of cases per one thousand persons
newdat2$n <- 1000
nonmel$pred.cases.per.thousand <- exp(predict(model.1, newdat2))

## Predicted number of cases per actual population
nonmel$pred.cases <- exp(predict(model.1))

## Show
nonmel

#~~~~~~~~~~~~~~~~~~~~~~
#~  Quasi-Poisson regression with an estimated dispersion parameter ----
#~~~~~~~~~~~~~~~~~~~~~~

## quasi-Poisson to allow the scale parameter to change from 1. Show the dispersion parameter.
model.1q <- glm(cases ~ city + age.range, offset = log(n), family = quasipoisson(link = "log"), data = nonmel)
summary(model.1q)

## The quasi-Poisson method can be used to estimate the dispersion parameter, i.e., degree of overdispersion. 
## It is 1.16, and almost equal to 1, which is the value used in regular Poisson regression. 
## Thus, the Poisson model is appears ok. If it is large, the standard error estimation from the quasi-Poisson model should be used. 
## Here because of the near one dispersion parameter, the SE estimates are almost identical in both methods.
#



#~~~~~~~~~~~~~~~~~~~~~~
#~  Lung Cancer data  ----
#~~~~~~~~~~~~~~~~~~~~~~

library(ISwR)
## Load data
data(eba1977)
eba1977

## Fit Poisson model
model.2 <- glm(cases ~ city + age, offset = log(pop), family = poisson(link = "log"), data = eba1977)
summary(model.2)

## Check dispersion parameter with quasi-Poisson regression
model.2q <- glm(cases ~ city + age, offset = log(pop), family = quasipoisson(link = "log"), data = eba1977)
summary(model.2q)

#~~~~~~~~~~~~~~~~~~~~~~
#~  British male physicians  ----
#~~~~~~~~~~~~~~~~~~~~~~

library(SMPracticals)
data(lung.cancer)

## Poisson
model.3 <- glm(y ~ years.smok + cigarettes,
               offset = log(Time),
               family = poisson(link = "log"), data = lung.cancer)    
summary(model.3)

glm.RR(model.3, 3)

















