# STEP 1: SET UP ----------------------------------------------------------

# To see your language locale 
Sys.getlocale() 

# To change it into English 
Sys.setlocale("LC_ALL", "English")  


# Ensures the package "pacman" is installed
if (!require("pacman")) {
  install.packages("pacman") }

pacman::p_load(
  
  rio,             # to import datasets
  skimr,           # for displaying your data
  janitor,         # a package for data cleaning
  foreign,         # to import stata files
  MASS,            # for negative binomial regression
  broom,           # to generate tidy tibbles of regression analysis 
  epitools,        # for univariable analysis
  lubridate,       # for working with dates
  performance,     # Assessment of Regression Models Performance
  gtsummary,       # descriptive tables and hypothesis testing
  tidyverse        # data management and visualization
)


# STEP 2: IMPORT AND EXPLORE THE DATA -------------------------------------

# Import the dataset for HIV  
hiv.data <- read.dta("data/HIV_Spain.dta")

# Inspect your dataset and generate basic statistics using the skim function  
skim(hiv.data)
# Glimpse is an alternative to skim which gives more accurate information about the nature of the  variables
glimpse(hiv.data)

# See the different values of the variable seroco
#if we just to a tabyl we see the STATA labels
tabyl(hiv.data$seroco)
#we have to transform it into numeric to see the original values
tabyl(as.numeric(hiv.data$seroco))


# STEP 3: UNIVARIATE ANALYSIS ---------------------------------------------

## 3.1: Main factors associated with HIV seroconversion --------------------

hiv.data %>% 
  select(sex, idu, msm, sexwork, hete, age2, lasttestY, centro) %>% # we select the columns we're interested (note that we do not select the outcome: seroco)
  map(.f = ~riskratio(.x, hiv.data$seroco))     # here we call the function for each column. ".x" represents all the columns in the database that have not been removed previously


# STEP 4: POISSON REGRESSION ----------------------------------------------


## 4.1: Data preparation for poisson regression ----------------------------

# The outcome variable needs to be a numeric variable for this part of the analysis.
# (because Poisson regression is defined for a count outcome.)
# Therefore, we transform 'seroco' as follows:
hiv.data <- hiv.data %>% 
  mutate(seroco_num = if_else(seroco == "yes", 1, 0))


# Furthermore, the time at risk (before seroconversion) differs between individuals;
# this will be incorporated into the model by an adjustment/denominator term.
# We calculate the denominator ('timeatrisk') for seroco == 0 and seroco == 1:
hiv.data <- hiv.data %>%
  mutate(timeatrisk = as.numeric(if_else
                                 (seroco == "yes", 
                                   (first_pos-first_test)/365.25,
                                   (last_neg-first_test)/365.25)))



## 4.2: Fit the poisson regression model with the variable idu only --------

#create the model
model1 <- glm(seroco_num ~ idu, #seroconversion as dependent variable and idu as independent variable
              family = poisson(link = "log"), #family for poisson regression
              data = hiv.data, 
              offset = log(timeatrisk)) #offset is the log of our time-varying variable
#get the results
summary(model1) 

#for interpretation, we transform the results back from their log scale and add confidence intervals
tidy(model1, exponentiate = TRUE, conf.int = TRUE)



## 4.3: Build multivariate poisson regression models ------------------

# model2 with idu, sex and age2
model2 <- glm(seroco_num ~ idu + sex + age2, 
              family = poisson(link = "log"), 
              data = hiv.data, 
              offset = log(timeatrisk))

tidy(model2, exponentiate = TRUE, conf.int = TRUE)

# model3 with all risk factors
model3 <- glm(seroco_num ~ idu + sex + age2 + sexwork + msm + hete + lasttestY + centro, 
              family = poisson(link = "log"), 
              data = hiv.data, 
              offset=log(timeatrisk))

tidy(model3, exponentiate = TRUE, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") #we remove the intercept to improve visualisation

## Assess the performance of the models
glance(model1)   # performance metrics of model1
glance(model2)   # performance metrics of model2
glance(model3)   # performance metrics of model3


# STEP 5: ASSUMPTIONS OF POISSON REGRESSION MODELS ------------------------


## 5.1: Check for overdispersion -------------------------------------------

check_overdispersion(model1)
check_overdispersion(model2)
check_overdispersion(model3)


# STEP 6: ALTERNATIVES TO POISSON REGRESSION ------------------------------


## 6.1. Negative Binomial Regression ---------------------------------------

#create negative binomial regression model
nb_model <- glm.nb(seroco_num ~ idu + sex + age2 + sexwork + msm + hete + lasttestY + centro +
                     offset(log(timeatrisk)),
                   data = hiv.data)
#results of the model
summary(nb_model)

#exponentiate results and add confidence intervals
tidy(nb_model, exponentiate = TRUE, conf.int = TRUE)


## 6.2: Quasi-Poisson Regression -------------------------------------------

#create the quasi-poisson regression model
quasipois_model <- glm(seroco_num ~ idu + sex + age2 + sexwork + msm + hete + lasttestY + centro,
                       family=quasipoisson,
                       data = hiv.data,
                       offset=log(timeatrisk))
#results of the model
summary(quasipois_model)

#exponentiate results and add confidence intervals
tidy(quasipois_model, exponentiate = TRUE, conf.int = TRUE)

#check performance to obtain AIC
glance(quasipois_model)


# (OPTIONAL) STEP 7: ANALYSIS OF AGGREGATED DATA --------------------------


## 7.1: Creating an aggregated dataframe -----------------------------------

# Step 1: Create all combinations of year, sex, age2, and idu
# This generates a data frame with all possible combinations of the variables
A <- expand_grid(
  year = 1989:2004,                # Years of interest
  sex = levels(hiv.data$sex),      # Levels of the 'sex' variable
  age2 = levels(hiv.data$age2),    # Levels of the 'age2' variable
  idu = levels(hiv.data$idu)       # Levels of the 'idu' variable
)

# Step 2: Aggregate data
# For each combination of year, sex, age2, and idu, calculate:
# - 'atrisk': Number of people at risk for seroconversion in that year
# - 'seroconversions': Number of seroconversions in that year
agg_db <- A %>%
  rowwise() %>% # Process each row independently (needed for custom calculations)
  mutate(
    # Calculate the number of people at risk
    atrisk = sum(
      year(hiv.data$first_test) <= year &  # Tested for the first time on or before the current year
        hiv.data$lasttestY <= year &      # Last test year is on or before the current year
        hiv.data$sex == sex &             # Matches the current 'sex' level
        hiv.data$age2 == age2 &           # Matches the current 'age2' level
        hiv.data$idu == idu               # Matches the current 'idu' level
    ) -
      sum(
        hiv.data$sex == sex &               # Matches the current 'sex' level
          hiv.data$age2 == age2 &           # Matches the current 'age2' level
          hiv.data$idu == idu &             # Matches the current 'idu' level
          hiv.data$first_pos <= year + 1,   # First positive test is before or during the next year
        na.rm = TRUE                        # Ignore missing values
      ),
    
    # Calculate the number of seroconversions
    seroconversions = sum(
      year(hiv.data$first_pos) == year &  # First positive test is in the current year
        hiv.data$sex == sex &             # Matches the current 'sex' level
        hiv.data$age2 == age2 &           # Matches the current 'age2' level
        hiv.data$idu == idu,              # Matches the current 'idu' level
      na.rm = TRUE                        # Ignore missing values
    )
  ) %>%
  ungroup()  %>% 
  
  filter(atrisk > 0) # This step removes combinations where 'atrisk' is 0 or less

# Look at the first rows of this new database
head(agg_db)


## 7.2: Run model on aggregated dataframe ----------------------------------

#create the model
pois_agg_model <- glm(seroconversions ~ sex + age2 + idu,
                family=poisson(link="log"),
                offset = log(atrisk),
                data = agg_db)


# we have a look at the summary of the regression output 
summary(pois_agg_model)

# use 'tidy()' to get risk ratio estimates and confidence intervals.
tidy(pois_agg_model, exponentiate = TRUE, conf.int = TRUE)

# We check for overdispersion:
check_overdispersion(pois_agg_model)

#create the Quasi-Poisson model
qp_agg_model <- glm(seroconversions ~ sex + age2 + idu,
                    family=poisson(link="log"),
                    offset = log(atrisk),
                    data = agg_db)


# we have a look at the Quasi-Poisson regression output 
summary(qp_agg_model)

# use 'tidy()' to get risk ratio estimates and confidence intervals.
tidy(qp_agg_model, exponentiate = TRUE, conf.int = TRUE)



# (OPTIONAL) STEP 8: PREDICT THE NUMBER OF SEROCONVERSIONS IN 2005 --------

# We first load the given data:
db_2005 <- read.table("data/data2005.txt", header=TRUE)
# and check its structure and that it is correctly loaded:
head(db_2005)


# Then we can use 'predict()' to apply a fitted model to these data:
predict(qp_agg_model, db_2005, type="response")

# (Note that type="response" is important here to predict the actual outcome;
# the default is otherwise the log-outcome (for technical reasons). Note also
# that the prediction has decimal values; this is because technically the
# response of the model is actually the *expected* number of events.)

# For convenience, we can attach these predicted values to the dataset:
db_2005 <- db_2005 %>% 
  mutate(pred_seroco = predict(qp_agg_model, db_2005 , type="response"))

# In order to derive a sensible estimate of the overall number of seroconversions
# for 2005, we finally sum up all estimated mean number of events and round the result:
pred_estimate <- round(sum(db_2005$pred_seroco))

# We print the results
pred_estimate
