##### DATA71011 Understanding Data and their Environment
##### Coursework Project
##### MSc Data Science 2024/25
##### Data preprocessing

# -----------------------------------------Libraries------------------------------------
# --------------------------------------------------------------------------------------

library(rio)
library(dplyr)
library(stringr)
library(lubridate)
library(mice)
library(ggplot2)
library(fastDummies)

# -----------------------------------------Import data----------------------------------
# --------------------------------------------------------------------------------------

stores = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/store.csv")
trainData = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/train.csv")
testData = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/test.csv")

# ---------------------------------------Irregularities-----------------------------------------
# ----------------------------------------------------------------------------------------------

#### PromoInterval
stores = stores %>% # White spaces in PromoInterval that are not recognized as NAs.
  mutate(across(everything(), ~ ifelse(str_trim(.) == "", NA, .)))

sum(is.na(stores$PromoInterval)) # 544 missing values


#### New vector to calculate the length of time of nearest competitor existence
reference_date = as.Date("2015-07-31") # Set reference date (last day of collected information)

stores$CompetitionOpenDate = as.Date(paste(stores$CompetitionOpenSinceYear,
                                           stores$CompetitionOpenSinceMonth, "1", sep = "-"))

stores$DiffTimeMonths = interval(stores$CompetitionOpenDate,
                                 reference_date) %/% months(1) # Calculate diff-time:


#### New vector to calculate the length of promo existence
stores$PromoDate = as.Date(paste(stores$Promo2SinceYear, stores$Promo2SinceWeek,
                                 1, sep = "-"), format = "%Y-%U-%u")

stores$DiffPromoTimeMonths = interval(stores$PromoDate, reference_date) %/% months(1)


# ------------------------------Missing data and imputation-----------------------------------
# --------------------------------------------------------------------------------------------

sum(is.na(stores$CompetitionDistance)) # 3 -> Hot Deck imputation
sum(is.na(stores$DiffTimeMonths)) # 354 -> Multiple Imputation
sum(is.na(stores$DiffPromoTimeMonths)) # 544 -> Replace with Zero (no promotion)

#### CompetitionDistance: Hot Deck imputation
stores = stores %>%
  group_by(StoreType, Assortment) %>%
  mutate(
    CompetitionDistance = ifelse(
      is.na(CompetitionDistance),
      median(CompetitionDistance, na.rm = TRUE),  # Replace with group median due to outliers
      CompetitionDistance
    )
  )

#### DiffPromoTimeMonths: Replace with zero (no promotions)
stores$DiffPromoTimeMonths[is.na(stores$DiffPromoTimeMonths)] = 0
sum(is.na(stores$DiffPromoTimeMonths))

#### DiffTimeMonths: Multiple Imputation with MICE
predictors = stores[, c("DiffTimeMonths", "StoreType", "Assortment",
                        "CompetitionDistance", "Promo2")] # Select relevant predictors

# Dummy variables
predictors$StoreType = as.factor(predictors$StoreType)
predictors$Assortment = as.factor(predictors$Assortment)
store_type_dummies = model.matrix(~ StoreType - 1, data = predictors)[, -1]
assortment_dummies = model.matrix(~ Assortment - 1, data = predictors)[, -1]
predictorsDummies = cbind(predictors, store_type_dummies, assortment_dummies)

predictorsDummies = predictorsDummies[-c(2,3)]

# Perform multiple imputation:
imputed_data = mice(predictorsDummies, m = 10, maxit = 50, method = "pmm", seed = 123)
complete_data = complete(imputed_data)

# Back to dataset:
imputed_diff_time_months = complete_data$DiffTimeMonths
stores$DiffTimeMonths[is.na(stores$DiffTimeMonths)] = imputed_diff_time_months[is.na(stores$DiffTimeMonths)]

# Checks:
densityplot(imputed_data)

xyplot(imputed_data, DiffTimeMonths ~ Promo2 + StoreTypeb 
       + StoreTypec + StoreTyped + Assortmentb + Assortmentc, pch=18,cex=1)

xyplot(imputed_data, DiffTimeMonths ~ CompetitionDistance, pch=18,cex=1)


# ---------------------------------Data integration---------------------------------
# ----------------------------------------------------------------------------------

#### Check if we have unique stores in stores.csv
length(unique(stores$Store)) == nrow(stores) #TRUE

# Data unification
data = merge(trainData, stores, by = "Store", all = TRUE)
print(dim(data))

# Only necessary variables:
data = data[,c(1:12,15,19:22)]

# Assigning format:
data$Date = as.Date(data$Date, format = "%d/%m/%Y") # Date for forecasting

data = data %>%
  mutate(StateHoliday = case_when(
    StateHoliday == "0" ~ 1L,
    StateHoliday == "a" ~ 2L,
    StateHoliday == "b" ~ 3L,
    StateHoliday == "c" ~ 4L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

data = data %>%
  mutate(StoreType = case_when(
    StoreType == "a" ~ 1L,
    StoreType == "b" ~ 2L,
    StoreType == "c" ~ 3L,
    StoreType == "d" ~ 4L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

data = data %>%
  mutate(Assortment = case_when(
    Assortment == "a" ~ 1L,
    Assortment == "b" ~ 2L,
    Assortment == "c" ~ 3L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

### Drop irrelevant features for forecasting:

data = data[,-c(14,16)]

data$Set = "Training"

# -------------------------------Test Set Preparation-------------------------------
# ----------------------------------------------------------------------------------

# + DayOfWeek
# + Promo
# + StateHoliday
# + SchoolHoliday
# + StoreType
# + Assortment
# + CompetitionDistance
# + Promo2
# + DiffTimeMonths
# + DiffPromoTimeMonths

compFeatures = stores[,c("Store","StoreType","Assortment","CompetitionDistance","Promo2",
                         "DiffTimeMonths","DiffPromoTimeMonths")]

testData = merge(testData, compFeatures, by = "Store", all.x = T)

# Assigning format:
testData$Date = as.Date(testData$Date, format = "%d/%m/%Y") # Date for forecasting

testData = testData %>%
  mutate(StateHoliday = case_when(
    StateHoliday == "0" ~ 1L,
    StateHoliday == "a" ~ 2L,
    StateHoliday == "b" ~ 3L,
    StateHoliday == "c" ~ 4L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

testData = testData %>%
  mutate(StoreType = case_when(
    StoreType == "a" ~ 1L,
    StoreType == "b" ~ 2L,
    StoreType == "c" ~ 3L,
    StoreType == "d" ~ 4L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

testData = testData %>%
  mutate(Assortment = case_when(
    Assortment == "a" ~ 1L,
    Assortment == "b" ~ 2L,
    Assortment == "c" ~ 3L,
    TRUE ~ NA_integer_  # To handle unexpected values
  ))

testData$Set = "Testing"

finalDATA = rbind(data, testData)

### Dummy variables of categorical predictors

# DayOfWeek
modelDATA = dummy_cols(finalDATA, select_columns = "DayOfWeek",
                       remove_first_dummy = TRUE)
modelDATA = modelDATA[,-c(2)]

#StateHoliday
modelDATA = dummy_cols(modelDATA, select_columns = "StateHoliday",
                       remove_first_dummy = TRUE)
modelDATA = modelDATA[,-c(7)]

# StoreType
modelDATA = dummy_cols(modelDATA, select_columns = "StoreType",
                       remove_first_dummy = TRUE)
modelDATA = modelDATA[,-c(8)]

# Assortment
modelDATA = dummy_cols(modelDATA, select_columns = "Assortment",
                       remove_first_dummy = TRUE)
modelDATA = modelDATA[,-c(8)]

# ----------------------------------------END---------------------------------------
# ----------------------------------------------------------------------------------

