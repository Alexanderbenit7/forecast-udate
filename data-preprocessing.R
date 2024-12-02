##### DATA71011 Understanding Data and their Environment
##### Coursework Project
##### MSc Data Science 2024/25
##### Data preprocessing

# -------------------------Libraries----------------------------
# --------------------------------------------------------------

library(rio)
library(dplyr)
library(stringr)

# -------------------------Import data--------------------------
# --------------------------------------------------------------

stores = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/store.csv")
trainData = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/train.csv")
testData = import("/Users/alexander/Documents/MSc Data Science/Understanding Data and their Environment/Assignment 2/test.csv")

# -----------------------Irregularities-------------------------
# --------------------------------------------------------------

stores = stores %>% # For PromoInterval
  mutate(across(everything(), ~ ifelse(str_trim(.) == "", NA, .)))



# -------------------Missing data and imputation----------------
# --------------------------------------------------------------

# Cols with problems: CompetitionDistance, CompetitionOpenSinceMonth
# CompetitionOpenSinceYear, Promo2SinceWeek, Promo2SinceYear

sum(is.na(stores$CompetitionDistance)) # 3
sum(is.na(stores$CompetitionOpenSinceMonth)) # 354
sum(is.na(stores$CompetitionOpenSinceYear)) # 354
sum(is.na(stores$Promo2SinceWeek)) # 508031
sum(is.na(stores$Promo2SinceYear)) # 508031

# Add a very large number that suggests absence of competition:
stores$HasCompetition = ifelse(is.na(stores$CompetitionDistance), 0, 1) # Dummy for competition
stores$CompetitionDistance[is.na(stores$CompetitionDistance)] = 80000




# -------------------------Data integration---------------------
# --------------------------------------------------------------

# Check if we have unique stores in stores.csv
unique_stores = stores %>% 
  summarise(total_stores = n(),
            unique_stores = n_distinct(Store))

print(unique_stores) # We do: 1115 unique stores

# Data unification
data = merge(trainData, stores, by = "Store", all = TRUE)
print(dim(data))


# --------------- Data aggregation and visualization------------
# --------------------------------------------------------------


# -----------------------Data transformation--------------------
# --------------------------------------------------------------

# -----------------------Feature selection--------------------
# --------------------------------------------------------------



