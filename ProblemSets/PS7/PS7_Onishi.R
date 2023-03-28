library(tidyverse)
library(modelsummary)
library(mice)
wages = read.csv("/DScourseS23/ProblemSets/PS7/wages.csv", 
                 header=T)
head(wages)
wages_2.0 = wages %>% drop_na(c(hgc,tenure))

# data summaries
datasummary_skim(wages)

# Listwise Deletion
complete_data = wages_2.0 %>% drop_na(logwage)
mod <- list()
mod[['Complete Cases Only']] = lm(logwage ~ hgc + college + tenure + (tenure^2) + age + married, data = complete_data)

# Mean Imputation
mean_data = wages_2.0
mean_data$logwage[is.na(mean_data$logwage)] = mean(mean_data$logwage, na.rm = TRUE)
mod[['Mean Imputation']] = lm(logwage ~ hgc + college + tenure + (tenure^2) + age + married, data = mean_data)

# Imputation via prediction
predicted_data = wages_2.0
predicted_data$logwage[is.na(predicted_data$logwage)] = predict(mod[[1]], newdata = predicted_data[is.na(predicted_data$logwage),])
mod[['Predicted from Compete Cases Model']] = lm(logwage ~ hgc + college + tenure + (tenure^2) + age + married, data = predicted_data)

# Multiple imputation regression model
mice_data = mice(wages_2.0, m = 5, printFlag = FALSE)
mod[['Mice']] <- with(mice_data, lm(logwage ~ hgc + college + tenure + (tenure^2) + age + married))
mod[['Mice']] <- mice::pool(mod[['Mice']])

modelsummary(mod)
