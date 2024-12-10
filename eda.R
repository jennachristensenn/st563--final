library(tidyverse)

# reading in the data
diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")


diabetes_data <- diabetes_data %>%
  filter(BMI < 50)

# converting into factors
diab_data <- diabetes_data |>
  mutate(diab_bin = factor(Diabetes_binary, levels = c(0, 1),
                           labels = c("no diabetes", "diabetes")),
         highBP = factor(HighBP, levels = c(0, 1), 
                         labels = c("no", "yes")),
         highCol = factor(HighChol, levels = c(0, 1), 
                          labels = c("No", "Yes")),
         colCheck = factor(CholCheck, levels = c(0, 1),
                           labels = c("No", "Yes")),
         heartDA = factor(HeartDiseaseorAttack, levels = c(0, 1), 
                          labels = c("No", "Yes")),
         heavyAlc = factor(HvyAlcoholConsump, levels = c(0, 1),
                           labels = c("No", "Yes")),
         genHlth = factor(GenHlth, levels  = c(1, 2, 3, 4, 5), 
                          labels = c("excellent", "very good", "good", "fair", "poor")),
         sex = factor(Sex, levels = c(0, 1), 
                      labels = c("female", "male"))) |>
  select(diab_bin, highBP, highCol, colCheck, heartDA, heavyAlc, genHlth, sex, Age) 
diab_data



# finding na values
sum_na <- function(column){
  sum(is.na(column))
}
na_counts <- diab_data |>
  summarize(across(everything(), sum_na))
na_counts


# frequency table for response variable
diab_ct <- table(diab_data$diab_bin)
diab_ct

# frequency table for response variable and general health
diab_genHlth <- table(diab_data$diab_bin, diab_data$genHlth)
diab_genHlth

# frequency table for response variable and sex
diab_sex <- table(diab_data$diab_bin, diab_data$sex)
diab_sex

# frequency table for response variable and highCol
diab_highCol <- table(diab_data$diab_bin, diab_data$highCol)
diab_highCol

ggplot(diab_data, aes(x = diab_bin)) +
  geom_bar(aes(color = genHlth)) +
  facet_wrap(~sex)