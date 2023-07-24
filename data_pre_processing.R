# processing the data

library(dplyr)
library(tidyr)
library(tidyverse)
library(lme4)

#loading data
raw_data <- readr::read_csv("fMRI_Data.csv")

#checking data structure
str(raw_data)

# checking if columns are double
is.double(raw_data$CA3)

# removing irrelevant sections

mydata <- raw_data|>select(-c(HitorMiss, Ret_OL, DG))



# get descriptive statistics

means_data <- mydata |>
  group_by(Enc_or_Ret) |>
  summarise(
    Mean_CA1 = mean(CA1),
    Mean_CA3 = mean(CA3),
    SD_CA1 = sd(CA1),
    SD_CA3 = sd(CA3)
  )

# Display the means
print(means_data)

# run anova
mydata_anova <- aov(CA1 + CA3 ~ Enc_or_Ret+ Error(PID/Trial), data = mydata)

# Display ANOVA summary
summary(mydata_anova)


# Model for CA1
model_CA1 <- lmer(CA1 ~ Enc_or_Ret + (1 | PID), data = mydata)

summary(model_CA1)

# plotting to check data 
plot(model_CA1)
qqnorm(resid(model_CA1))
qqline(resid(model_CA1))  

# Model for CA3

# trial as a random effect
model_CA3 <- lmer(CA3 ~ Enc_or_Ret + (1 | PID), data = mydata)

summary(model_CA3)

#plotting to check data
plot(model_CA3)
qqnorm(resid(model_CA3))
qqline(resid(model_CA3))
