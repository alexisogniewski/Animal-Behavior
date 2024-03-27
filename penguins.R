library(readxl)
library(tidyverse)
library(ggplot2)

#reading in the excel data
penguin_data <- read_excel("inst_penguindata (1).xlsx")

#trying to use anova on the data 
penguin_data |>
  summarise(
    mean_response = mean(number),
    n = n(),
    SE = sd(number)/sqrt(n),
    lower_CI = mean_response - qt(0.975, df = n - 1)*SE,
    upper_CI = mean_response + qt(0.975, df = n - 1)*SE,
    .by = state
  )
#fitting the finding into the anova model.
model <- aov(number~state, data = penguin_data)
summary(model)
