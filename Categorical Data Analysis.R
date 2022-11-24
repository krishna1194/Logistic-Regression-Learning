# Introduction

install.packages('AmesHousing')
install.packages('tidyverse')
install.packages("MASS")
install.packages("visreg")
install.packages("brglm")
install.packages("car")
install.packages("mgcv")
install.packages("unbalanced")
install.packages("multcomp")
install.packages("rJava")
install.packages("glmulti")
install.packages("givitiR")
install.packages("DescTools")
install.packages("ggplot2")
install.packages("ROCR")
install.packages("InformationValue")
install.packages("brant")
install.packages("VGAM")
install.packages("nnet")

library(AmesHousing)
library(tidyverse)
library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)
library(unbalanced)
library(multcomp)
library(rJava)
library(glmulti)
library(givitiR)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(brant)
library(VGAM)
library(nnet)

# Categorical Data Analysis
library(AmesHousing)
ames <- make_ordinal_ames()
## Converting Target variable from continuous to binary
ames <- ames %>%
  mutate(Bonus = ifelse(Sale_Price > 175000, 1, 0))
## Split data into train and test
set.seed(123)
ames <- ames %>% mutate(id = row_number())
train <- ames %>% sample_frac(0.7)
test <- anti_join(ames, train, by = 'id')
## Basic EDA
table(train$Bonus)
ggplot(data = train) +
  geom_bar(mapping = aes(x = Bonus))
ggplot(data = train) +
  geom_bar(mapping = aes(x = Central_Air))
table(train$Central_Air, train$Bonus)
## The prop.table function allows us to compare two variables in terms of proportions instead of frequencies.
prop.table(table(train$Central_Air, train$Bonus))
ggplot(data = train) +
  geom_bar(mapping = aes(x = Bonus, fill = Central_Air))
## Cross Table
library(gmodels)
CrossTable(train$Central_Air, train$Bonus, prop.chisq = FALSE, expected = TRUE)
### The advantage of the CrossTable function is that we can easily get not only the frequencies, but the cell, row, 
### and column proportions. For example, the third number in each cell gives us the row proportion. For homes without 
### central air, 96.6% of them are not bonus eligible, while 3.4% of them are. For homes with central air, 56.1% of 
### the homes are not bonus eligible, while 43.9% of them are. This would appear that the distribution of bonus 
### eligible homes changes across levels of central air - a relationship between the two variables. This expected 
### relationship needs to be tested statistically for verification. The expected = TRUE option provides an expected 
### cell count for each cell. These expected counts help calculate the tests of association in the next section.

# Tests of Association
### Statistical tests to evaluate relationships between two categorical variables. The null hypothesis for these 
### statistical tests is that the two variables have no association - the distribution of one variable does not change
### across levels of another variable. The alternative hypothesis is an association between the two variables - the 
### distribution of one variable changes across levels of another variable.
### SAMPLE SIZE of two dataset should be same. Else this is not god to compare.
###χ2-dist used and has the following characteristics: Bounded below by 0, Right-skewed, One set of degrees of freedom
library(vcd)
assocstats(table(train$Central_Air, train$Bonus))
### p-value less than 0.05 so, there is some association

# Measures of Association
### Where SAMPLE SIZE of two dataset doesn't matter.
### Measures of association were not designed to test if an association exists, as that is what statistical testing is
### for. They are designed to measure the strength of association
### Odds Ratios (only for comparing two binary variables) 
### Cramer’s V (able to compare nominal variables with any number of categories) 
### Spearman’s Correlation (able to compare ordinal variables with any number of categories)
library(DescTools)
OddsRatio(table(train$Central_Air, train$Bonus))
assocstats(table(train$Central_Air, train$Bonus))

