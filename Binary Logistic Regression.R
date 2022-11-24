# Binary Logistic Regression Model
## Maximum likelihood estimation
### It is a very popular technique for estimating statistical models. It uses the assumed distribution ( logistic) to 
### find the “most likely” values of the parameters to produce the data we see. In fact, mathematically 
### that the OLS solution in linear regression is the same as the MLE for linear regression. The likelihood function 
### measures how probable a specific grid of β values is to have produced your data, so we want to MAXIMIZE that


library(AmesHousing)
library(tidyverse)
library(MASS)
library(visreg)
library(brglm)
library(car)
library(mgcv)
#library(unbalanced)
library(multcomp)
#library(rJava)
#library(glmulti)
library(givitiR)
library(DescTools)
library(ggplot2)
library(ROCR)
library(InformationValue)
library(brant)
library(VGAM)
library(nnet)

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
## Model
logit.model <- glm(Bonus ~ Gr_Liv_Area + factor(Central_Air),data=train, family=binomial(link="logit"))
summary(logit.model)
### All variables are significant but we will perform GLOBAL LIKELIHOOD Ratio test; check any variable significant
logit.model.r <- glm(Bonus~1, data=train, family = binomial(link = "logit"))
summary(logit.model.r)
## Use ANOVA to check if there is a significant difference present in having these variables or not
## Likelihood Ratio Test (LRT) on this model compared to the original model
anova(logit.model,logit.model.r,test = 'LRT')
## Odds ratio
exp(cbind(coef(logit.model), confint(logit.model)))


# Testing Assumptions
### Apart from multicolinearity as an assumption. Continuous variables should be linearly related to logit function
### We use Generalized Additive Models (GAMs) to check this assumption
### The function applied to continuous variables should be estimated and GAM uses spline functions.
### If these splines say a straight line is good, then the assumption is met. 
### There are some options if the assumption is not met:
### 1.Use the GAM representation of the logistic regression model instead of the traditional logistic regression model
####    The GAM version of the logistic regression is less interpretable in the traditional sense of odds ratios. 
####    Instead, plots are used to show potentially complicated relationships.
### 2.Strategically bin the continuous variable.
####    The continuous variables are categorized and put in the original logistic framework in their categorical form
####    one could also use the GAM plots to decide on possible splits for binning the data

fit.gam <- gam(Bonus ~ s(Gr_Liv_Area) + factor(Central_Air),
               data = train, family = binomial(link = 'logit'),
               method = 'REML')
summary(fit.gam)
plot(fit.gam)
### If the edf value was 1, the best spline was a linear relationship with the target (linearity assumption holds)
### How close is close enough to 1? 
####  Chi-square statistical test, compare a model with a linear representation to the spline version.
anova(logit.model, fit.gam, test="Chisq")
### H0 is that the two models are equal. 
#### This imply that the linearity assumption would hold since the spline is not estimating anything more complicated
### p-value is small so, we reject H0. So, we cannot use logit directly, instead we will use spline.
#### We can also create bins and work with that.
train <- train %>% mutate(Gr_Liv_Area_BIN = cut(Gr_Liv_Area, breaks = c(-Inf,1000,1500,3000,4500,Inf)))
### Rebuilt the logistic regression with binned values
logit.model.bin <- glm(Bonus ~ factor(Gr_Liv_Area_BIN) + factor(Central_Air),
                       data = train, family = binomial(link = 'logit'))
summary(logit.model.bin)
### The categorical representation for the variable instead of using the GAM for predictions is that:
####  we still have some interpretability on this variable using odds ratios for the categories.


# Predicted Values
### The vigreg function below creates the effect plot, which will show us the logistic curve across different values 
### of the continuous variable, here Gr_Liv_Area. It will show each of the categorical variable categories as a 
### separate curve. If there is were more continuous variables they are just taken at a specified value - their 
### average value.
new_ames <- data.frame(Gr_Liv_Area = c(1500, 2000, 2250, 2500, 3500),
                       Central_Air = c("N", "Y", "Y", "N", "Y"))
new_ames <- data.frame(new_ames, 'Pred' = predict(logit.model, newdata = new_ames, type = "response"))
print(new_ames)
visreg(logit.model, "Gr_Liv_Area", by = "Central_Air", scale = "response",
       overlay = TRUE,
       xlab = 'Greater Living Area (Sqft)',
       ylab = 'Bonus Eligibility')
### The print function above looks at the scored data set.This is the predicted probability of a 1.It's default in R.
### If you want the predicted probability of the 0 category, you can just subtract this prediction from 1.