# Rare Event Sampling
### The typical cut-off to decide if their event is rare is 5%. 
### If event occurs less than 5% of the time, we adjust modeling to account for this.

library(knitr)
library(readr)

churn <- read.csv("C:/Users/krish/Desktop/IAA/Fall I/AA 502/Logistic Regression - Dr. Aric LaBarr/Code/Logistic Regression Data Sets/tele_churn.csv")
## Target variable frequency
table(churn$churn)
## Target variable proportion
prop.table(table(churn$churn))
## set the id for the data
churn$id <- 1:length(churn$churn)

# UNDERSAMPLING
set.seed(12345)
train_u <- churn %>%
  group_by(churn) %>%
  sample_n(104)
test_u <- churn[-train_u$id,]
table(train_u$churn)
table(test_u$churn)

# OVERSAMPLING
## 1. Sample_frac function to isolate down to 70% of the observations
## 2. To isolate the churners to replicate them: filter function with churn equal to TRUE
## 3. Slice and rep functions to repeat each of the observations 10 times (set by the each = option)
## 4. Isolate non-churners, with filter function to blend two sets together
## 5. rbind function to combine non-churners and the replicated churners together

set.seed(12345)
train_o <- churn %>%
  sample_frac(0.70)
train_o_T <- train_o %>%
  filter(churn == TRUE) %>%
  slice(rep(1:n(), each = 10))
train_o_F <- train_o %>%
  filter(churn == FALSE)
train_o <- rbind(train_o_F, train_o_T)
test_o <- churn[-train_o$id,]
table(train_o$churn)
table(test_o$churn)

# Two typical ways to adjust models from over or undersampling - adjusting the intercept and weighting.
## 1. Adjusting the Intercept
### The intercept is the “baseline” probability in model which is too high. Adjust predicted probabilities 
### from the regression model to account for this incorrect intercept.
### We add the equation to the predicted probabilities
logit.model <- glm(churn ~ factor(international.plan) +
                     factor(voice.mail.plan) +
                     total.day.charge +
                     customer.service.calls,
                   data = train_u, family = binomial(link = "logit"))
summary(logit.model)
test_u_p_bias <- predict(logit.model, newdata = test_u, type = "response")
test_u_p <- (test_u_p_bias*(104/208)*(154/3004))/((1-test_u_p_bias)*(104/208)*(2850/3004)+test_u_p_bias*(104/208)*(154/3004))
test_u <- data.frame(test_u, 'Pred' = test_u_p)
head(test_u_p)
### We store these as a new column in our validation data set.

## 2. Weighted Observations
### Weighted observations use weighted MLE instead of plain MLE since each observation doesn’t have the same weight 
### in the estimation of the parameters for our model.
### A new weight variable in our training data with the ifelse function. If our churn variable takes a value of TRUE 
### then that observation has a weight of 1, while the other observations have a weight
train_u$weight <- ifelse(train_u$churn == 'TRUE', 1, 18.49)
logit.model.w <- glm(churn ~ factor(international.plan) +
                       factor(voice.mail.plan) +
                       total.day.charge +
                       customer.service.calls,
                     data = train_u, family = binomial(link = "logit"),
                     weights = weight)
summary(logit.model.w)
## Predict
test_u_p_w <- predict(logit.model.w, newdata = test_u, type = "response")
test_u <- data.frame(test_u, 'Pred_w' = test_u_p_w)
head(test_u_p_w)
## Which one to use?
### The more common approach is the weighted observation approach. Empirical simulation studies have proven that for 
### large sample sizes (n > 1000), the weighted approach is better. In small sample sizes (n < 1000), the adjusted 
### intercept approach is only better when you correctly specify the model. In other words, if you plan on doing 
### variable selection because you are unsure if you have the correct variables in your model, then your model may be 
### misspecified in its current form until after your perform variable selection. That being the case, it is probably 
### safer to use the weighted approach. This is why most people use the weighted approach.


# Convergence Problems
## 1. Complete linear separation occurs when some combination of the predictors perfectly predict every outcome
## 2. Quasi-complete separation occurs when the outcome can be perfectly predicted for only a subset of the data
table(train_u$customer.service.calls, train_u$churn)
### There is quasi-complete output with perfect churn at service calls 6 and 7.
logit.model.w <- glm(churn ~ factor(international.plan) +
                       factor(voice.mail.plan) +
                       total.day.charge +
                       factor(customer.service.calls),
                     data = train_u, family = binomial(link = "logit"),
                     weights = weight)
summary(logit.model.w)
### The parameter estimates are high for category 6 and 7. 
### ORDINAL: Merge the data with data we have as "4+"
### NOMINAL: Merge with Least amount information lost
train_u$customer.service.calls.c <- as.character(train_u$customer.service.calls)
train_u$customer.service.calls.c[which(train_u$customer.service.calls > 3)] <- "4+"
table(train_u$customer.service.calls.c, train_u$churn)
logit.model.w <- glm(churn ~ factor(international.plan) +
                       factor(voice.mail.plan) +
                       total.day.charge +
                       factor(customer.service.calls.c),
                     data = train_u, family = binomial(link = "logit"),
                     weights = weight)
summary(logit.model.w)