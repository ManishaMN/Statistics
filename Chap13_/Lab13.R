#lab13

library(lmtest) # needed for the Durbin Watson function
library(gridExtra) # needed to put all graphs in the same plane
library(dplyr)
library(ggplot2)
library(survival)



safety <- read.csv('https://raw.githubusercontent.com/IAA-Faculty/statistical_foundations/master/safety.csv')

summary(safety)

safety$Size <- as.factor(safety$Size)

safety$Unsafe <- as.factor(safety$Unsafe)

sfety_logit2 <- glm(Unsafe ~ Region + Weight + Size,
                   data = safety, family = binomial(link = "logit"))



#Getting the summary of the logistic regression model ( size)
summary(sfety_logit2)

#concordance proportion 0.8482
survival::concordance(sfety_logit2)

#2. (removed weight)

sfety_logit3 <- glm(Unsafe ~ Region  + Size,
                    data = safety, family = binomial(link = "logit"))

summary(sfety_logit3)

#removed region( as size level increases unsafe outcome decreases= as size increases safety increases)


sfety_logit4 <- glm(Unsafe ~ Size,
                    data = safety, family = binomial(link = "logit"))

summary(sfety_logit4)


#concordance for reduced
survival::concordance(sfety_logit4)




library(pROC)

# Step 2: Extract the significant variables from the fitted model
significant_vars <- names(coef(sfety_logit2))[summary(sfety_logit2)$coefficients[, "Pr(>|z|)"] < 0.05]

# Step 3: Create the reduced model using only the significant variables
reduced_model <- update(sfety_logit2, formula = paste("Unsafe ~", paste(significant_vars, collapse = " + ")))

# Step 4: Get predicted probabilities from the reduced model
predicted_probs <- predict(reduced_model, type = "response")

# Step 5: Create a data frame with observed outcomes and predicted probabilities
results_df <- data.frame(Observed = safety$Unsafe, Predicted = predicted_probs)

# Step 6: Calculate the proportion of concordant pairs using the pROC package
roc_results <- roc(results_df$Observed, results_df$Predicted)
concordant_pairs_proportion <- coords(roc_results, "percent")
print(concordant_pairs_proportion)












#summarise(sfety_logit2)

100*(exp(cbind(coef(sfety_logit2), confint(sfety_logit2)))-1)

survival::concordance(sfety_logit2)
