# Summer Assessment Cheat Sheet
# Manisha

library(car); library(tidyverse); library(stats)
library(DescTools); library(AppliedPredictiveModeling)
library(nortest); library(lmtest); library(glmnet)
library(gmodels); library(vcdExtra); library(survival)





##### T-TESTS (comparing means) # lab(s) 2
# 1. Check normality of data
# ggplot(data = DATAFRAME, aes(x = FEATURE OF INTEREST)) +
#        stat_qq() +
#        stat_qq_line()
# shapiro.test(FEATURE OF INTEREST)
# shapiro Ho: distribution is normal
# * if failed, step 3

# 1b. If two-sample, check for equal variance:
# var.test(FEATURE 1 ~ FACTOR(FEATURE 2), data = DATAFRAME)
# var.test Ho: distributions have same variance
# * if failed, step 3

# 2a. One Sample T-Test
# t.test(SAMPLE, mu = MEAN UNDER Ho, alpha = 0.05)
# t.test Ho: sample mean = mu. 

# 2b. Two Sample T-Test
# t.test(SAMPLE ~ CATEGORY, data = DATAFRAME)
# t.test Ho: SAMPLE mean = SAMPLE mean between CATEGORY

# 3. One or both assumptions failed
# wilcox.test((FEATURE 1 ~ FACTOR(FEATURE 2), data = DATAFRAME, alpha = 0.05)
# IF normality failed but same variance: Ha: different medians
# IF equal variance failed: Ha: different location (distributional dominance)


##### ANOVA: # lab(s) 3, 5
# 1a. Check normality of data
# ggplot(data = DATAFRAME, aes(sample = FEATURE OF INTEREST, color = factor(GROUPING FEATURE))) +
#        stat_qq() +
#        stat_qq_line()
# shapiro.test(FEATURE OF INTEREST)
# shapiro Ho: distribution is normal

# 1b. Check equal variance assumption
# leveneTest(FEATURE OF INTEREST ~ factor(GROUPING FEATURE), data = DATAFRAME)
# levene Ho: equal variance

# 2a. If assumptions met, create ANOVA model
# anova(lm(FEATURE OF INTEREST ~ factor(GROUPING FEATURE), data = DATAFRAME))

# 2b. If normal but unequal variance
# oneway.test(FEATURE OF INTEREST ~ factor(GROUPING FEATURE), data = DATAFRAME,
#             var.equal = F)
# Welch Ho: xbar1 = xbar2

# 2c. If normality fails
# kruskal.test(FEATURE OF INTEREST ~ factor(GROUPING FEATURE), data = DATAFRAME)
# kruskal interpretation:
# IF: equal shape and var: Ho: median1 = median2
# ELSE: Ho: location1 = location2 (no difference in distributional dominance)

# 3a. Post-Hoc against control group
# DunnettTest(x = FEATURE OF INTEREST, g = GROUPING FEATURE,
#             control = 'LEVEL OF GROUPING FEATURE')

# 3b. Post-Hoc of all possible pairs
# print(TukeyHSD(aov(FEATURE OF INTEREST ~ factor(GROUPING FEATURE), data = DATAFRAME)))

# SLICED ANOVA
# sliced_model = data %>%
#                   group_by(FEATURE TO SLICE) %>%
#                   nest() %>%
#                   mutate(aov = map(data, ~summary(aov(FEATURE OF INTEREST ~ factor(GROUPING FEATURE),
#                                                       data = .x))))
# ex.
# drug_aov_s <- drugs %>%
#   group_by(Disease) %>%
#   nest() %>%
#   mutate(aov = map(data, ~summary(aov(BloodP ~ factor(DrugDose), 
#                                       data = .x))))




##### For REGRESSION # lab(s) 4,6,7,8,9,10,11
# 1a. Check if there is a correlation
# cor.test(PREDICTOR1, PREDICTOR2)
# cor.test Ho: no correlation

# 1b. Check correlation matrix between predictors
# corMatrix = cor(DATAFRAME[,c('PREDICTOR1', 'PREDICTOR2',...)])
# pairs(corMatrix)
# 2. Check normality & variance of residuals
# model = lm(TARGET ~ PREDICTOR1 + PREDICTOR2..., data = DATAFRAME)
# par(mfrow = c(2,2))
# plot(model)
# shapiro.test(model$residuals) # check normality of resids

# 2. Check results
# summary(model) # shows global F test, r^2, adj r^2 
# can run ANOVA on MLR with car::Anova(model)

# POLYNOMIAL regression
# model = lm(TARGET ~ PREDICTOR + I(PREDICTOR^2), data = DATAFRAME)

# 3. Diagnostics
# check residual plots
# ggplot(model, aes(x = PREDICTOR, y = resid(model))) +
#               geom_point() 
# * this plot should have a 'band' of values
# residuals are assumed to have homoscedasticity (constant variance)
# can check visually or with cor test
# ggplot(model, aes(x = fitted(model), y = resid(model))) +
#           geom_point()
# cor.test(abs(resid(model)), fitted.values(model), method = 'spearman', exact = T)
# Ho: r ~ 0 aka has homoscedasticity
# *if it's looking like crap, can try logxform, same process but change model to:
# model = lm(log(TARGET)~PREDICTOR)

# check normality of residuals
# ggplot(model, aes(x = model$residuals)) +
#        geom_histogram()
# qqnorm(resid(model)); qqline(resid(model))
# * if data is non-normal, can try box-cox xform
# boxcox(model). then check for normality again

# check "independence" of residuals
# time = seq(1, number of obs, by = 1)
# time_model = lm(model$residuals ~ time)
# dwtest(time_model, alternative = 'greater')
# Ho: no autocorrelation

# ID'ing outliers and influential obs:
# Outliers:
# Studentized resids:
# which(abs(rstudent(model_time)) > cutoff) of interest. 2 for small dataset, 3 for large

# Influential obs:
# Cook's D:
# cutoff: value > 4/(nrow(DATAFRAME) - model$rank - 1)
# ggplot(model_time, aes(x = time, y = cooks.distance(model))) +
#         geom_point(color = 'orange') +
#         geom_line(y = cutoff)

# both plots at same time
# ggplot(model, aes(x = hatvalues(model), y = rstudent(model))) +
#         geom_point() + geom_hline(yintercept = 3) +
#         geom_vline(xintercept=HATCUTOFF) +
#         labs(x = 'Hat Values, y = 'Student.Residuals)

# *see lab 9 & Diagnostics notes for each type of test

# Collinearity
# vif(model) 
# anything > 10 is suspect, > 5 if using GVIF

# can drop variables based on common sense (redundant) or
# use biased regression or
# if using polynomial or interaction term, center independent vars:
# scale(data, scale = F)

# Regularized Regression
# 1. select your vars:
# train_reg = DATAFRAME %>%
#               dplyr::select(Variable1, Variable2,...) %>%
#               replace(is.na(.), 0)
# train_x = model.matrix(TARGET ~ ., data = train_reg)[,-1]
# train_y = train_reg$TARGET

# ridge_model = glmnet(x = train_x, y = train_y, alpha = 0) cannot remove vars
# lasso_model = glmnet(x = train_x, y = train_y, alpha = 1)  can remove vars
# net_model = glmnet(x = train_x, y = train_y, alpha = .5) can remove vars
# plot(ridge_model, xvar = 'lambda') look at variables
# can do cross-validation version to avoid overfitting
# lasso_cv_model = cv.glmnet(x = train_x, y = train_y, alpha = 1)
# plot(lasso_cv_model)
# plot(lasso_cv_model, xvar = 'lambda')
# abline(v = log(lasso_cv_model$lambda.min), col = 'red')
# to look at variables 
# coef(lasso_cv_model, s = lasso_cv_model$lambda$1se)

# to compare predictions between models:
# reg lm object:
# modelpred = predict(model, newdata = testset)
# lasso etc object:
# lasso_pred = predict(lasso_model, s = lasso_model$lambda.1se, newx = testsetmatrix)
# * need to make design matrix for test set:
# train.x = model.matrix(TARGET ~ ., data = TESTSET)[,-1]


##### MODEL SELECTION # lab(s) 7
# AIC k = 2; BIC k = log(nrow(DATAFRAME))
# Empty model: lm(TARGET ~ 1, data = DATAFRAME)
# Full model: lm(TARGET ~., data = DATAFRAME)

# Forward select:
# forward_model = step(empty_model,
#                      scope = list(lower=empty_model,
#                                   upper=full_model),
#                      direction = 'forward', k = 2)
 
# backward_model = step(full_model,
#                       scope = list(lower = empty.model, upper = full.model),
#                       direction = 'backward', k = 2)

# stepwise:
# stepwise_model = step(empty.model,
#                      scope = list(lower = empty.model, upper = full.model,
#                      direction = 'both', k = 2))

# Compare models:
# AIC(MODELNAME)


##### Categorical Variables and Logistic Regression # lab(s) 12, 13
# to view crosstable of two categorical vars:
# CrossTable(VAR1, VAR2)
# need > 80% of cells to have exp count > 5

# Tests to show association exists

# Pearson X^2 test for association
# chisq.test(table(VAR1, VAR2))
# Ho: no association

# if we dont meet 80% of cells exp count > 5:
#fisher.test(table(VAR1, VAR2))

# if we are comparing two ordinal vars:
# Mantel-Haenszel X^2 test
# CMHtest(table(VAR1, VAR2))$table[1,]

# To measure association:
# Cramer's V
# assocstats(table(VAR1, VAR2))
# result is correlation between VAR1 and VAR2. 

# Spearman's correlation
# cor.test(x = as.numeric(ordered(VAR1)).
#          y = as.numeric(ordered(VAR2)),
#          method = 'spearman')

# Logistic regression assumptions:
# independence of obs
# logit is linearly related to variables aka:
# predictor -> logit(prediction) is linear

# factor appropriate vars and then
# lrmodel = glm(TARGET ~ PREDICTOR1 + PREDICTOR2 +...,
#             data = DATAFRAME,
#             family = binomial(link = 'logit))
# summary(lrmodel) will show AIC for model comparison

# survival::concordance(lrmodel)
# # concordant = correctly guessed pairs, discordant = wrong about pairs

# Forward and backward selection work with LR:
# train_log = train %>%
#               dplyr::select(Var1, Var2,...) %>%
#               replace(is.na(.), 0)

# Empty model: glm(TARGET ~ 1, data = DATAFRAME)
# Full model:  glm(TARGET ~., data = DATAFRAME)

# forward_model = step(empty_model,
#                      scope = list(lower=empty_model,
#                                   upper=full_model),
#                      direction = 'forward',
#                      k = log(dim(train_log)[1]))



