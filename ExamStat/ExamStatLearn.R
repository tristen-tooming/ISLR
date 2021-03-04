## ----setup, include=FALSE---------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------
lake = read.csv("lakesurvey.csv", header = T, sep = ' ')
str(lake)

## ----Data-Exploration, warning=FALSE, fig.width=10, fig.height=10-----------------------------
library(ggcorrplot)

corr = round(cor(lake), 1)
ggcorrplot(corr, outline.color = "white",lab = T,lab_size =4)


## ----Best-Subset-Selection, cache=T, message=F------------------------------------------------
library(ISLR)
library(leaps)
# Fit
# Showing only best (nbest = 1)
regfit.models <- regsubsets(pH~., data = lake, nbest = 1, nvmax = ncol(lake))

# Summary, Cp, BIC
res.sum <- summary(regfit.models)
as.data.frame(res.sum$outmat)

plot(regfit.models, scale='Cp')
plot(regfit.models, scale='bic')


## ----Best-Subset-Selection-Results, echo=FALSE, results='hold', message=F---------------------
regfit.sum.min.cp = which.min(res.sum$cp)
regfit.sum.min.cp.value = min(res.sum$cp)
cat(sprintf('Model with best Cp (%.2f) model: %s \n', regfit.sum.min.cp.value, regfit.sum.min.cp))
cat(sprintf('Model: pH ~ %s\n\n',
        paste(names(which(res.sum$which[regfit.sum.min.cp, ] == TRUE)),
              collapse = ' + ')))



regfit.sum.min.bic = which.min(res.sum$bic)
regfit.sum.min.bic.value = min(res.sum$bic)
cat(sprintf('Model with best BIC (%.2f) model: %s \n', regfit.sum.min.bic.value, regfit.sum.min.bic))
cat(sprintf('Model: pH ~ %s\n\n',
            paste(names(which(res.sum$which[regfit.sum.min.bic, ] == TRUE)),
                  collapse = ' + ')))


## ---- echo = FALSE, echo=F, message=F---------------------------------------------------------
library(caret)
library(pls)
library(caTools)
library(MASS)
library(tidyverse)


## ----Lasso, warning=FALSE, results='hold', cache=T, message=F---------------------------------
parameters <- c(seq(0.01, 0.1, by = 0.01))

model_lasso <- train(
  pH~., data = lake, method = "glmnet",
  scale = TRUE, # scale = TRUE for standardizing the variables to make them comparable.
  trControl = trainControl("LOOCV", number = 500),
  tuneGrid = expand.grid(alpha = 1,  lambda = parameters),
  metric = 'RMSE'
  )

## ----Lasso-Results, results='hold', max.print=50----------------------------------------------
plot(model_lasso, main = 'Performance of Lasso')

options(max.print=1000000)
coef(model_lasso$finalModel)
cat(sprintf('Best Lasso Model'))
cat(sprintf('Model: pH ~ %s\n\n',
            paste(model_lasso$finalModel$xNames,
                  collapse = ' + ')))


## ----Lasso-MSE--------------------------------------------------------------------------------
# Best Lasso MSE
lasso_mse = min(model_lasso$results$RMSE ** 2)
cat(sprintf("\nLasso MSE: %.3f \n\n", lasso_mse))


## ----PLS, cache=T, results='hold'-------------------------------------------------------------
model_pls <- train(
  pH~., data = lake, method = "pls",
  scale = TRUE, # scale = TRUE for standardizing the variables to make them comparable.
  trControl = trainControl("LOOCV", number = 500),
  tuneLength = 50
  )

## ----PLS-Results, results='hold'--------------------------------------------------------------
# Plot model RMSE vs different values of components
plot(model_pls, main = 'Performance of PLS')
# minimize the cross-validation error, RMSE
pls_mse = min(model_pls$results$RMSE ** 2)
cat(sprintf("\nPLS MSE: %.3f \n\n", pls_mse))


## ----Concrete---------------------------------------------------------------------------------
library(readxl)
concrete<- read_excel("Concrete_Data_2.xls")
col_names_ = c('Cement', 'Age', 'Strength')
colnames(concrete) = col_names_
str(concrete)
summary(concrete)

anyNA.data.frame(concrete)


## ----GAM-Spline, cache=TRUE, message=FALSE----------------------------------------------------
library(gam)
library(caret)
# Build the GAM model based on splines and find best model with LOOCV
fit_gam_spline <- train(
  Strength~., data = concrete, method = "gamSpline",
  scale = FALSE,
  trControl = trainControl("LOOCV", number = 1),
  tuneGrid = expand.grid(df = c(75, 100, 125)))


## ----GAM-spline-Results, results='hold'-------------------------------------------------------
fit_gam_spline
par(mfrow = c(1, 2))
plot(fit_gam_spline$finalModel)
cat(sprintf('GAM Split best model RMSE: %.4g', min(fit_gam_spline$results$RMSE)))


## ----GAM-Loess, warning=F, cache=TRUE, warning=FALSE------------------------------------------
# Build the GAM model based on splines and find best model with LOOCV
library(gam)
library(dplyr)
seq_lenght = 6

#fit_gam_loess = lo(Strength~., data = concrete, span = 0.3)

#fit_gam_loess <- train(
#  Strenght~., data=concrete, method = "gamLoess",
#  trControl = trainControl("LOOCV", number = 1),
#  tuneGrid = expand.grid(span = seq(0.1, 0.6, length = seq_lenght),
#                         degree = c(rep(1,seq_lenght))))

## ----GAM-Loess-Results, results='hold'--------------------------------------------------------
#fit_gam_loess
#par(mfrow = c(1,2))
#plot(fit_gam_loess$finalModel)
#cat(sprintf('GAM Loess best model RMSE: %.4g', min(fit_gam_loess$results$RMSE)))


## ----TPS, cache=F, message=FALSE, results='hold'----------------------------------------------
library(fields)

concrete_x = subset(concrete, select = -c(Strength))
concrete_y = concrete$Strength

tpsfit <- Tps(concrete_x, concrete_y, scale.type="unscaled", GCV = TRUE)

## ----TPS-results------------------------------------------------------------------------------
summary(tpsfit)
par(mfrow = c(2,2))
plot(tpsfit, main = 'Tps diagnostics')

par(mfrow = c(1,1))
surface(tpsfit, main = 'GCV surface of the Tps')

look = predict(tpsfit)
tps_mse = sqrt(mean((concrete_y - look) ** 2))
cat(sprintf("TPS MSE: %.3f", tps_mse))


## ----3-data-----------------------------------------------------------------------------------
olive = read.csv('olive.csv', header = T)
olive$Region = as.factor(olive$Region)
olive_sub = subset(olive, select = -c(Area, Sample))
str(olive_sub)
summary(olive_sub)


## ----3-Corr-Na, cache=T, warning=FALSE, fig.width=10, fig.height=10, message=F, results='hold'----
library(ggcorrplot)

# Scaling
olive_sub[, -1] = scale(olive_sub[, -1])
cat(sprintf("\n Is there NA values?: %s \n", any(is.na(olive_sub))))

corr = round(cor(olive_sub[, -1]), 1)
ggcorrplot(corr, outline.color = "white",lab = T,lab_size =4)

# Train / Test
smp_size <- floor(0.70 * nrow(olive_sub))
set.seed(2707)
train_ind <- sample(seq_len(nrow(olive_sub)), size = smp_size)

train <- olive_sub[train_ind, ]
test <- olive_sub[-train_ind, ]

summary(olive_sub)


## ----LDA, cache=T-----------------------------------------------------------------------------
library(MASS)
olive_lda = lda(Region~., data = train)
lda_predict = predict(olive_lda, test)

lda_acc = sum(test$Region == lda_predict$class)/length(test$Region) * 100

cat(sprintf("\nLDA Acc: %.2f\n\n", lda_acc))

n = length(olive_sub)
p.x = seq(from = min(lda_predict$x[,1]), to = max(lda_predict$x[,1]), length.out = n)
p.y = seq(from = min(lda_predict$x[,2]), to = max(lda_predict$x[,2]), length.out = n)

# notice I don't use t.lda for first variable
plot(olive_lda, panel = function(x, y, ...) { points(x, y, ...) },
     col = c(4,2,3)[factor(olive_sub$Region)], 
     pch = c(17,19,15)[factor(olive_sub$Region)],
     ylim=c(-10,10), xlim=c(-10,10))

contour(x = p.x, y = p.y, z = matrix(as.numeric(lda_predict$class), nrow = n, ncol = n), 
        levels = c(1, 2, 3), add = TRUE, drawlabels = FALSE)


## ----Random-Forest, cache=T-------------------------------------------------------------------
library(caret)
library(randomForest)

control <- trainControl(method='LOOCV', 
                        number=500)

str(train)
#Number randomely variable selected is mtry
mtry <- c(3, 5, 7)
tunegrid <- expand.grid(.mtry=mtry)
rf_model <- train(Region~., 
                  data=train,
                  importance = TRUE,
                  metric = "Accuracy",
                  tuneGrid=tunegrid,
                  trControl=control,
                  nodesize=3)

## ----LDA-result, results='hold'---------------------------------------------------------------
rf_model$finalModel$importance
plot(rf_model)

rf_predict = predict(rf_model, test)

rf_acc = sum(test$Region == rf_predict)/length(test$Region) * 100
cat(sprintf("\nLDA Acc (on test data): %.2f\n\n", rf_acc))

