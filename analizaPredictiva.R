#PENTRU A PUTEA FACE LOAD LA SETUL DE DATE VA TREBUI SĂ EXECUȚATI
#CODUL DIN FISIERUL analizaDescriptiva.R
#DUPĂ EXECUTAREA CODULUI SE VA GENERA SETUL DE DATE "swiss_credit_dataset_final.csv"

credit.dataset <- read.csv("swiss_credit_dataset_final.csv")

str(credit.dataset)

# transformarea - factoring a datelor
to.factors <- function(dataset, variables){
  for (variable in variables){
    dataset[[variable]] <- as.factor(dataset[[variable]])
  }
  return(dataset)
}

# normalizare
scale.features <- function(dataset, variables){
  for (variable in variables){
    dataset[[variable]] <- scale(dataset[[variable]], center=T, scale=T)
  }
  return(dataset)
}

numeric.vars<- c("credit.duration.months","age","credit.amount")

credit.dataset<- scale.features(credit.dataset,numeric.vars)

#selectarea datelor pe care vrem să le transformăm
categorical.vars <- c('credit.rating', 'account.balance', 
                      'previous.credit.payment.status',
                      'credit.purpose', 'savings',
                      'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 
                      'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type',
                      'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')

credit.dataset<- to.factors(dataset=credit.dataset,variables = categorical.vars)

indexes<-sample(1:nrow(credit.dataset),size= 0.6*nrow(credit.dataset))

train.data<-credit.dataset[indexes,]

test.data<- credit.dataset[-indexes,]

## Feature selection

library(caret)
library(randomForest)

run.feature.selection <- function(num.iters=20, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv",
                        verbose = FALSE, returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var,
                     sizes = variable.sizes,
                     rfeControl = control)
  return(results.rfe)
}
rfe.results <- run.feature.selection(feature.vars=train.data[,-1], 
                                     class.var=train.data[,1])

rfe.results

varImp(rfe.results)

## Modeling using logistic regression

library(caret)
library(ROCR)

source("performance_plot_utils.R")

test.feature.vars<-test.data[,-1]
test.class.var<- test.data[,1]

formula.init<- "credit.rating ~ ."
formula.init<- as.formula(formula.init)

lr.model<- glm(formula = formula.init,data = train.data,
               family = "binomial")

# view model details
summary(lr.model)

lr.predictions<- predict(lr.model,test.data, type="response")

lr.predictions<- round(lr.predictions)

u=union(lr.predictions,test.class.var)

confusionMatrix(table(factor(lr.predictions,u),factor(test.class.var,u)))


formula<- "credit.rating ~ ."

formula<-as.formula(formula)

control<-trainControl(method = "repeatedcv", number = 10, repeats = 2)

model<- train(formula, data=train.data, method="glm",trControl=control)

importance<- varImp(model,scale = FALSE)

plot(importance)

# build new model with selected features
formula.new <- "credit.rating ~ account.balance + 
credit.purpose + previous.credit.payment.status 
+ savings + credit.duration.months"

formula.new<- as.formula(formula.new)

lr.model.new<- glm(formula= formula.new, data=train.data,family = "binomial")

lr.predictions.new <- predict(lr.model.new, test.data, type="response") 
lr.predictions.new <- round(lr.predictions.new)
u= union(lr.predictions.new,test.class.var)
confusionMatrix(table(factor(lr.predictions.new, u),
                      factor(test.class.var, u)))

lr.model.best <- lr.model


lr.prediction.values <- predict(lr.model.best, test.feature.vars,
                                type="response")

predictions <- prediction(lr.prediction.values, test.class.var)

par(mfrow=c(1,2))

plot.roc.curve(predictions, title.text="LR ROC Curve")

plot.pr.curve(predictions, title.text="LR Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

##  Modeling using decision trees

library(rpart)# tree models 
library(caret) # feature selection
library(rpart.plot) # plot dtree
library(ROCR) # model evaluation
library(e1071) # tuning model
source("performance_plot_utils.R") # plotting curves

## separate feature and class variables
test.feature.vars <- test.data[,-1]
test.class.var <- test.data[,1]

formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
dataset.model <- rpart(formula=formula.init, method="class",data=train.data, 
                  control = rpart.control(minsplit=20, cp=0.05))

dataset.predictions <- predict(dataset.model, test.feature.vars, type="class")
u= union(dataset.predictions,test.class.var)
confusionMatrix(table(factor(dataset.predictions, u), 
                      factor(test.class.var, u)))

## dataset specific feature selection
formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=train.data, method="rpart", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

formula.new <- "credit.rating ~ account.balance + savings +
                        credit.amount + credit.duration.months + 
previous.credit.payment.status"
formula.new <- as.formula(formula.new)
dataset.model.new <- rpart(formula=formula.new, method="class",
                      data=train.data, 
                      control = rpart.control(minsplit=20, cp=0.05),
                      parms = list(prior = c(0.7, 0.3)))

dataset.predictions.new <- predict(dataset.model.new, test.feature.vars,
                              type="class")
u= union(dt.predictions.new ,test.class.var)
confusionMatrix(table(factor(dataset.predictions.new, u),
                      factor(test.class.var, u)))
## view model
dataset.model.best <- dataset.model.new
print(dataset.model.best)

## visualize the tree

par(mfrow=c(1,1))
prp(dt.model.best, type=1, extra=3, varlen=0, faclen=0)

## predictions

dataset.predictions.best <- predict(dataset.model.best, test.feature.vars,
                               type="prob")
dataset.prediction.values <- dataset.predictions.best[,2]
predictions <- prediction(dataset.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")

## calculate the auc value 

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc