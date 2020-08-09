## Multivariable Linear Regression
## Multivariable Regression
model <- lm(salary ~ ., data = batting_salary)
summary(model)
round(summary(model)$coefficients, 4)
confint(model)
plot(model)
saveRDS(model, "BatterModel.RDS")
# aaaa <-data.frame(summary(model))
# write.csv(model, "batterlm.csv")
#當我們建立出一個線性回歸時，必須要確認其殘差(residual)是否符合下面三個假設：

#常態性(Normality)

#獨立性(Independence)

#變異數同質性(Homogeneity of Variance)

# 故首先我們要先從回歸模型中找到殘差的值，可以使用names()函式，查看回歸模型內具有的資訊

#其中，residuals就是指殘差的值(coefficients代表係數)，因此我們可以取出來後進行上面三個假設的檢定：
# 常態性假設
# shapiro.test()函式可以用來檢驗殘差的常態性：
## Multivariable Regression
# model <- lm(salary ~ ., data = batting_salary)
# round(summary(model)$coefficient, 4)
names(model)
shapiro.test(model$residual)
# 虛無假設H0:殘差服從常態分配，因為p-value < 0.05，代表拒絕H0。

# 獨立性假設
# 要檢驗殘差的獨立性，需要使用套件car中的durbinWatsonTest()函式：
## Multivariable Regression
# model <- lm(salary ~ ., data = batting_salary)
# 因為這個函式會自動去抓模型中的殘差，故這裡放的是模型，而不是殘差的值
durbinWatsonTest(model) 
# 虛無假設H0:殘差間相互獨立，因為p-value < 0.05，代表拒絕H0。

# 變異數同質性假設
# 要檢驗殘差的變異數同質性，需要使用套件car中的ncvTest()函式：
## Multivariable Regression
# model <- lm(salary ~ ., data = batting_salary)
# 因為這個函式會自動去抓模型中的殘差，故這裡放的是模型，而不是殘差的值
ncvTest(model) 
# 虛無假設H0:殘差變異數具有同質性，因為p-value < 0.05，代表拒絕H0。


names(batting_salary)
dim(batting_salary)
sum(is.na(batting_salary$salary))
batting_salary=na.omit(batting_salary)
dim(batting_salary)
sum(is.na(batting_salary))
head(batting_salary)
## PCR
library("pls")
set.seed (1000)
pcr_fit <- pcr(salary~G+AB+R+H+X2B+X3B+HR+RBI+SB+CS+BB+SO+IBB+HBP+SH+SF+GIDP+AVG+OBP+SLG+OPS+IsoP+IsoD+SecA+EqA+OPS_plus+RC, 
               data = batting_salary, scale = TRUE, validation = "CV")
summary(pcr_fit)
saveRDS(pcr_fit, "Batterpcr1.RDS")
validationplot(pcr_fit, val.type = "MSEP")
predplot(pcr_fit)

# regression coefficients
# coef(pcr.fit, intercept = TRUE) # all components
coef(pcr_fit, ncomp = 3, intercept = TRUE) # only 3
coef(pcr_fit, ncomp = 1:3, intercept = TRUE)

# plot regression coefficients
coefplot(pcr_fit)
coefplot(pcr_fit, ncomp = 1:3,
         legendpos = "bottomleft")

# Train vs CV
plot(pcr_fit, labels = c(1:dim(batting_salary)[1]), which = "validation")
plot(pcr_fit, "validation", estimate = c("train", "CV"), legendpos = "topright")
plot(pcr_fit, "validation", estimate = c("train", "CV"), 
     val.type = "R2", legendpos = "bottomright")


# Scores:
scoreplot(pcr_fit, labels = c(1:dim(batting_salary)[1])) # only 1 and 2
scoreplot(pcr_fit, comps = 1:3, 
          labels = c(1:dim(batting_salary)[1]))

scoreplot(pcr_fit) # only 1 and 2
scoreplot(pcr_fit, comps = 1:3)

# Loadings:
loadingplot(pcr_fit, comps = 1:3, scatter = TRUE, 
            labels = c(1:dim(batting_salary)[1]))

loadingplot(pcr_fit, comps = 1:3, scatter = TRUE)

## 

## PCR
# standardization of predictors
# Assuming goal class is column 19 = Salary
# standardization of predictors
library(caret)
set.seed(100)
preObj <- preProcess(batting_salary, method=c("center", "scale"))
dd <- predict(preObj, batting_salary)
dim(dd)
head(dd)
# dd = scale(Hitters)

# partition the original data into training and testing datasets
set.seed(100)
dd = batting_salary
inTrain <- createDataPartition(y=dd$salary,
                               p=0.7, list=FALSE)
train <- dd[inTrain,]
test <- dd[-inTrain,]
dim(train)
dim(test)

#### Principal Components Regression
library(pls)
set.seed(200)
# already standarized data use scale=FALSE
pcr.fit = pcr(salary ~ G+AB+R+H+X2B+X3B+HR+RBI+SB+CS+BB+SO+IBB+HBP+SH+SF+GIDP+AVG+OBP+SLG+OPS+IsoP+IsoD+SecA+EqA+OPS_plus+RC, data=train, scale=FALSE, 
              validation="CV", segments=10)
summary(pcr.fit)
saveRDS(pcr.fit, "Batterpcr2.RDS")
# str(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# comapre to scale=TRUE
pcr.fit.sc = pcr(salary ~ G+AB+R+H+X2B+X3B+HR+RBI+SB+CS+BB+SO+IBB+HBP+SH+SF+GIDP+AVG+OBP+SLG+OPS+IsoP+IsoD+SecA+EqA+OPS_plus+RC, data=train, scale=TRUE, 
                 validation="CV", segments=10)
summary(pcr.fit.sc)
validationplot(pcr.fit.sc, val.type="MSEP")
saveRDS(pcr.fit.sc, "Batterpcr3.RDS")

#
pcr.x <- (capture.output(summary(pcr.fit)))
# pcr.x

validationplot(pcr.fit, val.type="MSEP")

# Compare
# http://www.science.smith.edu/~jcrouser/SDS293/labs/lab11-r.html

# find the best number of components
itemp = which.min(pcr.fit$validation$PRESS)     
itemp
# which.min(pcr.fit$validation$adj)
# dim(train) = 185
itemp.mean = pcr.fit$validation$PRESS[itemp]/(dim(train)[1]) 
itemp.mean
mean((pcr.fit$validation$pred[ , , itemp] - train[, 9])^2) 

itemp.sd = sd((pcr.fit$validation$pred[ , , itemp] - train[, 9])^2)/sqrt(dim(train)[1])   # 43452
itemp.sd

validationplot(pcr.fit, val.type="MSEP")
abline(h = itemp.mean + itemp.sd, lty=2)
k.pcr = min((1:pcr.fit$validation$ncomp)[pcr.fit$validation$PRESS/dim(train)[1] < itemp.mean + itemp.sd])  # the chosen k = 2
k.pcr
abline(v=k.pcr, lty=2)   
pcr.fit$coefficients[ , , k.pcr]   # fitted model with chosen number of components


#################################error#################################################################
# estimating mean prediction error
test.pcr =  predict(pcr.fit, test[, -c(27)], ncomp=k.pcr)
test.pcr

# mean (absolute) prediction error
mean(abs(test[ , 27] - test.pcr))               

# mean (squared) prediction error
mean((test[ , 27] - test.pcr)^2)                

# standard error of mean (squared) prediction error
sd((test[ , 27] - test.pcr)^2)/sqrt(dim(test)[1])  





