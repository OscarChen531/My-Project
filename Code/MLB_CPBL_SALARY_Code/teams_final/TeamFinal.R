# install.packages(c("pls", "caret", "data.table", "devtools", "formattable", "ISLR", "Rcpp", "stringi"))
library(pls)
library(caret)
library(data.table)
library(devtools)
library(formattable)
library(ISLR)
library(Rcpp)
library(stringi)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)


Team_salaries <- read.table("Team_salaries.csv", header = T, sep = ",")
Teams1985_2016 <- Teams[1918:2835,]


Teams1985_2016$RD =with(Teams1985_2016, R -RA)
Teams1985_2016$Wpct =with(Teams1985_2016, W/(W +L))

Teams1985_2016 <- merge(Teams1985_2016, Team_salaries,by=c("yearID",  "teamID", "lgID"))
Teams1985_2016 <- Teams1985_2016[,c(1:3, 7:10, 15:40, 44:45, 49:51)]

plot(Teams1985_2016$RD, Teams1985_2016$Wpct, xlab ="Run Differential", ylab ="Winning Percentage", main="Relationship between Run and Winning Percentage")

attach(Teams1985_2016)

## Descriptive Statistics
# describe(batting_salary)
dfSummary(Teams1985_2016)
view(dfSummary(Teams1985_2016))

## Multivariable Regression
linfit =lm(salary~., data=Teams1985_2016)
saveRDS(linfit, "Teammodel.RDS")
summary(linfit)
round(summary(linfit)$coefficients, 2)
round(summary(linfit)$coefficient, 4)
confint(linfit)
plot(linfit)

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
names(linfit)
shapiro.test(linfit$residual)
# 虛無假設H0:殘差服從常態分配，因為p-value < 0.05，代表拒絕H0。

# 獨立性假設
# 要檢驗殘差的獨立性，需要使用套件car中的durbinWatsonTest()函式：
## Multivariable Regression
# model <- lm(salary ~ ., data = batting_salary)
# 因為這個函式會自動去抓模型中的殘差，故這裡放的是模型，而不是殘差的值
durbinWatsonTest(linfit) 
# 虛無假設H0:殘差間相互獨立，因為p-value < 0.05，代表拒絕H0。

# 變異數同質性假設
# 要檢驗殘差的變異數同質性，需要使用套件car中的ncvTest()函式：
## Multivariable Regression
# model <- lm(salary ~ ., data = batting_salary)
# 因為這個函式會自動去抓模型中的殘差，故這裡放的是模型，而不是殘差的值
ncvTest(linfit) 
# 虛無假設H0:殘差變異數具有同質性，因為p-value < 0.05，代表拒絕H0。

# Stepwise regression model
step.model <- stepAIC(linfit, direction = "both", 
                      trace = FALSE)
summary(step.model)

set.seed(1000)
pcr.fit = pcr(salary ~ G + Ghome + W + L + R + AB + H + X2B + X3B + HR + BB + SO + SB + CS + HBP + SF + RA + ER + ERA + CG + SHO + SV + IPouts + HA + HRA + BBA + SOA + E + DP + FP + BPF + PPF + RD + Wpct, data=Teams1985_2016, scale=TRUE, validation="CV")
summary(pcr.fit)
RMSEP(pcr.fit)
MSEP(pcr.fit)
saveRDS(pcr.fit, "teampcr1.RDS")

# plot the root mean squared error
# validaton plot
validationplot(pcr.fit, val.type = "MSEP")


# plot the predicted vs measured values
predplot(pcr.fit)

# regression coefficients
# coef(pcr.fit, intercept = TRUE) # all components
coef(pcr.fit, ncomp = 5, intercept = TRUE) # only 3
coef(pcr.fit, ncomp = 1:5, intercept = TRUE)

# plot regression coefficients
coefplot(pcr.fit)
coefplot(pcr.fit, ncomp = 1:5,
         legendpos = "bottomleft")

# Train vs CV
plot(pcr.fit, labels = c(1:dim(Teams1985_2016)[1]), which = "validation")
plot(pcr.fit, "validation", estimate = c("train", "CV"), legendpos = "topright")
plot(pcr.fit, "validation", estimate = c("train", "CV"), 
     val.type = "R2", legendpos = "bottomright")


# Scores:
scoreplot(pcr.fit, labels = c(1:dim(Teams1985_2016)[1])) # only 1 and 2
scoreplot(pcr.fit, comps = 1:5, 
          labels = c(1:dim(Teams1985_2016)[1]))

scoreplot(pcr.fit) # only 1 and 2
scoreplot(pcr.fit, comps = 1:5)

# Loadings:
loadingplot(pcr.fit, comps = 1:5, scatter = TRUE, 
            labels = c(1:dim(Teams1985_2016)[1]))

loadingplot(pcr.fit, comps = 1:5, scatter = TRUE)

## PCR
# standardization of predictors
# Assuming goal class is column 19 = Salary
# standardization of predictors
set.seed(100)
preObj <- preProcess(Teams1985_2016, method=c("center", "scale"))
dd <- predict(preObj, Teams1985_2016)
dim(dd)
head(dd)
# dd = scale(Teams1985_2016)

# partition the original data into training and testing datasets
set.seed(100)
dd = Teams1985_2016
inTrain <- createDataPartition(y=dd$salary,
                               p=0.7, list=FALSE)
train <- dd[inTrain,]
test <- dd[-inTrain,]
dim(train)
dim(test)

#### Principal Components Regression
set.seed(200)
# already standarized data use scale=FALSE
pcr.fit = pcr(salary ~ ., data=train, scale=FALSE, 
              validation="CV", segments=10)
summary(pcr.fit)
saveRDS(pcr.fit,"teampcr2.RDS")
# str(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# comapre to scale=TRUE
pcr.fit.sc = pcr(salary ~ ., data=train, scale=TRUE, 
              validation="CV", segments=10)
summary(pcr.fit.sc) 
validationplot(pcr.fit.sc, val.type="MSEP")

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
mean((pcr.fit$validation$pred[ , , itemp] - train[, 34])^2) 

itemp.sd = sd((pcr.fit$validation$pred[ , , itemp] - train[, 34])^2)/sqrt(dim(train)[1])   # 43452
itemp.sd

validationplot(pcr.fit, val.type="MSEP")
abline(h = itemp.mean + itemp.sd, lty=2)
k.pcr = min((1:pcr.fit$validation$ncomp)[pcr.fit$validation$PRESS/dim(train)[1] < itemp.mean + itemp.sd])  # the chosen k = 2
k.pcr
abline(v=k.pcr, lty=2)   
pcr.fit$coefficients[ , , k.pcr]   # fitted model with chosen number of components

# estimating mean prediction error
test.pcr =  predict(pcr.fit, test[, -c(34)], ncomp=k.pcr)
test.pcr

# mean (absolute) prediction error
mean(abs(test[ , 34] - test.pcr))               

# mean (squared) prediction error
mean((test[ , 34] - test.pcr)^2)                

# standard error of mean (squared) prediction error
sd((test[ , 34] - test.pcr)^2)/sqrt(dim(test)[1])  
