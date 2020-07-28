###########Starting Pictcher######################
starting_pitcher_salary <- pitching_salary %>%
  filter(IPouts >= 360)

## Descriptive Statistics
#describe(starting_pitcher_salary)
dfSummary(starting_pitcher_salary)
view(dfSummary(starting_pitcher_salary))


## Multivariable Linear Regression
## Multivariable Regression
model <- lm(salary ~ ., data = starting_pitcher_salary)
summary(model)
round(summary(model)$coefficient, 4)
confint(model)
plot(model)

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

names(starting_pitcher_salary)
dim(starting_pitcher_salary)
sum(is.na(starting_pitcher_salary$salary))
batting_salary=na.omit(starting_pitcher_salary)
dim(starting_pitcher_salary)
sum(is.na(starting_pitcher_salary))
head(starting_pitcher_salary)
## PCR
library("pls")
set.seed (1000)
pcr_fit <- pcr(salary~W+L+G+GS+CG+SHO+SV+IPouts+H+ER+HR+BB+SO+BAOpp+IBB+WP+HBP+BK+BFP+GF+R+SH+SF+GIDP+ERA_plus+APR+ERA, 
               data = starting_pitcher_salary, scale = TRUE, validation = "CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")
predplot(pcr_fit)

# regression coefficients
# coef(pcr.fit, intercept = TRUE) # all components
coef(pcr.fit, ncomp = 3, intercept = TRUE) # only 3
coef(pcr.fit, ncomp = 1:3, intercept = TRUE)

# plot regression coefficients
coefplot(pcr.fit)
coefplot(pcr.fit, ncomp = 1:3,
         legendpos = "bottomleft")

# Train vs CV
plot(pcr.fit, labels = c(1:dim(starting_pitcher_salary)[1]), which = "validation")
plot(pcr.fit, "validation", estimate = c("train", "CV"), legendpos = "topright")
plot(pcr.fit, "validation", estimate = c("train", "CV"), 
     val.type = "R2", legendpos = "bottomright")


# Scores:
scoreplot(pcr.fit, labels = c(1:dim(starting_pitcher_salary)[1])) # only 1 and 2
scoreplot(pcr.fit, comps = 1:3, 
          labels = c(1:dim(starting_pitcher_salary)[1]))

scoreplot(pcr.fit) # only 1 and 2
scoreplot(pcr.fit, comps = 1:3)

# Loadings:
loadingplot(pcr.fit, comps = 1:3, scatter = TRUE, 
            labels = c(1:dim(starting_pitcher_salary)[1]))

loadingplot(pcr.fit, comps = 1:3, scatter = TRUE)

## 

## PCR
# standardization of predictors
# Assuming goal class is column 19 = Salary
# standardization of predictors
library(caret)
set.seed(100)
preObj <- preProcess(starting_pitcher_salary, method=c("center", "scale"))
dd <- predict(preObj, starting_pitcher_salary)
dim(dd)
head(dd)
# dd = scale(Hitters)

# partition the original data into training and testing datasets
set.seed(100)
dd = starting_pitcher_salary
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
pcr.fit = pcr(salary ~ W+L+G+GS+CG+SHO+SV+IPouts+H+ER+HR+BB+SO+BAOpp+IBB+WP+HBP+BK+BFP+GF+R+SH+SF+GIDP+ERA_plus+APR+ERA, data=train, scale=FALSE, 
              validation="CV", segments=10)
summary(pcr.fit)
# str(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# comapre to scale=TRUE
pcr.fit.sc = pcr(salary ~ W+L+G+GS+CG+SHO+SV+IPouts+H+ER+HR+BB+SO+BAOpp+IBB+WP+HBP+BK+BFP+GF+R+SH+SF+GIDP+ERA_plus+APR+ERA, data=train, scale=TRUE, 
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
test.pcr =  predict(pcr.fit, test[, -c(19)], ncomp=k.pcr)
test.pcr

# mean (absolute) prediction error
mean(abs(test[ , 19] - test.pcr))               

# mean (squared) prediction error
mean((test[ , 19] - test.pcr)^2)                

# standard error of mean (squared) prediction error
sd((test[ , 19] - test.pcr)^2)/sqrt(dim(test)[1])  



