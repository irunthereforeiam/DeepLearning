library(ISLR)
library(leaps)
set.seed(123)
swiss <- data.frame(datasets::swiss)
dim(swiss)

head(swiss)
summary(swiss)
View(swiss)

test <- swiss[sample(nrow(swiss), 5), ]
test #5 

train <- swiss[ - sample(nrow(swiss), 5), ]
nrow(train) #42 

a <- lm( Fertility ~ ., data = swiss)
summary(a)

par(mfrow = c(2,2))
plot( train$Education, train$Fertility) 
plot(train$Catholic, train$Fertility )
cor( train$Catholic, train$Fertility) # .4844
plot( train$Infant.Mortality, train$Fertility)
hist(train$Fertility)
 
cor(train$Infant.Mortality ,train$Fertility)


leap <- regsubsets(Fertility~., data = train, nbest = 10)
leapsum <- summary(leap)
leapsum
plot(leap, scale = 'adjr2')


swisslm <- lm(Fertility~., data = train)
swisslm2 <- lm(Fertility~.-Examination, data = train)
#use summary() for a more detailed look.




mean(swisslm$residuals^2)
## [1] 45.44987

mean(swisslm2$residuals^2)
## [1] 47.16062

# with all predictors is better. check on AIC info too. 

library(MASS)
step1 <- stepAIC(swisslm, direction = "both")

step1$anova




