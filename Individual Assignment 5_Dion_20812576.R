library(tidyverse)
library(ggplot2)
library(psych)
library(car)

redwine <- read.csv("winequality-red.csv")
whitewine <- read.csv("winequality-white.csv")

# 1 = red, 0 = white. Combined datasets.
df <- rbind(redwine, whitewine)

ggplot(df, aes(y=winetype, x=fixed.acidity)) + geom_point() 
ggplot(df, aes(y=winetype, x=sulphates)) + geom_point() 
ggplot(df, aes(y=winetype, x=pH)) + geom_point() 

ggplot(df, aes(x=pH)) + geom_histogram(color='black',fill='lightblue1') 

df$square.pH <- sqrt(df$pH)
ggplot(df, aes(x=square.pH)) + geom_histogram(color='black',fill='lightblue1')

df$square.sulphates <- sqrt(df$sulphates)
ggplot(df, aes(x=square.sulphates)) + geom_histogram(color='black',fill='lightblue1')

df$square.chlorides <- sqrt(df$chlorides)
ggplot(df, aes(x=square.chlorides)) + geom_histogram(color='black',fill='lightblue1')

df$square.volatile.acidity <- sqrt(df$volatile.acidity)
ggplot(df, aes(x=square.volatile.acidity)) + geom_histogram(color='black',fill='lightblue1')

df$square.total.sulfur.dioxide <- sqrt(df$total.sulfur.dioxide)
ggplot(df, aes(x=square.total.sulfur.dioxide)) + geom_histogram(color='black',fill='lightblue1')

df$square.density <- sqrt(df$density)
ggplot(df, aes(x=square.density)) + geom_histogram(color='black',fill='lightblue1')

# Logistic regression model 

model <- glm(winetype ~ square.pH + square.sulphates + square.chlorides +
               square.volatile.acidity + square.total.sulfur.dioxide + square.density, data = df, family = "binomial")
summary(model)
coef(model)
confint(model)
exp(confint(model))


# Multicollinearity

vif(model)
1/vif(model)
mean(vif(model))

df$log.pH.int <- log(df$square.pH)*df$square.pH
df$log.sulphates.int <- log(df$square.sulphates)*df$square.sulphates
df$log.chlorides.int <- log(df$square.chlorides)*df$square.chlorides
df$log.volatile.acidity.int <- log(df$square.volatile.acidity)*df$square.volatile.acidity
df$log.total.sulfur.dioxide.int <- log(df$square.total.sulfur.dioxide)*df$square.total.sulfur.dioxide
df$log.density.int <- log(df$square.density)*df$square.density

linearity <- glm(winetype ~ square.pH + square.sulphates + square.chlorides +
                   square.volatile.acidity + square.total.sulfur.dioxide + square.density +
                   log.pH.int + log.sulphates.int + log.chlorides.int + log.volatile.acidity.int + 
                   log.total.sulfur.dioxide.int + log.density.int, data = df, family = binomial())
summary(linearity)

# Independence of errors
durbinWatsonTest(model)

# Cook's distance
plot(sort(cooks.distance(model), decreasing=TRUE))
max(cooks.distance(model))

table1 <- data.frame(Variable = c("Intercept","squareroot.pH","squareroot.sulphates","squareroot.chlorides","squareroot.volatile.acidity","squareroot.total.sulfur.dioxide","squareroot.density"),
                     pvalue = c("< .001","< .001","< .001","< .001","< .001","< .001","< .001"))

  
table2 <- data.frame(Variable = c("log.pH.int","log.sulphates.int","log.chlorides.int","log.volatile.acidity.int","log.total.sulfur.dioxide.int","log.density.int"),
                     Coefficient = c(-177.38,-11.68,-76.04,-9.03,11.78,-175042.27),
                     Pvalue = c(".22",".36","< .001",".30",".75",".24"))

table3 <- data.frame(VIF = vif(model))
table3$Reciprocal.VIF <- data.frame(1/vif(model))[,1]

# Model interpretation, fit, analysis
modelChi <- model$null.deviance - model$deviance
chidf <- model$df.null - model$df.residual
chidf
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob
R2.hl<-modelChi/model$null.deviance
R2.hl
