rm(list=ls())                           # to clear environment (upper right pane)
while (!is.null(dev.list())) dev.off()  # to clear plot output (lower right pane)

setwd("C:\\texts") # set proper working direction
getwd()
dir()
-------------------------------------------------------------------------------------
library(psych)
install.packages("gridExtra")
library("gridExtra")

##### Data Preparation #####

# Remove any missing value in the data
red_wine <- read.csv("winequality_red.csv")
red_wine <- na.omit(red_wine)
View(red_wine)
str(red_wine)

# Remove outliers

library("ggplot2")

# Red 1
red1 <- red_wine$fixed.acidity
ggplot(red_wine, aes(x=red1)) + geom_histogram(colour="black", fill="pink") + ggtitle("Fixed acidity distribution")   + ylab("Quantity") + xlab("Fixed acidity") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out1 <- boxplot(red1,
                main = "Fixed acidity boxplot",
                xlab = "Grams/liter",
                ylab = "Fixed acidity",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out  
out1 <- red1[red1>14]
red_wine <- red_wine[-which(red1 %in% out1),]

# Red 2
red2 <- red_wine$volatile.acidity
ggplot(red_wine, aes(x=red_wine$volatile.acidity)) + geom_histogram(colour="black", fill="pink") + ggtitle("Volatile acidity distribution")   + ylab("Quantity") + xlab("Volatile acidity") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out2 <- boxplot(red2,
                main = "Volatile acidity boxplot",
                xlab = "Grams/liter",
                ylab = "Volatile acidity",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
out2 <- red2[red2>1.2]
red_wine <- red_wine[-which(red2 %in% out2),]

# Red 3
red3 <- red_wine$citric.acid
ggplot(red_wine, aes(x=red3)) + geom_histogram(colour="black", fill="pink") + ggtitle("Citric acidity distribution")   + ylab("Quantity") + xlab("Citric acidity") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out3 <- boxplot(red3,
                main = "Citric acidity boxplot",
                xlab = "Grams/liter",
                ylab = "Citric acidity",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
red_wine <- red_wine[-which(red3 %in% out3),]

# Red 4
red4 <- red_wine$residual.sugar
ggplot(red_wine, aes(x=red4)) + geom_histogram(colour="black", fill="pink") + ggtitle("Residual sugar distribution") + ylab("Quantity") + xlab("Residual sugar") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out4 <- boxplot(red4,
                main = "Residual sugar boxplot",
                xlab = "Grams/liter",
                ylab = "Citric acidity",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out

red4plot <- ggplot(red_wine, aes(x=red_wine$residual.sugar)) + geom_histogram()
red4plot_log <- red4plot + scale_x_log10()
red4plot_sqrt <- red4plot + scale_x_sqrt()
grid.arrange (red4plot,red4plot_log, red4plot_sqrt, ncol=1)

red4 <- (log10(red4))
out4 <- red4[red4>0.8]
red_wine <- red_wine[-which(red4 %in% out4),]
ggplot(red_wine, aes(x=red_wine$residual.sugar)) + geom_histogram(colour="black", fill="pink") + ggtitle("Residual sugar distribution") + ylab("Quantity") + xlab("Residual sugar") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))

# Red 5
red5 <- red_wine$chlorides
ggplot(red_wine, aes(x=red5)) + geom_histogram(colour="black", fill="pink") + ggtitle("Chloride distribution") + ylab("Quantity") + xlab("Chloride") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out5 <- boxplot(red5,
                main = "Chloride boxplot",
                xlab = "Grams/liter",
                ylab = "Chloride",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
summary(red5)

red5plot <- ggplot(red_wine, aes(x=red5)) + geom_histogram()
red5plot_log <- red5plot + scale_x_log10()
red5plot_sqrt <- red5plot + scale_x_sqrt()
grid.arrange (red5plot,red5plot_log, red5plot_sqrt, ncol=1)

red5 <- (log10(red5))
out5up <- red5[red5>-0.5]
out5down <- red5[-1.5>red5]
red_wine <- red_wine[-which(red5 %in% out5up),]
red_wine <- red_wine[-which(red5 %in% out5down),]
ggplot(red_wine, aes(x=red_wine$chlorides)) + geom_histogram(colour="black", fill="pink") + ggtitle("Chloride distribution") + ylab("Quantity") + xlab("Chloride") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))

# Red 6: 
red6 <- red_wine$free.sulfur.dioxide
ggplot(red_wine, aes(x=red_wine$free.sulfur.dioxide)) + geom_histogram(colour="black", fill="pink") + ggtitle("Free sulfur dioxide distribution")   + ylab("Quantity") + xlab("Free sulfur dioxide") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out6 <- boxplot(red6,
                main = "Free sulfur dioxide boxplot",
                xlab = "Milligrams/liter",
                ylab = "Free sulfur dioxide",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
out6 <- red6[red6>50]
red_wine <- red_wine[-which(red6 %in% out6),]

# Red 7: 
red7 <- red_wine$total.sulfur.dioxide
ggplot(red_wine, aes(x=red_wine$total.sulfur.dioxide)) + geom_histogram(colour="black", fill="pink") + ggtitle("Total sulfur dioxide distribution")   + ylab("Quantity") + xlab("Total sulfur dioxide") + theme(plot.title = element_text(face = "bold", color = "black", size=14)) + theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out7 <- boxplot(red7,
                main = "Total sulfur dioxide boxplot",
                xlab = "Milligrams/liter",
                ylab = "Total sulfur dioxide",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
out7 <- red7[red7>140]
red_wine <- red_wine[-which(red7 %in% out7),]


# Red 8: 
red8 <- red_wine$density
ggplot(red_wine, aes(x=red_wine$density)) + 
  geom_histogram(colour="black", fill="pink") + 
  ggtitle("Density distribution")   + 
  ylab("Quantity") + xlab("Density") + 
  theme(plot.title = element_text(face = "bold", color = "black", size=14)) + 
  theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + 
  theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out8 <- boxplot(red8,
                main = "Density boxplot",
                xlab = "Grams/cubic centimeter",
                ylab = "Density",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
out8up <- red8[red8>1.002]
out8down <- red8[0.991>red8]
red_wine <- red_wine[-which(red8 %in% out8up),]
red_wine <- red_wine[-which(red8 %in% out8down),]

# Red 9: 
red9 <- red_wine$pH
ggplot(red_wine, aes(x=red_wine$pH)) + 
  geom_histogram(colour="black", fill="pink") + 
  ggtitle("pH distribution")   + 
  ylab("Quantity") + xlab("pH") + 
  theme(plot.title = element_text(face = "bold", color = "black", size=14)) + 
  theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + 
  theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out9 <- boxplot(red9,
                main = "pH boxplot",
                xlab = "pH scale",
                ylab = "pH",
                col = "orange",
                border = "brown",
                horizontal = TRUE,
                notch = TRUE
)$out
out9up <- red9[red9>3.8]
out9down <- red9[2.9>red9]
red_wine <- red_wine[-which(red9 %in% out9up),]
red_wine <- red_wine[-which(red9 %in% out9down),]

# Red 10
red10 <- red_wine$sulphates
ggplot(red_wine, aes(x=red_wine$sulphates)) + 
  geom_histogram(colour="black", fill="pink") + 
  ggtitle("Sulphate distribution")   + 
  ylab("Quantity") + xlab("Sulphate") + 
  theme(plot.title = element_text(face = "bold", color = "black", size=14)) + 
  theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + 
  theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out10 <- boxplot(red10,
                 main = "Sulphate boxplot",
                 xlab = "Grams/liter",
                 ylab = "Sulphate",
                 col = "orange",
                 border = "brown",
                 horizontal = TRUE,
                 notch = TRUE
)$out
out10 <- red10[red10>1.125]
red_wine <- red_wine[-which(red10 %in% out10),]

# Red 11: 
red11 <- red_wine$alcohol
ggplot(red_wine, aes(x=red_wine$alcohol)) + 
  geom_histogram(colour="black", fill="pink") + 
  ggtitle("Alcohol distribution")   + 
  ylab("Quantity") + xlab("Alcohol") + 
  theme(plot.title = element_text(face = "bold", color = "black", size=14)) + 
  theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + 
  theme(axis.title.y = element_text(face = "bold", color = "black", size=10))
out11 <- boxplot(red11,
                 main = "Alcohol boxplot",
                 xlab = "% volume",
                 ylab = "Alcohol",
                 col = "orange",
                 border = "brown",
                 horizontal = TRUE,
                 notch = TRUE
)$out
red_wine <- red_wine
str(red_wine)
# After cleaning, 146 outliers have been removed out of the data set

##### Exploratory Data Analysis #####

# Summary Statistics
library(psych)
summary(red_wine)
summary_table <- describe(red_wine)
summary_table$range <- NULL
summary_table$kurtosis <- NULL
summary_table$skew <- NULL
summary_table$se <- NULL
write.csv(summary_table, "exportedtable_red_wine.csv")

# Plot "Count on Quality Score"

ggplot(red_wine, aes(x=red_wine$quality)) + 
  geom_bar(colour="black", fill="#F27314") + 
  ggtitle("Quality Score")   + 
  ylab("Quantity") + xlab("Quality scale") + 
  theme(plot.title = element_text(face = "bold", color = "black", size=14)) + 
  theme(axis.title.x = element_text(face = "bold", color = "black", size=10)) + 
  theme(axis.title.y = element_text(face = "bold", color = "black", size=10))

# Distribution histogram on each independent variable (11 independent variables)
par(mfrow=c(1,3))
hist(red_wine$fixed.acidity,xlab="fixed.acidity",ylab="Frequency",main="fixed.acidity", 
     y = c(0,400),col=c("pink","purple","green","yellow"))
hist(red_wine$volatile.acidity,xlab="volatile.acidity",ylab="Frequency",main="volatile.acidity", 
     y = c(0,400),col=c("red","gray","green","yellow"))
hist(red_wine$citric.acid,xlab="citric.acid",ylab="Frequency",main="citric.acid", 
     y = c(0,400),col=c("red","gray","green","orange"))

par(mfrow=c(1,3))
hist(red_wine$residual.sugar,xlab="residual.sugar",ylab="Frequency",main="residual.sugar", 
     y = c(0,400),col=c("green","gray","purple","orange"))
hist(red_wine$chlorides,xlab="chlorides",ylab="Frequency",main="chlorides", 
     y = c(0,400),col=c("red","blue","purple","orange"))
hist(red_wine$free.sulfur.dioxide,xlab="free.sulfur.dioxide",ylab="Frequency",main="free.sulfur.dioxide", 
     y = c(0,400),col=c("gray","blue","purple","green"))

par(mfrow=c(1,3))
hist(red_wine$total.sulfur.dioxide,xlab="total.sulfur.dioxide",ylab="Frequency",main="total.sulfur.dioxide", 
     y = c(0,400),col=c("green","yellow","purple","orange"))
hist(red_wine$density,xlab="density",ylab="Frequency",main="density", 
     y = c(0,400),col=c("green","blue","purple","orange"))
hist(red_wine$pH,xlab="pH",ylab="Frequency",main="pH", 
     y = c(0,400),col=c("gray","orange","purple","green"))

par(mfrow=c(1,3))
hist(red_wine$sulphates,xlab="sulphates",ylab="Frequency",main="sulphates", 
     y = c(0,400),col=c("gray","yellow","purple","orange"))
hist(red_wine$alcohol,xlab="alcohol",ylab="Frequency",main="alcohol", 
     y = c(0,400),col=c("yellow","red","purple","green"))

##### Variables Selection #####

# Detect Correlation between each variable
while (!is.null(dev.list())) dev.off()
library(corrplot)
corrplot(corr=cor(red_wine[,c(1:12)], use = "complete.obs"), type = "up", method = "number")

# plot correlation Xi vs Y 
library(ggplot2)
#1 Fixed.acidity vs Quality
ggplot(red_wine, aes(x = fixed.acidity, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Fixed.acidity vs Quality")
#2 Volatile.acidity vs Quality
ggplot(red_wine, aes(x = volatile.acidity, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Volatile.acidity vs Quality")
#3 Citric.acid vs Quality
ggplot(red_wine, aes(x = citric.acid, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Citric.acid vs Quality")
#4 Residual.sugar vs Quality
ggplot(red_wine, aes(x = residual.sugar, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Residual.sugar vs Quality")
#5 Chlorides vs Quality
ggplot(red_wine, aes(x = chlorides, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Chlorides vs Quality")
#6 Free.sulfur.dioxide vs Quality
ggplot(red_wine, aes(x = free.sulfur.dioxide , y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Free.sulfur.dioxide vs Quality")
#7 Total.sulfur.dioxide
ggplot(red_wine, aes(x = total.sulfur.dioxide, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Total.sulfur.dioxide vs Quality")
#8 Density vs Quality
ggplot(red_wine, aes(x = density, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Density vs Quality")
#9 PH vs Quality
ggplot(red_wine, aes(x = pH, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("PH vs Quality")
#10 Sulphates vs Quality
ggplot(red_wine, aes(x = sulphates, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Sulphates vs Quality")
#11 Alcohol vs Quality
ggplot(red_wine, aes(x = alcohol, y = quality))+
  geom_point(shape = 2)+
  geom_smooth(method = lm, se = FALSE)+
  ggtitle("Alcohol vs Quality")

##### Linear Regression Model #####

# Linear Regression (try on 6 variables)
Linear_regression_model_1 <- lm(quality ~ volatile.acidity + citric.acid + total.sulfur.dioxide +
                                  density + sulphates + alcohol, data = red_wine)
summary(Linear_regression_model_1)

##### Variable Transformation #####

acids_total <- cbind(red_wine$fixed.acidity,red_wine$volatile.acidity,red_wine$citric.acid)
acids_total <- rowSums(acids_total)
red_wine_transform <- cbind(red_wine,acids_total)
lm_acids_model <- lm(quality~acids_total,data = red_wine)
summary(lm_acids_model)

##### Stepwise Regression Model #####

library("stats")
full.model <- lm(quality~.,data=red_wine)
summary(full.model)
step.model <- step(full.model,direction ="backward")
summary(step.model)

##### Lasso Regression Model #####

# Model: lasso
x <- model.matrix(quality~., red_wine)[, -1]
y <- red_wine$quality 

library(glmnet)
set.seed(123)

# Create a vector of lambda values
lambda <- 10^seq(10, -2, length = 100)

# Create train and test data
train = sample(1:nrow(x), nrow(x)*0.75)
test = (-train)
ytest = y[test]

# Find the best lambda from the list via cross-validation
cv.out_ <- cv.glmnet(x[train,], y[train], alpha = 1)
bestlam <- cv.out_$lambda.min
lasso_mod <- glmnet(x[train,], y[train], alpha = 1, lambda = bestlam)
predict(lasso_mod, type = 'coefficients', s = bestlam)[1:12,]


##### Compare two models: OLS (Mutiple Linear Regression) & LASSO #####
OLS.pred <- predict(full.model, newdata = red_wine[test,])
lasso.pred <- predict(lasso_mod, s = bestlam, newx = x[test,])

# Check MSE
OLS_MSE <- mean((OLS.pred - ytest)^2)
lasso_MSE <- mean((lasso.pred-ytest)^2)
OLS_MSE
lasso_MSE

# Plot result 
plot(ytest, main = "Model Comparison")
points(lasso.pred, type = "b", col="blue")

plot(ytest, main = "Model Comparison")
points(round(lasso.pred), type = "b", col="blue")





