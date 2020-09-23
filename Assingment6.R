#Author: ECON452
#Date: Fall 2019
#Subject: Working with the GDP dataset
#Version: 3.0

#1. Import Data
#2. Clean and Transform Data
#3. Visualize Data
#4. Build regression model
#5. Export Data

############################################ Preliminaries ##############################################
#delete complete workspace
rm(list = ls())

#change workdirectory (setwd)
#>>>>>>>>>>> change your path here
dir.input = "/Users/poochiestemmler/Downloads/"

#set working directory
setwd(dir.input)

#load libraries
library(corrplot)
library(leaps)

######################################## Variable definition ############################################
#input file name  
data.filename <- "GDP_raw_data_clean_dummy.csv"

###################################### End Variable definition ##########################################
########################################## End preliminaries ############################################

#********************************************************************************************************
############################################# Import data ###############################################
#read in stock data
data.raw <- read.csv(data.filename, stringsAsFactors = FALSE)

#review data
head(data.raw)

############################################# CLEANING ################################################## 
sum(complete.cases(data.raw))
#sum(complete.cases(t(data.raw)))

data.gdp <- data.raw[complete.cases(data.raw),]
#data.gdp2 <- data.raw[,complete.cases(t(data.raw))]


############################################# TRANSFORMATION ############################################ 
#calculate returns by: (p(t)/p(t-1)) - 1
#previous years needed

#create dummy variables using 'model.matrix' (creates a matrix, e.g., by expanding factors to a set of dummy variables)
#model.matrix(~data.gdp$State)
dInGroup <- model.matrix(~ data.gdp$Income.group - 1) #We obtain an identity matrix if we remove the intercept from the model (-1)

#select DM as base variable and remove it
dInGroup <- data.frame(dInGroup[,c(-1)]) #selected 'High income' as base variable
colnames(dInGroup)[1] <- "LowIncome"
colnames(dInGroup)[2] <- "LowMiddleIncome"
colnames(dInGroup)[3] <- "HighMiddleIncome"

#merge dummy variables with dataset
data.gdp <- cbind(data.gdp,dInGroup)

#extract numerics
data.gdp_num <- data.gdp[,-c(1,2)]

############################################# VISUALIZATION ############################################# 
#create line chart for stock performance
plot (data.gdp$Real.GDP.Growth, main="GDP Real Growth", ylab = "GDP Growth", type="l", col="blue")

#create histogram for stock returns
hist(data.gdp$Real.GDP.Growth, main="GDP Real Growth Distribution", col="deepskyblue")

############################################# ANALYSE ################################################### 
#Summary of the dataset
summary(data.gdp_num)

#descriptive analysis
mean(data.gdp_num$Real.GDP.Growth)
median(data.gdp_num$Real.GDP.Growth)
sd(data.gdp_num$Real.GDP.Growth)
min(data.gdp_num$Real.GDP.Growth)
max(data.gdp_num$Real.GDP.Growth)

#### Bivariate analysis ####
#correlation
gdp.correl <- cor(data.gdp_num)
corrplot(gdp.correl, method = "circle")
corrplot(gdp.correl, method = "number")

plot(data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Urban_Pop., col = 'blue')
plot(data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Gini, col = 'green')

#### single factor regression model ####
# '~' Tilde is used to separate the left- and right-hand sides in a model formula (shorthand notation)
# left of the ~ is the response, on the right the explanatory variables
reg1 <- lm(data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Urban_Pop.)
summary(reg1)

reg2 <- lm(data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Unemployment)
summary(reg2)

#combined in Chart
plot (data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Urban_Pop, col = 'lightblue', pch = 19, main = "GDP growth vs Urban_pop")
reg3 <- lm(data.gdp_num$Real.GDP.Growth ~ data.gdp_num$Urban_Pop.)
abline(reg3, col="red") #add linear regression line using 'abline"
summary(reg3)$adj.r.squared

plot (data.gdp_num$Real.GDP.Growth ~ log(data.gdp_num$Urban_Pop), col = 'lightblue',pch = 19, main = "GDP growth vs ln(Urban_pop)")
reg4 <- lm(data.gdp_num$Real.GDP.Growth ~ log(data.gdp_num$Urban_Pop.))
abline(reg4, col="red") #add linear regression line using 'abline"
summary(reg4)$adj.r.squared

#### multi factor regression model ####
# '.' in the formula tells R to use all variables in the dataframe
reg5 <- lm(data.gdp_num$Real.GDP.Growth~.,data = data.gdp_num[,-c(11:13)]) #without dummy
summary(reg5)
plot(reg5$residuals, col = 'red', main = "residuals reg5")

reg6 <- lm(data.gdp_num$Real.GDP.Growth~.,data = data.gdp_num)
summary(reg6)
plot(reg6$residuals, col = 'red', main = "residuals reg5")

data.gdp_num <- cbind(data.gdp_num, log(data.gdp_num$Urban_Pop.))
reg7 <- lm(data.gdp_num$Real.GDP.Growth~.,data = data.gdp_num)
summary(reg7)
plot(reg6$residuals, col = 'red', main = "residuals reg6")

#compare the  models
summary(reg5)$adj.r.squared
summary(reg6)$adj.r.squared
summary(reg7)$adj.r.squared

#run regsubset
reg_all = regsubsets(data.gdp_num$Real.GDP.Growth ~ ., data = data.gdp_num)
reg_all.summary <- summary(reg_all, statistic="adjr2")

plot(reg_all, scale="adjr2")
plot(reg_all.summary$adjr2, xlab="Number of variables", ylab="RSquare",main="Adj.RSQ",type="l")

selected_variables <- 3
coef(reg_all, scale="adjr2",selected_variables)

#adjr2 of the reduced model
summary(reg_all)$adjr2[selected_variables]

############################################# EXPORT ########################################################## 
#create the charts and save them in your work directory
jpeg('GDP_growth.jpg')
plot (data.gdp_num$Real.GDP.Growth, main="GDP Real Growth", type="l", col="blue")
dev.off()

jpeg('regsubsets.jpg')
plot(reg_all, scale="adjr2")
dev.off()

jpeg('regsubsets_no_var.jpg')
plot(reg_all.summary$adjr2, xlab="Number of variables", ylab="RSquare",main="Adj.RSQ",type="l")
dev.off()