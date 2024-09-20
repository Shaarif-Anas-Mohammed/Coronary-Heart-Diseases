#READ THE DATA AND CHANGE VARIABLES CLASS 
  
data = read.csv(file.choose(), header=T)
data$TenYearCHD=as.factor(data$TenYearCHD)
data$diabetes=as.factor(data$diabetes)
data$prevalentHyp=as.factor(data$prevalentHyp)
data$prevalentStroke=as.factor(data$prevalentStroke)
data$BPMeds = as.factor(data$BPMeds)
data$currentSmoker  =as.factor(data$currentSmoker)
data$male = as.factor(data$male)
summary(data)

#CREATE CONTINGENCY TABLES
  
attach(data)
gender.table = table(male,TenYearCHD)
currentsmoker.table = table(currentSmoker,TenYearCHD)
BPmedication.table = table(BPMeds,TenYearCHD)
prevalentstroke.table = table(prevalentStroke,TenYearCHD)
prevalenthyp.table = table(prevalentHyp,TenYearCHD)
diabetes.table = table(diabetes,TenYearCHD)

# BOXPLOTS FOR EACH NUMERIC VARIABLES AGAINST RESPONSE VARIABLE

boxplot(age~TenYearCHD, main = "Age vs TenYearCHD", xlab="TenYearCHD", ylab="Age",las=1, col=4, names=c("NO", "YES"))
boxplot(heartRate~TenYearCHD, main = "Heart rate vs TenYearCHD", xlab="TenYearCHD", ylab="Heart rate",las=1, col=4, names=c("NO", "YES"))
boxplot(cigsPerDay~TenYearCHD, main = "cigs per day vs TenYearCHD", xlab="TenYearCHD", ylab="cigs per day",las=1, col=4, names=c("NO", "YES"))
boxplot(totChol~TenYearCHD, main = "Total Cholestrol vs TenYearCHD", xlab="TenYearCHD", ylab="Total Cholestrol",las=1, col=4, names=c("NO", "YES"))
boxplot(sysBP~TenYearCHD, main = "Systole Blood Pressure vs TenYearCHD", xlab="TenYearCHD", ylab="Systole BP",las=1, col=4, names=c("NO", "YES"))
boxplot(diaBP~TenYearCHD, main = "diastole Blood Pressure vs TenYearCHD", xlab="TenYearCHD", ylab="diastole BP",las=1, col=4, names=c("NO", "YES"))
boxplot(BMI~TenYearCHD, main = "Body Mass Index vs TenYearCHD", xlab="TenYearCHD", ylab="BMI",las=1, col=4, names=c("NO", "YES"))
boxplot(glucose~TenYearCHD, main = "Glucose levels vs TenYearCHD", xlab="TenYearCHD", ylab="Glucose levels",las=1, col=4, names=c("NO", "YES"))

# PIECHARTS FOR EACH CATEGORIAL VARIABLES 
  
pie(c(2419,1819),c("Female", "Male"),main="Gender",col=c(5,2))
pie(c(2144,2094),c("NO", "YES"),main="Current Smoker",col=c(5,2))
pie(c(4061,177),c("NO", "YES"),main="BP Medication",col=c(5,2))
pie(c(4213,25),c("NO", "YES"),main="Prevalent Stoke",col=c(5,2))
pie(c(2922,1316),c("NO", "YES"),main="Prevalent Hypertension",col=c(5,2))
pie(c(4129,109),c("NO", "YES"),main="Diabetes",col=c(5,2))
pie(c(3594,644),c("NO", "YES"),main="Ten Year CHD",col=c(5,2))

# 95% CI FOR CATEGORICAL EXPLANATORY VARIABLES 

library(binom)
binom.confint(1819,4238,methods="asymptotic")
binom.confint(2094,4238,methods="asymptotic")
binom.confint(177,4238,methods="asymptotic")
binom.confint(25,4238,methods="asymptotic")
binom.confint(1316,4238,methods="asymptotic")
binom.confint(109,4238,methods="asymptotic")
binom.confint(644,4238,methods="asymptotic")

#RISK RATIOS FOR CATEGORICAL VARIABLES 

library(epitools)
riskratio(gender.table,method="wald",rev="both")
riskratio(currentsmoker.table,method="wald",rev="both")
riskratio(BPmedication.table,method="wald",rev="both")
riskratio(prevalentstroke.table,method="wald",rev="both")
riskratio(prevalenthyp.table,method="wald",rev="both")
riskratio(diabetes.table,method="wald",rev="both")



