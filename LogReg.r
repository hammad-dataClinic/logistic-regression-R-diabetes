install.packages("dplyr")
library(dplyr)

#file.choose()

g <- read.csv("D:\\Code\\Rstudio online\\DiabetesLogistic.csv", header = TRUE)
View(g)

dim(g)

colnames(x = g)

chol <- g$chol
gender <- as.factor(g$gender)
dm <- as.factor(g$dm)

t_gender <- table(gender, exclude = NULL)
t_gender
addmargins(t_gender)

t_gender <- round(100*prop.table(t_gender))

addmargins(t_gender)

t_gender

dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

summary(chol)

height <- g$height
weight <- g$weight

height.si <- height*0.0254
weight.si <- weight*0.453592
bmi <- weight.si/height.si^2

summary(bmi)

bmi_categories <- ifelse(bmi < 18.5, "underweight", 
                         ifelse(bmi >=18.5 & bmi < 25, "normal", 
                                ifelse(bmi >= 25 & bmi <30, "overwight",
                                       ifelse(bmi > 30, "obese", NA))))
table(bmi_categories, exclude = NULL)

bmi_x_dm <- table(bmi_categories, dm2, exclude = NULL)
#create a table of dm w bmi_categories, in which each value is the percent contributed to the entire sample
round(100*prop.table(bmi_x_dm), digits = 1)

##create a table of dm w bmi_categories, in which each value is the row percent
round(100*prop.table(bmi_x_dm, margin = 1), digits = 1)

##create a table of dm w bmi_categories, in which each value is the COLUMN percent
round(100*prop.table(bmi_x_dm, margin = 2), digits = 1)

#under 45, 45-64, 65-74 and 75 or over.

age <-g$age

ageGroups <- ifelse(age <45, "under 45", 
                    ifelse(age >=45 & age <65, "45-64",
                           ifelse(age >=65 & age < 75, "65-74",
                                  ifelse(age>=75, "75 or over", NA))))
table(ageGroups)
table(gender)

gender <- g$gender

gender_x_age <- table(gender, ageGroups, exclude = NULL)

dim(g)
length(ageGroups)
length

gender_x_age
ageGroups

round(100*prop.table(gender_x_age), digits = 1)

table(dm, exclude = NULL)

#Creating a simple logistic regression model using age as a continuous predictor
logisticDm <-  glm(dm ~ age, family = binomial(link = logit))

summary(logisticDm)

#check the variable used to find regression, the odds are given for the variable tagged "1' in following output
table(logisticDm$y)


#check distribution of a variable with a categorical, to see if a linear relation exists
#binary variable should be kept in columns
dmByAge <- table(age, dm)

freqTable <-  prop.table(dmByAge, margin = 1)
freqTable

#calculate odds and logodds
odds = freqTable[,"yes"]/freqTable[,"no"]

logodds <-  log(odds)

logodds

#plot for the relationship
plot(rownames(freqTable), logodds)

#dm and gender
genderXDM <- glm(dm ~ gender, family = binomial(link = logit))
summary(genderXDM)

table(genderXDM$x)

#for age as the predictor
summary(logisticDm)

#"""summary(logisticDm)

#Call:
#glm(formula = dm ~ age, family = binomial(link = logit))

#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -4.404530   0.542828  -8.114 4.90e-16 ***
#age          0.052465   0.009388   5.589 2.29e-08 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#We have the intercept and the age coefficients, given in LogOdds
# to convert odds to probability, use odds/1+odds

#logodds can be converted to Odds ratio by exponentiatin them as follows:
o = exp(0.052465)

#to convert these odds into probabilty
# probability = odds/1+odds
p = o/(1+o)
p

#so we have:
logoddsAge = 0.052465
ORAge = o
#ORAge is 1.053866

#probility is p = 0.5131132

#this mwans that from in any given year, the probaility of developing dm is 0.5 times or 50% greater than previous year
# also means that the logodds of one year compared to previous year is 0.052,
# while the odds ratio of developing diabetes in say, age 20 compared to age 19, is 1.05

g
View(g$location)
location <- as.factor(g$location)

t <- table(location, dm)

round(100*prop.table(t, margin = 1))

model <-  glm(dm ~ location, family = binomial(link = logit))
summary(model)

exp(-0.1395)

age

hist(age, breaks = 15)

d <- density(age) 
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title 

View(g$chol)

hist(chol)
d <- density(x = chol, na.rm = TRUE)
plot(d)

table(gender, dm)

table(bmi, dm)

hist(bmi)

d_bmi <- density(bmi, na.rm = TRUE)
plot(d_bmi)

table(gender)

modelGender <- glm(dm ~ gender, family = binomial(link = logit))
summary(modelGender)

#model for multiple logistic regression (multivariable is the wrong word to use here because of the fact that 
#we are only interpreting a single outcome)
multiple <- glm(dm ~ ageGroups + gender + bmi_categories, family = binomial(link = logit))

summary(multiple)

exp(0.00408)

#to calculate the confidence intervals for the odds ratio estimates
exp(confint(multiple))

table(ageGroups, dm)
hist(x = bmi_categories)

#model without the categories
simpleMultiple <- glm(dm~age + gender + bmi , family = binomial(link = logit))
summary(simpleMultiple)

exp(0.055454)

exp(confint(simpleMultiple))

colnames(g)
chol
age
insurance <-  as.factor(g$insurance)
insurance

#TEST week 3
test <- glm(dm~ age + chol + insurance, family = binomial(link = logit))
summary(test)

#OR for age
exp(0.049753)

#OR for chol
exp(0.008402)

#OR for insurance1
exp(-0.271955)

#OR for insurance2
exp(-0.589803)

## WEEK 4

#Checking the goodness of fit and predictable nature of our model

#Goodness of fit check is done by McFAdden RSquared, c-stat (area under ROC) and Hosmer Lemeshow

#McFadden R squared:

testmodel <- glm(dm~ age+ gender+ bmi, family = binomial(link = logit))
nullmodel <- glm(dm~ 1, family = binomial(link = logit))

Rsquared <- 1-logLik(testmodel)/logLik(nullmodel)
Rsquared # this is the R-squared value, which is 16% in our case

#calculating c-statistic
install.packages("DescTools")
library(DescTools)
Cstat(testmodel) # gives a cstat of 0.78, which is really good meaning that our model is very close to the 
                 # ideal model using the same predictors

#to run a HosmerLemeshow goodness of fit test
install.packages("generalhoslem")
library(generalhoslem)

logitgof(obs = testmodel$y, exp = fitted(testmodel), g = 10) #this gives a p-value less than 0.05, which actually means
                                                             # THIS IS A BADLY FIT MODEL


summary(testmodel)

#Understanding Deviance: Residual and Null

fullmodel <- glm(dm ~ age + chol + insurance, family = binomial(link = logit))
summary(fullmodel)

anova(fullmodel, test = "Chisq")

#The greater the deviance, the worse the model is. NULL deviance tells us how far the null model is from a 
#perfectly fit model aka the SATURATED MODEL
#As we add more predictors, if they tend to explain the data better, the deviance should decrease, meaning the
#model explains the results better; meaning it is closer to the Saturated model

##Analysis of Deviance Table

#Model: binomial, link: logit

#Response: dm

#Terms added sequentially (first to last)


#Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
#NULL                        388     334.54              
#age        1   35.413       387     299.12 2.667e-09 ***
 # chol       1    7.363       386     291.76  0.006658 ** 
  #insurance  2    2.478       384     289.28  0.289613

#Each new predictor seems to bring the deviance lower, which means that it improves the model
#the P value tells us whether the variable added improves the model substantially while taking into account
#the DEGREES OF FREEDOM,  so  in our case age and chol are good predictors, insurance is not

#Trying out backwards elimination
#Various papers have established several risk factors for the development of diabetes, including age, BMI, cholesterol, HDL and blood pressure.
hdl <- g$hdl
bpsys <- g$bp.1s
bpdias <- g$bp.1d
fitmodel0 <- glm(dm ~ age + bmi + chol + bpsys + bpdias, family = binomial(link = logit)) 

summary(fitmodel0)

fitmodel1 <- glm(dm ~ age + bmi + chol, family = binomial(link = logit)) 

summary(fitmodel1)

#gender, location, frame, insurance, and smoking added to model
location <-  g$location
frame <-  g$frame
smoke <- g$smoking

fitmodel2 <- glm(dm ~ age + bmi + chol + bpsys + bpdias + gender + location+frame+insurance+smoke , family = binomial(link = logit)) 

summary(fitmodel2)

#between models fit1 and fit2, fit1 is better because of a LOWER AIC value
