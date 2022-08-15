install.packages("car")
install.packages("dunn.test")
install.packages("report")
install.packages("car")
install.packages("multcomp")
install.packages("BSDA")
library("BSDA")
library(dunn.test)
library(report)
library(car)
library(multcomp)
library(Rmisc)

######### 0.Read Data #########
load("C:/Users/Kingj/Desktop/Semester VIII/BMD407 - Biomedical Statistics/Project/BloodPressure.RData")
View(BloodPressure)

#########1.Descriptive Statstics #########.
#Summary'
summary(BloodPressure)
#Mean of variables
mean(BloodPressure$dose)
mean(BloodPressure$bp.reduction)
mean(BloodPressure$gender)
#Median of variables
median(BloodPressure$dose)
median(BloodPressure$bp.reduction)
median(BloodPressure$gender)
#Max of all variables
max(BloodPressure$dose)
max(BloodPressure$bp.reduction)
max(BloodPressure$gender)
#Min of all variables.
min(BloodPressure$dose)
min(BloodPressure$bp.reduction)
min(BloodPressure$gender)
#Quantile
quantile(BloodPressure$dose)
quantile(BloodPressure$bp.reduction)
quantile(BloodPressure$gender)
#Frequency table
install.packages('epiDisplay')
library(epiDisplay)
tab1(BloodPressure$dose, sort.group = "decreasing", cum.percent = TRUE)
#Correlation Coefficient
cor(BloodPressure$bp.reduction,BloodPressure$dose)

######### 2.Graphics #########
#Barchart For Gender
barplot(BloodPressure$gender)
#bar chart graph with mean bp.reduction in  males and females 
barplot(tapply(BloodPressure$bp.reduction,list(Gender=BloodPressure$gender),mean,na.rm=T), xlab="Gender",ylab="Mean b[.reduction")
#Histogram
hist(BloodPressure$dose)
hist(BloodPressure$bp.reduction)
#Scatter Plot
plot(BloodPressure$bp.reduction)
abline(lm(bp.reduction~gender,data=BloodPressure),col='red')
plot(BloodPressure$dose)
abline(lm(dose~gender,data=BloodPressure),col='red')
#Box Plot
boxplot(BloodPressure$dose)
boxplot(BloodPressure$gender)

######### 3. Outlier detection #########
# the only table that could be valid to be an outlier is the "BloodPressure$dose" due to the varying of its content
# Check for outliers using *Boxplot*
boxplot(BloodPressure$dose, ylab = "dose",main = "Boxplot of Blood Pressure reduction")
# Check for outliers using *Percentiles*
lower_bound <- quantile(BloodPressure$dose, 0)
lower_bound

upper_bound <- quantile(BloodPressure$dose, .10)
upper_bound

# NOW USING **STATISTICAL TESTS**
# Check for outliers using *Grubbs's test*
install.packages("outliers")
library(outliers)
test <- grubbs.test(BloodPressure$dose)
test
# Check for outliers using *Dixon's test*
subdat <- BloodPressure[1:30,]
test <- dixon.test(subdat$dose)
test
# Check for outliers using *Rosner's test*
library(EnvStats)
test <- rosnerTest(BloodPressure$dose,k = 3)
test

##### After further analysis of the existence of outliers i think that there are no outliers present in this data set.Moreover 
##### when we used Rosner's test it returned Boolean expression being false concerning its presence in the data#####
#-----------------------------------------------------#

##Q4.	Testing for normality

#Check the normality using two methods
#--We have used QQ plot to test to determine whether or not a sample comes from a normal distribution
# Firstly we assume that H0= the data is normally distributed HA = data is not normally distributed


# 1-We firstly need to check normality between pb reduction in each gender,in Dose zero only so   we filter doses to use only dose 0
subdata_Dose_0<-BloodPressure[(BloodPressure$dose== 0),] 

#2- Then we check normality of  bp reduction in both genders
qqnorm(subdata_Dose_0[subdata_Dose_0$gender==0,]$bp.reduction, main=' bp reduction in Males in dose 0')
qqline(subdata_Dose_0[subdata_Dose_0$gender==0,]$bp.reduction)


qqnorm(subdata_Dose_0[subdata_Dose_0$gender==1,]$bp.reduction, main=' bp reduction in Females in  dose 0')
qqline(subdata_Dose_0[subdata_Dose_0$gender==1,]$bp.reduction)
#As we can see the data follows a normal distribution as there is no  deviate  from a straight diagonal line

           #--------------------#
#check normality of  bp reduction per each dose

qqnorm(BloodPressure[BloodPressure$dose=="0",]$bp.reduction, main=' bp reduction in dose 0')
qqline(BloodPressure[BloodPressure$dose=="0",]$bp.reduction,)

qqnorm(BloodPressure[BloodPressure$dose=="2",]$bp.reduction, main=' bp reduction in dose 2')
qqline(BloodPressure[BloodPressure$dose=="2",]$bp.reduction,)

qqnorm(BloodPressure[BloodPressure$dose=="5",]$bp.reduction, main=' bp reduction in dose 5')
qqline(BloodPressure[BloodPressure$dose=="5",]$bp.reduction,)

qqnorm(BloodPressure[BloodPressure$dose=="10",]$bp.reduction, main=' bp reduction in dose 10')
qqline(BloodPressure[BloodPressure$dose=="10",]$bp.reduction,)

#As we can see the data follows a normal distribution as there is no  deviate  from a straight diagonal line
#------------------------------------------------------------------------
#we also used another method to check normality using shapiro.test

#check normality of  bp reduction in both genders
shapiro.test(subdata_Dose_0[subdata_Dose_0$gender==0,]$bp.reduction)
shapiro.test(subdata_Dose_0[subdata_Dose_0$gender==1,]$bp.reduction)

#check normality of  bp reduction per each dose
shapiro.test(BloodPressure[BloodPressure$dose=="0",]$bp.reduction)
shapiro.test(BloodPressure[BloodPressure$dose=="2",]$bp.reduction)
shapiro.test(BloodPressure[BloodPressure$dose=="5",]$bp.reduction)
shapiro.test(BloodPressure[BloodPressure$dose=="10",]$bp.reduction)


#p value of the test turns out to be =  0.6143, 0.7578 , 0.7623 and 0.7763 respectively  per each dose 
#	And p value = 0.3317 in male and 0.5644 in females
#	since all of them are more than 0.05 we can accept the assumption that data comes from a population that is normally distributed 

#------------------------------------------------------------------------

#•Check the homoscedasticity using two methods.

# we Firstly assume that the null hypothesis is that our data have equal vaiance and alternative hypothesis is that our data don't have equal variance
# Firstly we used leveneTest to test the homoscedasticity between bp reduction per each dose 

leveneTest(bp.reduction~gender==0, data=subdata_Dose_0)
leveneTest(bp.reduction~gender==1, data=subdata_Dose_0)

#p value in males and females =0.8752 so it's more than alpha= 0.05 so we don't have enough evidence to reject the null hypothesis so our data is homo

leveneTest(bp.reduction~dose==0, data=BloodPressure)
leveneTest(bp.reduction~dose==2, data=BloodPressure)
leveneTest(bp.reduction~dose==5, data=BloodPressure)
leveneTest(bp.reduction~dose==10, data=BloodPressure)
#The p value dose 0 and 10  = 0.1893 , 0.6661 respectively ,so it's more than alpha= 0.05 so we don't have enough evidence to reject the null hypothesis so our data is homo
# While the p value of dose 2 and 5 = 0.003609, 0.04873 respectively which is less than 0.05 so we don't have enough evidence to reject the null hypothesis so our data is hetero
             #-----------------------#

#Secondly we used var.test to test the homoscedasticity between bp reduction per each dose 
var.test(bp.reduction~dose==0, data=BloodPressure)  # pvalue =0.2062
var.test(bp.reduction~dose==2, data=BloodPressure)  #pvalue = 0.0007731
var.test(bp.reduction~dose==5, data=BloodPressure)  #pvalue = 0.01023
var.test(bp.reduction~dose==10, data=BloodPressure)  #pvalue = 0.9038

#Secondly we used var.test to test the homoscedasticity between bp reduction and each gender

var.test(bp.reduction~gender==0, data=subdata_Dose_0)  # pvalue =0.8729
var.test(bp.reduction~gender==1, data=subdata_Dose_0)  # pvalue =.8729

# The p value in both genders is also greater than 0.05 so our data is homo and we don't have enough evidence to reject null hypothesis
#The p value dose 0 and 10  it's more than alpha= 0.05 so we don't have enough evidence to reject the null hypothesis so our data is homo
# While the p value of dose 2 and 5 is less than 0.05 so we don't have enough evidence to reject the null hypothesis so our data is hetero


#-------------------------------------------------------------------

#Q5.Statistical Inference
#	Calculate the 90%, 95%, 99% confidence interval for the means of bp.reduction per each Dose.

#we used  the z.test() function (from the BSDA package) to construct the confidence interval.
##We supply the population standard deviation and the confidence level to the function 


s1=sd(BloodPressure$bp.reduction)
s2_Does0=sd(BloodPressure$dose==0)
s2_Does2=sd(BloodPressure$dose==2)
s2_Does5=sd(BloodPressure$dose==5)
s2_Does10=sd(BloodPressure$dose==10)

n1=40
n2=40


#90% CI

test2_Dose_0 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==0, sigma.x = s1,sigma.y = s2_Does0, conf.level = 0.90)
test2_Dose_0$conf.int

test2_Dose_2 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==2, sigma.x = s1,sigma.y = s2_Does2, conf.level = 0.90)
test2_Dose_2$conf.int

test2_Dose_5 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==5, sigma.x = s1,sigma.y = s2_Does5, conf.level = 0.90)
test2_Dose_5$conf.int

test2_Dose_10 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==10, sigma.x = s1,sigma.y = s2_Does10, conf.level = 0.90)
test2_Dose_10$conf.int

#The result of 90% Confidence interval for the means of bp.reduction and all doses "0 ,2,5,10" is  between the interval 5.309938 and  9.440062


#---------------------------------------------------------------
#95% CI

test1_Dose_0 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose== 0, sigma.x = s1,sigma.y = s2_Does0, conf.level = 0.95)
test1_Dose_0$conf.int

test1_Dose_2 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose== 2, sigma.x = s1,sigma.y = s2_Does2, conf.level = 0.95)
test1_Dose_2$conf.int

test1_Dose_5 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose== 5, sigma.x = s1,sigma.y = s2_Does5, conf.level = 0.95)
test1_Dose_5$conf.int

test1_Dose_10 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose== 10, sigma.x = s1,sigma.y = s2_Does10, conf.level = 0.95)
test1_Dose_10$conf.int

#The result of 95% Confidence interval for the means of bp.reduction and all doses "0 ,2,5,10" is  between the interval 4.914326  and  9.835674


#---------------------------------------------------------------

#99% CI
test3_Dose_0 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==0, sigma.x = s1,sigma.y = s2_Does0, conf.level = 0.99)
test3_Dose_0$conf.int

test3_Dose_2 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==2, sigma.x = s1,sigma.y = s2_Does2, conf.level = 0.99)
test3_Dose_2$conf.int


test3_Dose_5 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==5, sigma.x = s1,sigma.y = s2_Does5, conf.level = 0.99)
test3_Dose_5$conf.int

test3_Dose_10 <- z.test(BloodPressure$bp.reduction,BloodPressure$dose==10, sigma.x = s1,sigma.y = s2_Does10, conf.level = 0.99)
test3_Dose_10$conf.int

#The result of 99% Confidence interval for the means of bp.reduction and all doses "0 ,2,5,10" is  between the interval 4.141127 and  10.608873
#---------------------------------------------------------------------

#•How would you describe those inferences and what do you observe in terms of the interval width when request higher confidence (i.e. 99% C.I.)?

#It has been observed that that the size of a confidence intervals depends on the confidence level. The bigger the confidence level, the wider the interval is.
#so 99% CI has the widest interval compared with other Intervals

#--------------------------------------------------------------#
############part6-Hypothesis testing
###################################start point 1 of question 6#######################################

#•	We hypothesis that bp.reduction is different between male vs female (in the group that 
#received placebo, i.e. Dose = 0). Assuming normality and homoscedasticity, can you test this
#hypothesis using statistical hypothesis framework
#############ANSWER###########################################33
#statistical question: is there any difference in the mean of bp.reduction between male and female who take placebo?
#null hypothesis(H0):there is no difference between the mean of the male and female in bp.reduction(equal)
#alternative hypothesis(H1):there is a difference in the mean of  bp.reduction between the male and female groups(not equal)


#assume:male=0,female=1
BloodPressure$gender[BloodPressure$gender == "male"] <- 0
BloodPressure$gender[BloodPressure$gender == "Female"] <- 1

#assume it's normally distributed and homoscedasticity
#after we get the t-test value (p-value) we compare it with the alpha value(significance level=0.05)
#as we compare with dose=0 (placebo) 
data0=BloodPressure[BloodPressure$dose==0,]

#if p-value<alpha value so we have enough evidence to reject the null hypothesis
#if p-value > alpha value so we do not have enough evidence to reject the null hypothesis.
#as assuming the normality and homoscedasticity so we will use student's t-test that we use to compare between 2 groups(bp.reduction and gender)  
t.test(bp.reduction~gender, data=data0 , alternative="two.sided",var.equal=TRUE)
###-> p-value=0.01679 which means that it is smaller than th alpha value so we have enough evidence to reject the null hypothesis and support 
###the alternative hypothesis.
###so means are not equal as the mean of male(0) is 2.2 and the mean of female(1) is -4.0


###################################end point 1 of question 6#######################################

###################################start point 2 of question 6#####################################
#•	Assess whether the previous assumptions have been meet for the test.
install.packages("dunn.test")
install.packages("report")
install.packages("car")
install.packages("multcomp")
library(dunn.test)
library(report)
library("car")
library(multcomp)
#to check normality
#we will use qq-plot, histogram and shapiro test also we can use histogram
##null hypothesis(H0):the data is normally distributed
##alternative hypothesis(H1): the data is not normally distributed
##when we apply the shapiro test, the p-value value we compare it with the alpha value(significance level=0.05)

#male
par(mfrow=c(2,2))
qqnorm(data0[data0$gender==0,]$bp.reduction, main='Male bp reduction in dose 0',col='blue')
qqline(data0[data0$gender==0,]$bp.reduction,col='blue')
hist(data0[data0$gender==0,]$bp.reduction,main='Male')
shapiro.test(data0[data0$gender==0,]$bp.reduction)
#p-value is 0.3317

#female
qqnorm(data0[data0$gender==1,]$bp.reduction, main='Female bp reduction in dose 0',col='pink')
qqline(data0[data0$gender==1,]$bp.reduction,col='pink')
hist(data0[data0$gender==1,]$bp.reduction,main='Female')
shapiro.test(data0[data0$gender==1,]$bp.reduction)
#p-value is 0.5644
##so we do not have evidence to reject the null hypothesis as the data in both groups so the data is normally distributed 
#but when we apply histogram it shows that it is not normally distributed, p-value of shapiro test is more stronger




#check homoscedasticity
#null hypothesis(H0):variace in the two groups are equal(homoscedasticity)
#alternative hypothesis(H1):variance in the two groups are not equal(heteroscedasiticy)

var.test(bp.reduction~gender,data=data0)
#p-value is 0.8729 as it is bigger than the alpha value

subset=data.frame(data0) #as backup we copied the main data
subset$gender=factor(subset$gender) #make the gender in the copied data as factor
leveneTest(bp.reduction~gender,data=subset) #to more checking we use levene test because it is more powerful for
#symmatric distributions and robust against many types of non-normality
#the p-value is 0.8752 is bigger than alpha
boxplot(bp.reduction~gender,data=data0)

#as the results of the var.test and the levene test the p-value is greater than the alpha so
#we do not have evidence to reject the null hypothesis so the data is homoscedasticity
###################################end point 2 of question 6#####################################


###################################start point 3 of question 6#####################################
#•	We hypothesis that bp.reduction is “higher” in the group receiving Dose = 10 compared to 
#the control (Dose =0). Can you test this hypothesis assuming heteroscedasiticy

#statistical question: Is the bp.reduction higher with the group who take dose 10 than the group who take dose 0?
#H0:the mean of bp.reduction in group who take dose 10 is less or equal to the mean of bp.reduction the group who take dose 0
#H1:the mean of bp.reduction in group who take dose 10 is higher than the mean of bp.reduction the group who take dose 0

data10=BloodPressure[BloodPressure$dose==10,]
#as the data is assumed is heteroscedasiticy so we check the normality of the two groups (dose=0 and dose=10)
#we already checked the the normality od dose 0 group
##null hypothesis(H0):the data is normally distributed
##alternative hypothesis(H1): the data is not normally distributed
par(mfrow=c(2,2))
BloodPressure$dose=factor(BloodPressure$dose,labels = c("0","2","5","10"))

#dose 0
qqnorm(BloodPressure[BloodPressure$dose=="0",]$bp.reduction, main='bp reduction in dose 0',col='green')
qqline(BloodPressure[BloodPressure$dose=="0",]$bp.reduction,col='green')
hist(BloodPressure[BloodPressure$dose=="0",]$bp.reduction,main='dose 0')
shapiro.test(BloodPressure[BloodPressure$dose=="0",]$bp.reduction)
#p-value is 0.6143 which is greater than the alpha value so we do not have enough evidence to reject the null 
#hypothesis so it is normally distributed
#while the histogram shows that it is not normal

#dose 10
qqnorm(BloodPressure[BloodPressure$dose=="10",]$bp.reduction, main='bp reduction in dose 10',col='red')
qqline(BloodPressure[BloodPressure$dose=="10",]$bp.reduction,col='red')
hist(BloodPressure[BloodPressure$dose=="10",]$bp.reduction,main='Female')
shapiro.test(BloodPressure[BloodPressure$dose=="10",]$bp.reduction)
#p-value is 0.7763 which is greater than the alpha value so we do not have enough evidence to reject the null 
#hypothesis so it is normally distributed
#and the histogram shows that it is normal

install.packages("dplyr")
library(dplyr)

install.packages("onewaytests")
library("onewaytests")

#welch.test(BloodPressure[BloodPressure$dose==0,]$bp.reduction, BloodPressure[BloodPressure$dose==10,]$bp.reduction,var.equal = FALSE)
welch.test(bp.reduction~dose==0,data = BloodPressure) #results of p-value is 3.206514e-06 

welch.test(bp.reduction~dose==10,data=BloodPressure) #result od p-value is 4.164391e-06
## as the p-value smaller than so we  have enough evidence to reject the null hypothesis so the mean of
#bp.reduction in group who take dose 10 is higher than the mean of bp.reduction the group who take dose 0

###################################end point 3 of question 6#####################################


###################################start point 4 of question 6#####################################
#•	Assess the previous test assumption
#check normally distributed
##null hypothesis(H0):the data is normally distributed
##alternative hypothesis(H1): the data is not normally distributed
#dose 0
qqnorm(subData[subData$dose==0,]$bp.reduction, main='bp reduction in dose 0',col='brown')
qqline(subData[subData$dose==0,]$bp.reduction,col='brown')
hist(subData[subData$dose==0,]$bp.reduction)
shapiro.test(subData[subData$dose==0,]$bp.reduction)
#p-value is 0.6143 which is greater than the alpha value so we do not have enough evidence to reject the null 
#hypothesis so it is normally distributed
#while the histogram shows that it is not normal

#dose 10
qqnorm(subData[subData$dose==10,]$bp.reduction, main='bp reduction in dose 10',col='orange')
qqline(subData[subData$dose==10,]$bp.reduction,col='orange')
hist(subData[subData$dose==10,]$bp.reduction)
shapiro.test(subData[subData$dose==10,]$bp.reduction)
#p-value is 0.7763 which is greater than the alpha value so we do not have enough evidence to reject the null 
#hypothesis so it is normally distributed
#while the histogram shows that it is normal


#check variance
#null hypothesis(H0):variace in the two groups are equal(homoscedasticity)
#alternative hypothesis(H1):variance in the two groups are not equal(heteroscedasiticy)
var.test(bp.reduction~dose==0,data=BloodPressure) 
#p-value is 0.2062 as it is bigger than the alpha value
leveneTest(bp.reduction~dose==10,data=BloodPressure)
#the p-value is 0.6661 is bigger than alpha
boxplot(bp.reduction~gender,data=subData)
#as the results of the var.test and the levene test the p-value is greater than the alpha so
#we do not have evidence to reject the null hypothesis so the data is homoscedasticity

###################################end point 4 of question 6#####################################




###################################start point 5 of question 6#####################################

#•	 We hypothesis that bp.reduction is different between the different doses (ignoring the 
#gender). Can you perform comparison between the different groups, after assessing the 
#assumptions and performing post-hoc testing.

#our data contains categorial data (dose) and continous data (bp.reduction)
#we will use ANOVA test

#statistical question:is there any differences in the mean between all the groups?
#H0: there is no difference in the mean of all the groups
#H1: there is at least difference in one group.


#but first we should check normality by shapiro test
shapiro.test(BloodPressure[BloodPressure$dose == "0",]$bp.reduction) #p-value is 0.6143 so we do not have enough 
#evidence to reject H0, normally
shapiro.test(BloodPressure[BloodPressure$dose == "2",]$bp.reduction) #p-value is 0.7578 so we do not have enough 
#evidence to reject H0, normally
shapiro.test(BloodPressure[BloodPressure$dose == "5",]$bp.reduction) #p-value is 0.7623 so we do not have enough 
#evidence to reject H0, normally
shapiro.test(BloodPressure[BloodPressure$dose == "10",]$bp.reduction) #p-value is 0.7763 so we do not have enough 
#evidence to reject H0, normally


#test the equality between the 4 groups
install.packages("multcompView")
library("report")
library(multcompView)
BloodPressure$dose=factor(BloodPressure$dose,labels = c("0","2","5","10"))
anovalModel=aov(bp.reduction~dose,data = BloodPressure)
summary(anovalModel)
#p-value is 7.71e-11 * so it is more smaller than the alpha value 
coef(anovalModel)
report(anovalModel)
# The main effect of dose is statistically significant and large (F(3, 36) = 35.42, p < .001; Eta2 = 0.75, 
#95% CI [0.61, 1.00])
#the p-value is smaller than the alpha value so we have enough evidence to reject the null hypothesis
oneway.test(bp.reduction~dose,data = BloodPressure,var.equal = T) 
#p-value is p-value = 7.707e-11


###post-hoc
#null hypothesis (H0): there is no difference in the mean of all the groups
#alternative hypothesis (H1): there is at least difference in one group.

PairComp<-TukeyHSD(anovalModel) 
PairComp
#the results shows that there are differences in the mean between the groups
plot(PairComp)


###################################end point 5 of question 6#####################################



#-----------------------------Q7-Linearity --------------------------------------------------------------------------------

subsetdata_doese0<-BloodPressure[(BloodPressure$dose == 0), ]
Regression_Male_vsFemale_in_dose_0= lm(subsetdata_doese0$bp.reduction ~ subsetdata_doese0$gender)
summary(Regression_Male_vsFemale_in_dose_0)
confint(Regression_Male_vsFemale_in_dose_0,level=0.95)
pt(-3.011,8)*2
#-----------------------------------------------------------------------
#p-value is less than alpha, s, we have enough evidence to reject the null hypo
#we can reject the null hypo as 0 not included in the interval which mean there is a difference between females and males when there is no dose
#-----------------------------------------------------------------------
Regression_all_doses=lm(BloodPressure$bp.reduction ~ BloodPressure$dose)
summary(Regression_all_doses)
confint(Regression_all_doses,level=0.95)
#-----------------------------------------------------------------------
# P-value is 1.2 which greater than alpha 0.05 which means we do not have evidence to reject the null
# confidence interval contains zero which mean we there is difference between females and males in different doses
#------------------------------- Bouns question---------------
m1<-lm(BloodPressure$bp.reduction~ BloodPressure$gender+BloodPressure$dose)
summary(m1)
library(car)
avPlots(m1)
#------------------------------------------ For extra understanding
subsetdata<-BloodPressure[(BloodPressure$gender == 1), ]
Multiplereg=lm(subsetdata$bp.reduction ~ subsetdata$dose)
summary(Multiplereg)
##
subsetdata2<-BloodPressure[(BloodPressure$gender == 0), ]
Multiplereg2=lm(subsetdata2$bp.reduction ~ subsetdata2$dose)
summary(Multiplereg2)













