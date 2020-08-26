# Case Study Solutions : solder.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('solder.csv')
# View the data loaded
data
# Dropping the first column which is nothing but the Serial number
data=data[2:7]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 900 rows and 6 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is to model/predict the "number" of visible solder skips
# using the predictor variables given in the dataset

# The response variable : skips as we see is a "count" variable 
data$skips

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 3:

q3<- c(mean(data$skips), var(data$skips))
c(mean=q3[1], var=q3[2], ratio=q3[2]/q3[1])

# The mean number of solder visible skips is 5.53 and the variance is 71.35, 
# 12 times the mean. 
# The data is clearly over-dispersed

#-------------------------------------------------------------------------------------------------
# Soln. to Question 4:

#Summarising the dataset : 
summary(data)

# Observations :
# Opening with factor levels :  L.M and S : Equally shared with 300 observations
# Solder with factor levels : Thick & Thin are also equally shared in the data with 450 observations
# Pad Type with factor levels : D4,D6,D7,L4,L6 & L7 have 90 each and (Other) has 360
# Skips variable has max value of 48 and a median of 2

#-------------------------------------------------------------------------------------------------
# Soln. to Question 5:

# Plotting a histogram for the number of solder visible skips (response/target variable)

hist(data$skips,col="red", xlim=c(0,50),
     xlab="Number of visible solder skips")

# The distribution here clearly looks like a Poisson Distribution
# It looks clumped at 0 (total zeroes : 285 out of 900 overall)
# Histogram indicates that this dataset has abundance of zeroes

#-------------------------------------------------------------------------------------------------
# Soln. to Question 6:

# Creating a frequency distribution table
table(data$skips)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

# We have seen that the response variable "skips" is clearly a count variable.
# The distribution as we clearly see isn't normal which the implementation of linear regression model assumptions require.
# The count data does not follow a normal distribution, because it simply can not and 
# hence, simple linear regression is not the way to go. 
# Hence, we go for Poisson , Negative Binomial models for count variables.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

# Check the datatypes of the variables 
str(data)

boxplot(skips~Opening,data)

# It can be seen that the distribution for Opening : L and M are quite similar,
# except that for the S . S has a migher median compared to L & M
# Also :  there are few observations as potential outliers amongst all 3 openings

#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

# Creating a 2 way frequency table

freq_table <- table(data$Solder,data$Mask)
freq_table

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Draw a bar plot to the table created above: 

barplot(freq_table, main="Choice vs Region Distribution",xlab="Mask",legend = rownames(freq_table), beside=TRUE)

# Clearly seen : Thick is the most prominent for A3 mask
# Thin is the most prominent for A6 mask

#-------------------------------------------------------------------------------------------------

# Soln. to Question 11:

data.pois <- glm(skips ~ ., data, family = poisson)
summary(data.pois)

# Here : as we observe as per significance : all predictor variables are significant except "PadTypeD7"

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# AIC stands for : Akaike Information Criterion or 
# AIC is an alternative criterion for model selection and is based on log of maximum likelihood 
# function under the assumed model when the model dimension is also unknown

# AIC does not provide a test for model fit but makes a statement about the relative quality of 
# models and the model with smallest AIC is prefered.
#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

# Influence plot
# install.packages("car")
library(car)
influencePlot(data.pois)

# So data points : 896, 879, 519, 398 and 397 are at the extremes. 
# Let's check in the dataset

data[c(397:398, 519, 879, 896),]

# Case 879: had the number of visible solder skips of 42 !
# Others: Number of visible solder skips >> predicted (because they have larger residuals as seen in graph)

# Also, or the Poisson model, 1 observations is nominated as large + outliers:
outlierTest(data.pois, cutoff=0.001)
# Cases : 398 (Higher residuals ~ 5)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# The output from summary(data.pois) above showed that the Poisson model fits quite badly. 
# The residual deviance is 1829 with 882 degrees of freedom

# The residual deviance is way too higher than the residual degrees of freedom
# If the asssumed model (Poisson) fits the data well, the residual deviance are expected to be 
# approximately equal to the residual degrees of freedom.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# As, we clearly saw in answer 2:  Mean Count of visible solder skips = 5.53 which is almost 
# 12 times variance of the number/count of skips = 71.35 
# Thus the data are highly over dispersed

# Variance > Mean : Overdispersion

# If the variance is equal to the mean, the dispersion statistic would equal one.
# When the dispersion statistic is close to one, a Poisson model fits. 
# If it is larger than one, a negative binomial model fits better.

# The existence of overdispersion leads to the requirement of alternative models that fit the data better.
# Overdispersion is a common phenomenon with count data which is due to either excess of zeros or heterogeneity of population.

# install.packages("AER")
library(AER)
dispersiontest(data.pois,trafo = 1)

# Here : we observe the alternative hypothesis is true , indicating overdispersion

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:

# Alternative model which you would use to take care of over-dispersion is : 
# "Negative Binomial" model

# The negative-binomial model is a different generalization of the Poisson that allows for over-dispersion

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# This is implemented in glm.nb() in the MASS package.

library(MASS)

data.nbin <- glm.nb(skips ~ ., data)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

summary(data.nbin)
# Observations: (Negative Binomial Model)
# Residual deviance: 1008.3  on 882  degrees of freedom
# AIC = 3683.3

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# Comparison of Poisson Model with the above fit NB Model

summary(data.pois)

# Observations: (Poisson Model)
# Residual deviance: 1829  on 882 degrees of freedom
# AIC: 3967.6

# Observations: (NB Model)
# Residual deviance: 1008.3  on 882  degrees of freedom
# AIC: 3683.3


# Here : it's clearly observed : NB model performs better than Poisson due to over dispersion
# and is a good fit (Residual deviance is closer to the residual degrees of freedom and lower AIC)

library(vcdExtra)
LRstats(data.pois,data.nbin)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# Zero Inflated & Hurdle Models

# Hurdle models assume that there is only one process by which a zero can be produced, 
# while zero-inflated models assume that there are 2 different processes that can produce a zero.

# Hurdle models assume 2 types of subjects: 
# (1) those who never experience the outcome and 
# (2) those who always experience the outcome at least once. 

# Zero-inflated models conceptualize subjects: 
# (1) those who never experience the outcome and 
# (2) those who can experience the outcome but don't always.

# There is package pscl containing hurdle() and zeroinfl() functions to implement in R

#-------------------------------------------------------------------------------------------------
# Soln. to Question 21:

# Fitting Zero Inflated and Hurdle Models

# install.packages("pscl")
library(pscl)

# Zero Inflated Models ( Poisson and Negative Binomial)
data.zip <- zeroinfl(skips ~ ., data, dist="poisson")
data.znb <- zeroinfl(skips ~ ., data, dist="negbin")

summary(data.zip)
summary(data.znb)


# Hurdle Models ( Poisson and Negative Binomial)
data.hp  <- hurdle(skips ~ ., data, dist="poisson")
data.hnb <- hurdle(skips ~ ., data, dist="negbin")


summary(data.hp)
summary(data.hnb)


#-------------------------------------------------------------------------------------------------
# Soln. to Question 22:

summary(data.znb)

# Opening is the significant variable affecting the zero counts for the best fit model
# followed by Mask B3 and PadTypeW4!

# ZINB has the lowest AIC amongst all the models proving it to be the best fit for the given data.


#-------------------------------------------------------------------------------------------------
# Soln. to Question 23:

# Comparing Models : 
LRstats(data.pois, data.nbin, data.zip, data.znb, data.hp, data.hnb, 
        sortby="AIC")

# From the final output of comparison between Poisson, Negative Binomial, Zero Inflated and Hurdle Models

# The final recommendation is that the Zero Inflated Negative Binomial model is the best fit !
