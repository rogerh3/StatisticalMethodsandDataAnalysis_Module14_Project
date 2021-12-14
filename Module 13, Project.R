#Roger H Hayden III
#12/3/2021
#Statistical Methods and Data Analysis
#Module 13 - Project

# Description: Data represents the time spent by fish guarding and fanning eggs in their nest.
# Each day, a different male and female are studied. Numerical values for guarding and fanning
# represent minutes performing that activity in a day. Each fish is measured according to both
# guarding and fanning activities. Ten fish of each gender are studied in each of four pH
# conditions. Treat pH and gender as nominally scaled. In R they can be as.factor(). 

fish<-read.csv(file=file.choose(),sep=",",header=TRUE)
names(fish)
dim(fish)

fish[1:5]

# 1) Irrespective of gender and pH, conduct a matched pairs-t test and a Sign test to test
# Ho:mu_Guarding = mu_Fanning. Report the test statistic and p-value. You can use t.test and
# binom.test (or equivalent).

t.test(fish$Guarding, fish$Fanning, mu=0,alt="two.sided",paired=T,conf.level=0.95)

u<- sum(fish$Guarding>fish$Fanning)
binom.test(u,80,p=0.5,alt="two.sided")


# 2) How do the results appear from (1) if we consider Ho:mu_Guarding = mu_Fanning for each pH
# separately? Report the 8 test statistic values and their p-values and note what
# differences you see.

g1<-fish[fish$pH==7,4]
f1<-fish[fish$pH==7,5]

t.test(g1,f1,mu=0,alt="two.sided",paired=T,conf.level-0.95)
u2<-sum(g1>f1)
binom.test(u2,20,p=0.5,alt="two.sided")

g2<-fish[fish$pH==6.5,4]
f2<-fish[fish$pH==6.5,5]

t.test(g2,f2,mu=0,alt="two.sided",paired=T,conf.level-0.95)
u3<-sum(g2>f2)
binom.test(u3,20,p=0.5,alt="two.sided")

g3<-fish[fish$pH==6,4]
f3<-fish[fish$pH==6,5]

t.test(g3,f3,mu=0,alt="two.sided",paired=T,conf.level-0.95)
u4<-sum(g3>f3)
binom.test(u4,20,p=0.5,alt="two.sided")

g4<-fish[fish$pH==5.5,4]
f4<-fish[fish$pH==5.5,5]

t.test(g4,f4,mu=0,alt="two.sided",paired=T,conf.level-0.95)
u5<-sum(g4>f4)
binom.test(u5,20,p=0.5,alt="two.sided")

# 3) How do the results appear from (1) if we consider Ho:mu_Guarding = mu_Fanning for each
# gender separately? Report the 4 test statistic values and their p-values and note what
# differences you see.

m<-fish[fish$Gender=="M",]
f<-fish[fish$Gender=="F",]

t.test(m$Guarding,f$Guarding,mu=0,alt="two.sided",paired=T,conf.level=0.95)

u6<- sum(m$Guarding>f$Guarding)
binom.test(u6,80,p=0.5,alt="two.sided")

t.test(m$Fanning,f$Fanning,mu=0,alt="two.sided",paired=T,conf.level=0.95)

u7<- sum(m$Fanning>f$Fanning)
binom.test(u7,80,p=0.5,alt="two.sided")

# 4) Plot guarding times vs fanning times in a scatterplot, coloring the observations
# according to gender. Do this for each pH separately. The four plots might be more
# effectively considered if you use the par() function in R and then do your four plots.
# Report what you believe is the big story the data has to tell regarding guarding vs
# fanning. This is just exploratory data analysis. Tell me what you see.
# 5) Notes: You are going to want to use the plot function in R and color points by gender
# and by pH to help you visualize the structure of the data.

pH_values <- sort(unique(fish$pH))

fish$Color <- ifelse(fish$Gender=="M", "Blue", "Red")
#Male is blue, Female is Red

par(mfrow=c(2,2))

for(pH_value in pH_values){
  fish2 <- subset(fish, (pH == pH_value))
  
  plot(fish2$Guarding, fish2$Fanning,
       col=fish2$Color,
       main=paste("pH =",pH_value),
       xlab="Time Spent Guarding (min)",
       ylab="Time Spent Fanning (min)",
       pch=16)
  
  lines(0:70,0:70,type="l",col="black")
}



# Description: Generic data set with a response Y and 7 potential predictors X1:X7. The purpose
# of this exercise is to let you experience model building, often a messy process. You want the
# response Y modeled as a function of X1:X7. This is a continuation of least squares in Module 11,
# but the associations are not as clear. The recommended approach is as follows

generic <-read.csv(file=file.choose(),sep=",", header=TRUE)
names(generic)
dim(generic)

# 1) Get to know your data. Make histograms and summary statistics of your data to get a
# sense of distributions.

boxplot(generic$y)
hist(generic$y)
summary(generic)


# 2) Consider associations. Plot each predictor against the response. You could also do a
# quick line fit or get its correlation. Correlation is with "cor()". A line fit can be achieved 
# using the linear model function. Two commands, model<-lm(y~x1), followed by
# summary(model) will give you least squares result for an individual predictor x1 against
# the response. This will give you a rough idea of what might be important. Try for all 7.

model<-lm(generic$y~generic$x1)
summary(model)
plot(generic$y,generic$x1)

model2<-lm(generic$y,generic$x2)
summary(model2)
plot(generic$y,generic$x2)

model3<-lm(generic$y,generic$x3)
summary(model3)
plot(generic$y,generic$x3)

model4<-lm(generic$y,generic$x4)
summary(model4)
plot(generic$y,generic$x4)

model5<-lm(generic$y,generic$x5)
summary(model5)
plot(generic$y,generic$x5)

model6<-lm(generic$y,generic$x6)
summary(model6)
plot(generic$y,generic$x6)

model7<-lm(generic$y,generic$x7)
summary(model7)
plot(generic$y,generic$x7)

# 3) Create a comprehensive model, model<-lm(y~x1+x2+x3+x4+x5+x6+x7), and then look at
# its performance with summary(model). I am taking interactions off the table for this
# exercise. No x1 x x2 types of terms.

model_all<-lm(generic$y~generic$x1+generic$x2+generic$x3+generic$x4+generic$x5+generic$x6+generic$x7)
summary(model_all)
plot(generic$y,generic$x1+generic$x2+generic$x3+generic$x4+generic$x5+generic$x6+generic$x7)

# 4) Note t-test results for the slope parameter in the regression as an indication of what
# might be important and what might not be in explaining the response Y.


# 5) Now grab the residuals from your model using res<-resid(model) and consider if they
# meet normal assumptions. Plot them in a histogram. Plot them against the response.
# Plot them in a normal quantile plot using qqnorm(x1).

res<-resid(model)
hist(res)
qqnorm(generic$x1)


# 6) Big hints. This model in (3) will not be your best model. A very common transformation
# on Y, given its skewness, will help. If you don't know which, search on transformations
# for skewed-right distributions. Also, one of the predictors is best represented as a
# quadratic (square). Residuals are your clue. Don't bother centering it about its mean.
# Just use the raw x2 for whichever one you choose.

model_all2<-lm(log(generic$y)~(generic$x1+(generic$x2)^2+generic$x3+generic$x4+generic$x5+generic$x6+generic$x7))
summary(model_all2)
plot(log(generic$y),(generic$x1+(generic$x2)^2+generic$x3+generic$x4+generic$x5+generic$x6+generic$x7))

model_all3<-lm(log(generic$y)~(generic$x1+generic$x2+generic$x3+generic$x4+generic$x5+(generic$x6)^2+generic$x7))
summary(model_all3)
plot(log(generic$y),(generic$x1+generic$x2+generic$x3+generic$x4+generic$x5+(generic$x6)^2+generic$x7))

# 7) Now create a comprehensive model on the transformed y. Plotting residuals against
# your predictors will likely suggest a quadratic term and insignificant terms can be
# dropped from your model. We won't eliminate outliers for this exercise.

res2 <- resid(model_all3)
plot(fitted(model_all3),res2)
abline(0,0)


# 8) Report
# a. R2 for your best model
# b. Coefficients of your best model
# c. Explain briefly what led you to the transformation chosen for Y and the quadratic
# for one of the X's.
# d. It will take a bit of work to do the modeling-hey, that's real stat in practice, but
# no more than a ½ of a page is required to explain this.



# Description: Glaucoma data set with many measures taken on an individual to associate with
# whether they have glaucoma eye disease or are healthy. One area of intense activity is machine
# learning / statistical discrimination. This has been going on for decades using discriminant
# analysis, hierarchical clustering, neural nets, etc. In the last two decades or so, classification
# trees have become very popular, especially the more sophisticated application of Random
# Forests. The purpose is to classify groups according to other measures. Can a few measures
# allow you to predict whether the individual has glaucoma or does not? Classification trees begin
# with an impure parent node with all the data, meaning it has both glaucoma and normal
# individuals. In the first pass of the analysis, other measures are considered and an optimum
# split of the data is determined for each alternative measure individually in such a way that the
# resulting child nodes would be purer than the parent node. Then the best measure is chosen to
# make the first split. Subsequent iterations strive to make successive generations of child nodes
# more and more pure. There is a limit of course. We want the model to be parsimonious. One
# way we assess how we are doing with all the data is through the confusion matrix, which details
# correct and incorrect classifications from the model. More sophisticated use of the approach, 
# which we will not do here, involves pruning the tree generated to make it as parsimonious as
# possible while retaining good predictive ability. We would also employ something like n-fold
# cross validation, using a portion of the data to build the model and a portion to test. That is
# good practice to build a less-brittle model (more generally applicable) but I leave those
# discussions for later classes.
# You need to download the package rpart to R or RStudio.
# I will guide you through this, because I haven't taught it, but want you to have an experience
# with a more complex data set. The reporting for credit will be straightforward if you follow my
# lead.

# First, read in a data frame we will call disease using this command. Your computer will respond
# by allowing you to navigate to wherever you have downloaded the glaucoma.csv file to. Select
# glaucoma.csv

disease<-read.csv(file=file.choose(),sep=",",header=TRUE)

# Next, get a sense of what is in your data set.

head(disease,3)

# You see that you have 63 fields. The first 62 are potential predictors. The last is the Class. So
# that we can address Class individually later on, just attach the data frame.

attach(disease)

# So, what we want to focus on are predictor candidates that might do a good job separating
# Class, normal vs glaucoma. These are all numerical, so if we regard as separate populations the
# numerical values of a single variable in the normal vs glaucoma groups, we could screen with a
# two-sample t-test. To save space, we'll just focus on the p-values.

for (i in 1:62){
  x<-disease[,i][Class=="normal"]
  y<-disease[,i][Class=="glaucoma"]
  result[i]<-t.test(x,y,alternative="two.sided")$p.value
  }

# Then to see them easily, with the variable names, list in column form the names and p-values

cbind(colnames(disease)[1:62],result)

# What you see are 62 variables with many p-values <0.05 or <0.01 or even <0.001. How to
# choose? Maybe a significant mean difference is not sufficient to suggest the ability to
# discriminate between classes, but it is a start as we think critically about predictors.

# Let's look at box plots. Create the eyebox function as follows. It is set up to provide 15 displays
# at a time of side by side boxplots, normal vs glaucoma for 15 measures.

names<-colnames(disease)
eyebox<-function(n){
  m<-n+14
  par(mfrow=c(3,5))
    for (i in n:m){
    boxplot(disease[,i]~Class,main=names[i])
    }
}

# Then to run it for the first 15, just execute

eyebox(1)

# You may have to stretch your display window to see the labels clearly. Looking at the boxplots
# you now see that, for example, the very first variable 'ag' is of little use-too much distribution
# overlap between glaucoma and normal. But others, such as "abrn" and "abri" may show
# promise-the distributions have more separation. To look at the first 60 variables (we'll skip the
# last two) run the eyebox function with argument 16, 31, or 46. You may have to open a new
# graphics window prior to each run; it might overwrite your current plot. It depends on R vs
# RStudio and sometimes Mac vs PC. On my Mac in R I have to open a new Quartz window.

eyebox(16)
eyebox(31)
eyebox(46)

# Now that you have a sense of what predictors might be useful, let rpart do the work, splitting
# on each variable to improve the purity of a child node. 

install.packages('rpart')
library(rpart)

fit<-rpart(Class~.,data=disease,method="class")
fit

# When you type fit, it will give you the results of the classification tree. It gives you.
# . Node number
# . Decision rule for the tree
# . Number (total) of records in the node
# . Number of misclassified records in the node
# . Node label (predicted class)
# . Percent glaucoma
# . Percent normal
# . * if it is a terminal node (no more splitting beyond that point)
# This output defines your classification tree. If you are interested, look at the variables that are
# providing decision rules and refer back to their boxplots.

# To clarify how split decisions get you to a specific node for classification, you can use
# path.rpart(fit, node #)

# For example, if I am interested in node 14 I type the following and it tells me successively how
# the data are partitioned to get to that node. You already know from the fit output what the
# class of the node is along with stats.

path.rpart(fit,14)

# Just to gain more familiarity with what is going on and provide a check for the classification
# rules, we could continue with node 14 and apply the rules given

table(Class[varg>=.209 & mhcg<.1695 & vars<.064])

# You see that the 10+9 is the 19 total advertised in the node. And the glaucoma class label
# means that the 9 normal in the node were misclassified. You could do this for other nodes if
# you wish. Just get the path and then use table appropriately.

# Now for what I want you to turn in. A prediction of membership is given by predict. We can use
# it to develop the stats for a confusion matrix. The confusion matrix shows the counts for
# correctly classified and misclassified.

prediction<-predict(fit)
head(prediction)
sum(prediction[,1]<=.5 & Class=="glaucoma")
sum(prediction[,1]>.5 & Class=="glaucoma")
sum(prediction[,2]<=.5 & Class=="normal")
sum(prediction[,2]>.5 & Class=="normal")

