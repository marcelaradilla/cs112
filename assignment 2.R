#Question 1 
#coming up with a formula including 1 dependent variable, 1 predictor variable, and a random error term 
#'a' will be the dependent variable given y a formula including the independent variable 'b'
set.seed(1522)
b<-c()
b<-sample(-5000:5000, size=999, replace=TRUE) #random independent variable 
a<-c()
a<-0.52*b+rnorm(999, mean=0, sd=20) #data generating formula with added normal error term 
summary(a)
plot(b,a,cex=0.05,xlab='Predictor',ylab='Response')
regression1<-lm(a~b)
View(regression1)
abline(regression1,col='blue')
legend(-2000,2700,legend=c('Regression 1', 'Regression 2'), col=c("blue", "red"),lty=1:1, cex=0.8)
summary(regression1)
b<-c(b,-550000)
a<-c(a,221000)
regression2<-lm(a~b)
summary(regression2)
abline(regression2,col='red')

#Question 2 
set.seed(456)
library(Matching) 
data("lalonde") #loading the data 
names(lalonde) #checking the title of the treatment column  
toremove<-which(lalonde$treat==1) #finding out which rows are part of the treatment group 
newdata<-lalonde[-toremove,] #removing rows to only work with the control group 
#age, educ, re74, re75, educ*re74, educ*re75, age*re74, age*re75, age*age, and re74*re75 
reg3<-lm(re78~age+educ+re74+re75+I(educ*re74)+ I(educ*re75)+I(age*re74)+I(age*re75)+I(age*age)+I(re74*re75),data=newdata) 
summary(reg3) #getting the summary of the regression model 
library(arm)
reg3$coef
sim_results1 <- sim(reg3, n.sims = 10000) #simulating 10K coefficients 
summary(newdata$educ) #using the summary functions to get the medians and 75% of education, re74 and re75 
summary(newdata$re74)
summary(newdata$re75)
sigmavalues<-sigma.hat(sim_results1)
ed_median <- median(newdata$educ)
re74_median <- median(newdata$re74)
re75_median <- median(newdata$re75)
ed_75<-11
re74_75<-139.4
re75_75<-650.1
#educ, re74, and re75
storage1<-c()
storage2<-c()
storage3<-c()
storage4<-c()
storage5<-c()
storage6<-c()
storage7<-c()
storage8<-c()
for (i in 17:55)
{
  storage_vector<-c()
  storage_vector2<-c()
  for(t in 1:10000)
  {
    storage_vector[t]<-sim_results1@coef[t,1]+sim_results1@coef[t,2]*i+ed_median*sim_results1@coef[t,3]+sim_results1@coef[t,4]*re74_median+sim_results1@coef[t,5]*re75_median+sim_results1@coef[t,6]*ed_median*re74_median+sim_results1@coef[t,7]*ed_median*re75_median+sim_results1@coef[t,8]*i*re74_median+sim_results1@coef[t,9]*i*re75_median+sim_results1@coef[t,10]*i*i+sim_results1@coef[t,11]*re74_median*re75_median
    storage_vector2[t]<-sim_results1@coef[t,1]+sim_results1@coef[t,2]*i+ed_median*sim_results1@coef[t,3]+sim_results1@coef[t,4]*re74_75+sim_results1@coef[t,5]*re75_75+sim_results1@coef[t,6]*ed_75*re74_75+sim_results1@coef[t,7]*ed_75*re75_75+sim_results1@coef[t,8]*i*re74_75+sim_results1@coef[t,9]*i*re75_75+sim_results1@coef[t,10]*i*i+sim_results1@coef[t,11]*re74_75*re75_75
    
  }
  storage1[i-16]<-quantile(storage_vector, probs = c(0.025, 0.975))[1]
  storage2[i-16]<-quantile(storage_vector, probs = c(0.025, 0.975))[2]
  storage3[i-16]<-quantile(storage_vector2, probs = c(0.025, 0.975))[1]
  storage4[i-16]<-quantile(storage_vector2, probs = c(0.025, 0.975))[2]
  storage5[i-16]<-quantile(storage_vector+rnorm(10000, 0, sigmavalues), probs = c(0.025, 0.975))[1]
  storage6[i-16]<-quantile(storage_vector+rnorm(10000, 0, sigmavalues), probs = c(0.025, 0.975))[2]
  storage7[i-16]<-quantile(storage_vector2+rnorm(10000, 0, sigmavalues), probs = c(0.025, 0.975))[1]
  storage8[i-16]<-quantile(storage_vector2+rnorm(10000, 0, sigmavalues), probs = c(0.025, 0.975))[2]
}
#predicted values are those that do not include the simulated errors 
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(0,13000), 
     main = "Predicted Real Earnings in 1978", xlab = "Age (years)", 
     ylab = "Real Earnings in 1978 (USD)")

for (i in 17:55) {
  segments(
    x0 = i,
    y0 = storage1[i-16],
    x1 = i,
    y1 = storage2[i-16],
    lwd = 2,col='red')
}
for (i in 17:55) {
  segments(
    x0 = i,
    y0 = storage3[i-16],
    x1 = i,
    y1 = storage4[i-16],
    lwd = 2,col='blue')
}
legend(20,10000,legend=c('Predictors at 50%','Predictors at 75%'), fill=c("red",'blue'), cex=0.8)
#expected values are those that include the simulated sigmas 
plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,55), ylim = c(-9000,19000), 
     main = "Expected Real Earnings in 1978", xlab = "Age (years)", 
     ylab = "Real Earnings in 1978 (USD)")
for (i in 17:55) {
  segments(
    x0 = i,
    y0 = storage5[i-16],
    x1 = i,
    y1 = storage6[i-16],
    lwd = 2,col='red')
}
for (i in 17:55) {
  segments(
    x0 = i,
    y0 = storage7[i-16],
    x1 = i,
    y1 = storage8[i-16],
    lwd = 2,col='blue')
}
legend(18,19000,legend=c('Predictors at 50%','Predictors at 75%'), fill=c("red",'blue'), cex=0.8)
pred50<-data.frame("Age"=(17:55),"LowerBound50"=storage1,"Upper Bound50"=storage2) #creating some dataframes to display them as tables
pred75<-data.frame("Age"=(17:55),"LowerBound75"=storage3,"Upper Bound75"=storage4)

#Question 3 
set.seed(1998)
data('PlantGrowth')
PlantGrowth
toremove1<-which(PlantGrowth$group=='trt2')
newplants<-PlantGrowth[-toremove1,]
treatmentrows<-which(newplants$group=='trt1')
controlrows<-which(newplants$group=='ctrl')
new_group<-c()
for (i in treatmentrows)
{
  new_group[i]<-1
}
for (i in controlrows)
{
  new_group[i]<-0
}
new_group
new_weight<-newplants$weight
new_weight
newplants1<-data.frame('weight1'=new_weight,'group1'=new_group)
reg4<-lm(weight1 ~group1, data = newplants1)
summary(reg4)
confint(reg4)
reg4$coefficients[1]

storageplants <- c()
storageintercept<-c()
for(i in 1:10000)
{
  
  storageplants[i] <- coef(lm(weight1 ~group1, data = newplants1[sample(20, replace = TRUE), ]))[2]
  storageintercept[i]<-coef(lm(weight1 ~group1, data = newplants1[sample(20, replace = TRUE), ]))[1]
  
}
hist(storageplants, main='Bootstrap-sample Results', xlab='Group Coefficient', col='darkgreen')
hist(storageintercept)
quantile(storageintercept, probs = c(0.025, 0.975))
quantile(storageplants, probs = c(0.025, 0.975))

#Question 4 
#my function 
getrsquared<-function(ys,predys){ 
  sse<-sum(((ys-predys)^2)) #getting the residual sum of squares 
  sst<-sum(((ys-mean(ys))^2)) #getting the total sum of squares 
  return(1-(sse/sst)) #computing r squared 
}


#now I will show it works with the plantgrowth dataset 
predictions1<-c() #creating an empty vector to put in the predicted y's
for (i in 1:20){
  predictions1[i]<-reg4$coefficients[1]+reg4$coefficients[2]*new_group[i]
} #I am generating the predicted y's using the regression model I had before, reg4 

getrsquared(new_weight,predictions1) #now I input the actual y's and the predicted y's vectors into the function 
summary(reg4) #and now I compare it with the one that the regression model gave me originally 

#Question 5 
set.seed(12211)
library(foreign)
q5data<-read.dta('nsw.dta')
names(q5data)
treatmentpeople<-which(q5data$treat=='1')
controlpeople<-which(q5data$treat=='0')
treatpart<-q5data[treatmentpeople,]
controlpart<-q5data[controlpeople,]
reg5<-glm(treat~age+education+black+hispanic+married+nodegree+re75, family=binomial,data=q5data)
summary(reg5)
predicted1<-predict(reg5,type='response')
hist1<-predicted1[treatmentpeople]
hist(hist1,main='Treatment Group Propensity Scores',xlab='Propensity Score',col='red',labels=TRUE)
legend(0.5,80,legend=c('Treatment','Control'), fill=c("red",'blue'), cex=0.8)
hist2<-predicted1[controlpeople]
hist(hist2, main='Control Group Propensity Scores',xlab='Propensity Score',col='blue',labels=TRUE)
legend(0.47,100,legend=c('Treatment','Control'), fill=c("red",'blue'), cex=0.8)
