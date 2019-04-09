#Causal Inference Assignment 
#Marcela Radilla Deloya 

#Question 2
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]

# check that all missing data is gone...
which(is.na(foo) == TRUE)

# take a peek at the data set (identify the columns)
head(foo)
names(foo)

#from replication.do
#logistic pbs2s3 wartype logcost wardur factnum factnum2 trnsfcap untype4 treaty develop exp decade
#model without interaction term 
glm1 <- glm(pbs2s3~wartype + logcost + wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade, data = foo, family = "binomial")
summary(glm1)
#Model with interaction term 
glm2 <- glm(pbs2s3~wartype + logcost + wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4), data = foo, family = "binomial")

marginal1 <- c()
marginal2 <- c()
marginal3 <- c()
marginal4<-c()

#THIS CODE IS THE ONE THAT INCLUDES THE INTERACTION TERM ON THE X AXIS AND ITERATES OVER LOGCOST
for (i in 1:20){
  p1<-data.frame(wartype=mean(foo$wartype),logcost=i,wardur=mean(foo$wardur),
                 factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=0,
                 treaty=mean(foo$treaty),develop=mean(foo$develop),
                 exp=mean(foo$exp),decade=mean(foo$decade))
  
  p2<-data.frame(wartype=mean(foo$wartype),logcost=i,wardur=mean(foo$wardur),
                 factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=1,
                 treaty=mean(foo$treaty),develop=mean(foo$develop),
                 exp=mean(foo$exp),decade=mean(foo$decade))
  marginal3[i]<-predict(glm1,p2,type='response')-predict(glm1,p1,type='response')
  marginal4[i]<-predict(glm2,p2,type='response')-predict(glm2,p1,type='response')
}
plot(1:20, marginal4,type='l',col='red',xlab='Log of death and displacement',ylab='Marginal effects of UN peacekeeping operations')
lines(1:20, marginal3,col='blue')
legend(1,0.7,legend=c('Original Model','Model with interaction term'), fill=c("blue",'red'), cex=0.8)

#THIS CODE DOES NOT INCLUDE THE INTERACTION TERM IN THE X-AXIS AND IT ITERATES OVER THE WAR DURATION VARIABLE
for (i in 1:315){
  p1<-data.frame(wartype=mean(foo$wartype),logcost=mean(foo$logcost),wardur=i,
                 factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=0,
                 treaty=mean(foo$treaty),develop=mean(foo$develop),
                 exp=mean(foo$exp),decade=mean(foo$decade))
  
  p2<-data.frame(wartype=mean(foo$wartype),logcost=mean(foo$logcost),wardur=i,
                 factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=1,
                 treaty=mean(foo$treaty),develop=mean(foo$develop),
                 exp=mean(foo$exp),decade=mean(foo$decade))
  marginal1[i]<-predict(glm1,p2,type='response')-predict(glm1,p1,type='response')
  marginal2[i]<-predict(glm2,p2,type='response')-predict(glm2,p1,type='response')
}

plot(1:315, marginal2,type='l',col='red',xlab='Duration of wars in months',ylab='Marginal effects of UN peacekeeping operations',xlim=c(0, 315), ylim=c(0.0, 0.8))
lines(1:315, marginal1,col='blue')
legend(197,0.8,legend=c('Original Model','Model with interaction term'), fill=c("blue",'red'), cex=0.8)

plot(1:315, marginal2,type='l',col='red',xlab='Duration of wars in months',ylab='Marginal effects of UN peacekeeping operations')
lines(1:315, marginal1,col='blue')
legend(197,0.8,legend=c('Original Model','Model with interaction term'), fill=c("blue",'red'), cex=0.8)

#Question 3 
foo_orig <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
names(foo_orig)
# extract relevant columns + adding column for the variable uncint, pbs2l,pbs5l
foo <- foo_orig[, c(6:8, 11:16, 99, 50, 114, 34,35,49, 52,63, 136, 109, 126, 48, 160, 142, 10)]
foo$pbs5l
# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47,-4,-16,-84,-98,-93), ]

# check that all missing data is gone...

which(is.na(foo) == TRUE)
# take a peek at the data set (identify the columns)
head(foo)
names(foo)

#defining treatment with the variable uncint 

#prompt:

#Tr <- rep(0, length(foo$uncint))
#Tr[which(foo$uncint != 0 & foo$uncint != 1)] <- 1

#The first line of code is creating a vector of zeroes with length equal to the number of values 
#existent for the uncint variable
#uncint is a variable whose value describes the level of intrusiveness of the UN action
#A value of 0 is the least intrusive form of action and a value of 4 is the most intrusive one. 
#The second line of code is making all the values where the value of uncint is not 1 and not 0 equal to 1. 
#This leaves us with a vector where all the values of uncint that are either 1 or 0 are equal to 0 and 
#all values that are either 2, 3, or 4 are equal to 1. 


#since the variable uncint is formed with character data, the code above will not work. We will turn that character data into 0s and 1s
Tr <- rep(0, length(foo$uncint))
a<-which(foo$uncint=='PKO')
b<-which(foo$uncint=='Enforcement')
c<-which(foo$uncint=='Observer')
Tr[a] <- 1
Tr[b]<-1
Tr[c]<-1
foo$uncint
#now we have a vector that has zeroes for UN operations coded as 'None' or 'Observer' 
#and 1s for UN operations registered as 'PKO' or 'Enforcement'


#Question 4 
library(Matching)
#creating a new vector to turn the character data from foo$pbs5l into zeroes and ones 

#LOGISTIC REGRESSION
#here I will get the tmt effect estimate for 2 years after the war
glm3 <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + 
              treaty + develop + exp + decade, family = binomial, data = foo)

summary(glm3) 
#here I will get the tmt effect estimate for 5 years after the war
glm5<-glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + 
            treaty + develop + exp + decade, family = binomial, data = foo)

summary(glm5)
glm3mb <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, nboots = 1000)
summary(glm3mb)


#PROPENSITY SCORE 
foo$Tr = Tr
glm4 <- glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, family = 'binomial', data = foo)
summary(glm4)
matchy1 <- Match(X = glm4$fitted, Tr = Tr, Y = foo$pbs2l)
matchy2 <- Match(X = glm4$fitted, Tr = Tr, Y = foo$pbs5l)
summary(matchy1) 
summary(matchy2) 
mb1  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = matchy1, nboots = 1000)
mb2 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = matchy2, nboots = 1000)
matchy3<- Match(X = glm4$fitted, Tr = Tr, Y = foo$pbs2l,BiasAdjust = TRUE)
matchy4 <- Match(X = glm4$fitted, Tr = Tr, Y = foo$pbs5l,BiasAdjust = TRUE)
summary(matchy3)
summary(matchy4)
mb3  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = matchy3, nboots = 1000)
mb4  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = matchy4, nboots = 1000)

#GENETIC MATCHING 

X=cbind(foo$wartype,foo$logcost,foo$wardur,foo$factnum,foo$factnum2,
        foo$trnsfcap,foo$treaty,foo$develop,foo$exp,foo$decade)

#BalanceMat=cbind(foo$wartype,foo$logcost,foo$wardur,foo$factnum,foo$factnum2,foo$trnsfcap,foo$treaty,foo$develop,foo$exp,foo$decade)
genout1 <- GenMatch(Tr=Tr, X=X, pop.size=200, wait.generations = 25)
mout1<-Match(Tr=Tr, X=X, Weight.matrix=genout1,Y=foo$pbs2l)
summary(mout1)
mout2<-Match(Tr=Tr, X=X, Weight.matrix=genout1,Y=foo$pbs5l)
summary(mout2)
mout3<-Match(Tr=Tr, X=X, Weight.matrix=genout1,Y=foo$pbs2l, BiasAdjust = TRUE)
summary(mout3)
mout4<-Match(Tr=Tr, X=X, Weight.matrix=genout1,Y=foo$pbs5l, BiasAdjust = TRUE)
summary(mout4)
mbb1 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = mout1, nboots = 5000)
mdd1 <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo, match.out = mout2, nboots = 5000)