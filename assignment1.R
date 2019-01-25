######### RUN THE CODE BELOW IN R. R-STUDIO IS THE RECOMMENDED IDE. BOTH R AND R-STUDIO ARE FREE.
######### QUESTIONS SHOULD BE POSTED TO PIAZZA
######### THE ACTUAL ASSIGNMENT BEGINS ON LINE 71


### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data structure
head(foo)

# one thing to be very careful with (in this data set) is the use of dates. 8 columns involve dates.

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25)

# these columns need some tweaking--I want to address missing values, calling the blank (empty) 
# elements "NA" instead of leaving them blank, and I wish to tell R these are "Date" objects.

for(i in date.columns)  # this "for loop" only loops through the "date.columns" -- no other columns.
  
{
  
  # identify which values are missing in the "i"th column of the foo data set
  which_values_are_missing <- which(as.character(foo[, i]) == "")
  
  # those values that are missing (blank) in the "i"th column are replaced by <NA>
  # because R knows how to handle "NA" -- NA means something special in R--blanks are handled 
  # more unpredictably (which is bad).
  foo[which_values_are_missing, i] <- NA
  
  # last step--replace each of these columns (which is structured as a column of "factor" values)
  # as a column of dates--i.e., convert them to an object of "class" = Date. They are dates, after all.
  # And if you convert them to the Date class, R will know they are dates and you can manipulate 
  # dates in a simple, straightforward way. Otherwise, you won't be able to easily manipulate them
  # arithmetically.  E.g., for simple Date operations, see lines 48-58 below...
  # **By the way, if you don't understand what a "factor" is in R, you should Google it.** 
  foo[, i] <- as.Date(as.character(foo[, i]))
  
}

# Now R knows that these columns are comprised of dates
# for example...  Replicate this yourself...

# foo[3,12]
# [1] "1968-03-13"

# foo[4,12]
# [1] "1968-07-03"

# foo[3,12] - foo[4,12]
# Time difference of -112 days

# Also, one additional helpful hint... How to eliminate rows with NAs...
# The "is.na" function--for more info, Google it or type ?is.na at the R command prompt in the console.
which.have.NAs <- which(is.na(foo$Rating == TRUE)) # for which rows is the claim "is.na" a TRUE claim?

# Then, if you wanted to, e.g., remove all those rows, retaining only the rows with ratings...
new_foo <- foo[-which.have.NAs, ]

# Notice I called this tweaked data set "new_foo" instead of rewriting over the original data set...
# It's a bit safer to do this, in case I decide I want to quickly revert back to the original data set.

#My code starts here 
#finding out which rows have missing Circulation Date and deleting them 
which.no.date<-which(is.na(new_foo$CirculationDate ==TRUE))
foo2<-new_foo[-which.no.date, ]

#finding out which rows have Circulation Date<2008-01-01 and deleting them 
which.before.date <- which(foo2$CirculationDate<as.Date("2008-01-01"))
foo3<-foo2[-which.before.date,]
head(foo3)

#format(df_withDates$CirculationDate, "%Y")
#dates coerced to only the year 

#SOLVING QUESTION 1 
#since we still have some rows with NA values under Original Completion Data, we will not be able to 
#calculate the difference between the original completion date and the approval date 
#we will remove rows with NA values for the Original Completion Date column 
which.no.completion<-which(is.na(foo3$OriginalCompletionDate ==TRUE))
foo4<-foo3[-which.no.completion,]
head(foo4)

#we would will also need to get rid of rows with no Approval Date and no Revised Completion Date
which.no.approval<-which(is.na(foo4$ApprovalDate ==TRUE))
which.no.revised<-which(is.na(foo4$RevisedCompletionDate ==TRUE))
#since we  notice that there are no rows with missing Approval Date or missing RevisedCompletionDate 
#we do not have to remove them 

#we will now compute the difference between Approval Date and Original Completion Date 
#creating a vector where we will store the time differences 
#this vector will describe the project duration at approval for each project 
difference.1.vector  <-  c()
#this will loop through all the rows of foo4
for (i in  1:nrow(foo4) )
{
  #this is setting, in the place of the current row, the value of the time difference between 
  #OriginalCompletionDate and ApprovalDate, that will be difference.1.vector
  difference.1.vector[i] <- foo4[i,"OriginalCompletionDate"]- foo4[i,"ApprovalDate"]
}
summary(difference.1.vector)
quantile(difference.1.vector)

#since we need to consider how project duration at approval changes over time, and we are suggested to 
#consider projects with earlier and later circulation dates, we will look at the earliest and latest 
#projects according to their Circulation Date, looking at them as the years go by
summary(foo4$CirculationDate)
#from the summary function we can notice that project Circulation Dates go from "2008-01-07" to "2018-06-29" 
#we will now see how project duration at approval has changed throughout the years 

foo.to.year<-format(foo4$CirculationDate, "%Y")
DA2008<-difference.1.vector[which(foo.to.year=="2008")]
DA2009<-difference.1.vector[which(foo.to.year=="2009")]
DA2010<-difference.1.vector[which(foo.to.year=="2010")]
DA2011<-difference.1.vector[which(foo.to.year=="2011")]
DA2012<-difference.1.vector[which(foo.to.year=="2012")]
DA2013<-difference.1.vector[which(foo.to.year=="2013")]
DA2014<-difference.1.vector[which(foo.to.year=="2014")]
DA2015<-difference.1.vector[which(foo.to.year=="2015")]
DA2016<-difference.1.vector[which(foo.to.year=="2016")]
DA2017<-difference.1.vector[which(foo.to.year=="2017")]
DA2018<-difference.1.vector[which(foo.to.year=="2018")]
summary(DA2008)
summary(DA2009)
summary(DA2010)
summary(DA2011)
summary(DA2012)
summary(DA2013)
summary(DA2014)
summary(DA2015)
summary(DA2016)
summary(DA2017)
summary(DA2018)
means<-c(mean(DA2008),
         mean(DA2009),
         mean(DA2010),
         mean(DA2011),
         mean(DA2012),
         mean(DA2013),
         mean(DA2014),
         mean(DA2015),
         mean(DA2016),
         mean(DA2017),
         mean(DA2018))
IQRs<-c(summary(DA2008)[5]-summary(DA2008)[2],
        summary(DA2009)[5]-summary(DA2009)[2],
        summary(DA2010)[5]-summary(DA2010)[2],
        summary(DA2011)[5]-summary(DA2011)[2],
        summary(DA2012)[5]-summary(DA2012)[2],
        summary(DA2013)[5]-summary(DA2013)[2],
        summary(DA2014)[5]-summary(DA2014)[2],
        summary(DA2015)[5]-summary(DA2015)[2],
        summary(DA2016)[5]-summary(DA2016)[2],
        summary(DA2017)[5]-summary(DA2017)[2],
        summary(DA2018)[5]-summary(DA2018)[2])
years<-c(2008:2018)
summary(means)
means


#these plots represent the mean durations at approval of the projects and the interquartile ranges of these 
plot(years,means,pch=16,col='blue',xlab='Year',ylab='Mean Duration',main='Mean Durations of Projects (2008-2018)')
#par(new=TRUE)
plot(years,IQRs,pch=16,col='red',xlab='Year',ylab='IQR of duration',main='Interquartile ranges for Durations of Projects')

#letter B of question 1 

#computing the actual duration 
actual.1.duration<-c()
#this will loop through all the rows of foo4
for (i in  1:nrow(foo4) )
{
  #this is setting, in the place of the current row, the value of the time difference between 
  #RevisedCompletionDate and ApprovalDate 
  actual.1.duration[i] <- foo4[i,"RevisedCompletionDate"]- foo4[i,"ApprovalDate"]
}
summary(actual.1.duration)
#we will compute the difference between the two durations to know how the planned project duration differs from the
#actual duration 
difference.1.durations<-c()
for (i in 1:nrow(foo4))
{
  difference.1.durations[i]<-actual.1.duration[i]-difference.1.vector[i]
}
summary(difference.1.durations)
difference.1.durations
quantile(actual.1.duration)
plot(foo4$CirculationDate,difference.1.durations)

#We can also look at how the mean differences between durations of the projects change year after year 
#organizing the duration differences by year according to Circulation Date 
Df2008<-difference.1.durations[which(foo.to.year=="2008")]
plot(foo4$CirculationDate[which((foo.to.year=="2008"))],Df2008)
summary(Df2008)
Df2009<-difference.1.durations[which(foo.to.year=="2009")]
Df2010<-difference.1.durations[which(foo.to.year=="2010")]
Df2011<-difference.1.durations[which(foo.to.year=="2011")]
Df2012<-difference.1.durations[which(foo.to.year=="2012")]
Df2013<-difference.1.durations[which(foo.to.year=="2013")]
Df2014<-difference.1.durations[which(foo.to.year=="2014")]
Df2015<-difference.1.durations[which(foo.to.year=="2015")]
Df2016<-difference.1.durations[which(foo.to.year=="2016")]
Df2017<-difference.1.durations[which(foo.to.year=="2017")]
Df2018<-difference.1.durations[which(foo.to.year=="2018")]
#calculating the mean of these differences getting this into a vector 
b.meandifferences<-c()
b.meandifferences<-c(mean(Df2008),
                     mean(Df2009),
                     mean(Df2010),
                     mean(Df2011),
                     mean(Df2012),
                     mean(Df2013),
                     mean(Df2014),
                     mean(Df2015),
                     mean(Df2016),
                     mean(Df2017),
                     mean(Df2018))
b.meandifferences
#plotting according to year D
plot(years,b.meandifferences,pch=16,col='blue',xlab='Year',ylab='Difference in Durations',main='Duration and Approval and Actual Duration Difference')

#solving question 2 
#we will figure out what percentage of projects were rated 0, 1, 2, and 3 
#first we will find out which rows have each one of the ratings 
#we will use foo3,since this time we do not have to exclude the ones that have a missing Original Completion Date,
#which happened in foo4. We already removed those projects with NA values for rating so there is nothing 
#else we need to remove for this question
which.2.have.0<-c()
which.2.have.0<-which(foo3$Rating==0)
which.2.have.1<-c()
which.2.have.1<-which(foo3$Rating==1)
which.2.have.2<-c()
which.2.have.2<-which(foo3$Rating==2)
which.2.have.3<-c()
which.2.have.3<-which(foo3$Rating==3)
#then we will figure out which percentage of the current data set (post 2008) they represent 
percentage.2.for.0<-round(length(which.2.have.0)/nrow(foo3)*100)
percentage.2.for.1<-round(length(which.2.have.1)/nrow(foo3)*100)
percentage.2.for.2<-round(length(which.2.have.2)/nrow(foo3)*100)
percentage.2.for.3<-round(length(which.2.have.3)/nrow(foo3)*100)

Rating<-c(0,1,2,3)
Percentage<-c(percentage.2.for.0,percentage.2.for.1,percentage.2.for.2,percentage.2.for.3)
table1<-data.frame(Rating,Percentage)
table1

#solving question 3, we will do the same percentages calculation but we will exclude PPTA projects 
#finding out which rows have PPTA  projects  
to.exclude.PPTA<-which(foo3$Type=='PPTA')
#creating  foo5 which excludes them 
foo5<-foo3[-to.exclude.PPTA,]
#calculating percentages 
which.3.have.0<-c()
which.3.have.0<-which(foo5$Rating==0)
which.3.have.1<-c()
which.3.have.1<-which(foo5$Rating==1)
which.3.have.2<-c()
which.3.have.2<-which(foo5$Rating==2)
which.3.have.3<-c()
which.3.have.3<-which(foo5$Rating==3)
#then we will figure out which percentage of the current data set (post 2008) they represent 
percentage.3.for.0<-round(length(which.3.have.0)/nrow(foo5)*100)
percentage.3.for.1<-round(length(which.3.have.1)/nrow(foo5)*100)
percentage.3.for.2<-round(length(which.3.have.2)/nrow(foo5)*100)
tadapercentage.3.for.3<-round(length(which.3.have.3)/nrow(foo5)*100)
#then we will create the table  
Rating<-c(0,1,2,3)
Percentage1<-c(percentage.3.for.0,percentage.3.for.1,percentage.3.for.2,percentage.3.for.3)
table2<-data.frame(Rating,Percentage1)
table2
#solving question  4 
#We will now identify the top 25% of projects by revised amount, we will go back to foo3, which doesn't exclude
#projects with missing Original completion date or projects that are Type==PPTA
#checking if we had any rows with NA values for Revised Amount in case we need to remove them 
which(is.na(foo3$RevisedAmount))
summary(foo3$RevisedAmount)
quantile(foo3$RevisedAmount)

#based on the quantile  function, we know what the boundaries of the top and bottom 25% of projects by 
#RevisedAmount are. Now we know that the top 25% of projects are the ones that have between 1 (3rd quartile) 
#and 29.86 (max value) as  their final project budget and the bottom 25% are the ones that have between
#0.006 (the minimum value) and 0.4 (the first quartile) 
top.25.percent<-which(foo3$RevisedAmount>=1)
top.25.percent1<-top.25.percent[-543]
bottom.25.percent<-which(foo3$RevisedAmount<=0.4)
ratings.for.top25<-c()
ratings.for.top25<-foo3[top.25.percent,"Rating"]
revised.amounts.top25<-foo3[top.25.percent,"RevisedAmount"]
table(foo3$Rating[top.25.percent])
table(foo3$Rating[bottom.25.percent])
#Comparing the ratings of the projects 

#histograms for rating frequencies of top and bottomg 25% of projects according to Revised Amount 
hist(foo3$Rating[top.25.percent],breaks=3,col='darkgreen',xlab='Project Rating',main='Ratings for top 25%')
hist(foo3$Rating[bottom.25.percent],breaks=3,col='darkgreen',xlab='Project Rating',main='Ratings for bottom 25%')

#Comparing the characteristics of the bottom and top 25% 
#we will slice the data set into  two groups, top and bottom 25% according revised amount and then look at 
#their values in other characteristics 
footop25<-foo3[top.25.percent,]
foobottom25<-foo3[bottom.25.percent,]
#proportions for each possible value of the columns for Dept, Country, Division and LTAA 
table(footop25$Dept)
table(footop25$Country)
table(footop25$Division)
table(footop25$LTAA)

table(foobottom25$Dept)
table(foobottom25$Country)
table(foobottom25$Division)
table(foobottom25$LTAA)