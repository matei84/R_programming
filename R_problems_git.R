In this script I tried to create a class "LongitudinalData". I will do it through S3.

The first step, I will create a method "makeLD" to transform the data from data.frame to longitudinal. 
I also create the method print (not big deal)

The second step the method subject with extract all the data for a particular person. There the problems start:

Although when I create the method subject, the function "str" gives me the suggestion that I am doing well,
It is impossible for me to see the data, it did not return anything (I would like to know why I can not see the data).

The second problem, the summary method. I tried to create a summary function (The way that I am looking for
is in the "expected_outcome.R" file) for subject(data, 14) and it does not return what I want, which is 
in the  "expected_outcome.R" file.  


#########################
install.packages("tidyverse")
library(tidyverse)

data1<-read_csv("MIE.csv")

class(data1)
summary(data1)

#############################################################

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#creating the "MAKE_LD" method:

make_LD<-function(data) UseMethod("make_LD")

make_LD.data.frame<-function(data){structure(list(id=data$id,
								   visit=data1$visit,
								   room=data1$room,
								   value=data1$value,
								   timepoint=data1$timepoint),
								  class=c("LongitudinalData", "data.frame"))}

data1_LD<-make_LD(data1)

print.LongitudinalData<-function(x){
	
	paste("Longitudinal dataset with", nlevels(factor(x$id)), "subjects")
						}

print(data1_LD)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Creating the method "SUBJECT":

subject<-function(data, n) UseMethod("subject")

subject.LongitudinalData<-function(data, n=c(14, 20, 41, 44, 46, 54, 64, 74, 104, 106)) 
					     {
						structure(lapply(data, function(x) x[which (data$id==n)]),
						class = c("subject", "LongitudinalData", "data.frame"))}		
						
						
print.subject<-function(x){
	
	paste("subject ID: ", x$id[[c(1)]])}
					

print(subject(data1_LD, 14))

sub_x<-subject(data1_LD, 14)

str(sub_x)


# The way that I tink close to do what I want is near this: https://www.r-bloggers.com/pivot-tables-in-r/

summary.subject<-function(data){
			 
			n=c(14, 20, 41, 44, 46, 54, 64, 74, 104, 106)) 
					     {
						structure(lapply(data, function(x) x[which (data$id==n)]),
						class = c("subject", "LongitudinalData"))	}

