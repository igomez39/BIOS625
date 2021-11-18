library(tidyverse)
library(data.table)

#Read Data
setwd("Add working directory here")
dat=as.data.frame(fread("educ_puf.csv",sep=","))

#####################################################
#Select Variables of Interest and Create Long Dataset
#####################################################
#Add variable names here
#For variables that are measured only once, make sure the name doesn't start with an "X" (rename if needed)
vars=c("CHILDID","S1_ID","S2_ID","S3_ID","S4_ID","S5_ID","S6_ID","S7_ID","S8_ID","S9_ID",
       "X1MTHETK5","X2MTHETK5","X3MTHETK5","X4MTHETK5","X5MTHETK5","X6MTHETK5","X7MTHETK5",
       "X8MTHETK5","X9MTHETK5","X1PUBPRI","X2PUBPRI","X3PUBPRI","X4PUBPRI","X5PUBPRI",
       "X6PUBPRI","X7PUBPRI","X8PUBPRI","X9PUBPRI","X1LOCALE","X2LOCALE","X3LOCALE",
       "X4LOCALE","X5LOCALE","X6LOCALE","X7LOCALE","X8LOCALE","X9LOCALE","A4HGHSTD",
       "X12PAR1ED_I","X12PAR2ED_I")

#Takes in a vector of integers (1 through 9)
#Returns the corresponding follow-up date in readable format
Get_Dates=function(indices) {
  dates=c("Fall 2010","Spring 2011","Fall 2011","Spring 2012",
          "Fall 2012","Spring 2013","Spring 2014","Spring 2015","Spring 2016")
  return(dates[indices])
}

#Takes in a vector of times (1 through 9)
#Shifts the times so that Spring 2011 is time 1
Shift_Times=function(times) {
  times[which(times==2)]=1
  times[which(times==4)]=2
  times[which(times==6)]=3
  times[which(times==7)]=4
  times[which(times==8)]=5
  times[which(times==9)]=6
  return(times)
}

#Takes in a vector of data values and vector of "Not Ascertained" codes
#Sets "Not Ascertained" to NA value
Not_Ascertained_to_NA_Value=function(x,codes) {
  x[x %in% codes]=NA
  return(x)
}

dat_long=dat %>% select(vars) %>% 
                 rename(Parent1_Education=X12PAR1ED_I,
                        Parent2_Education=X12PAR2ED_I,Teacher_Education=A4HGHSTD,
                        X1_ID=S1_ID,X2_ID=S2_ID,X3_ID=S3_ID,X4_ID=S4_ID,X5_ID=S5_ID,
                        X6_ID=S6_ID,X7_ID=S7_ID,X8_ID=S8_ID,X9_ID=S9_ID) %>%
                 pivot_longer(starts_with("X"),
                              names_to=c("Time",".value"),names_pattern="(X[1-9])(.+)") %>%
                 mutate(Time=as.numeric(substr(Time,2,2)),Math_Score=MTHETK5,Pub_Pri=PUBPRI,
                        Locale=LOCALE,School_ID=`_ID`,Child_ID=CHILDID) %>%
                 mutate(Dates=Get_Dates(Time),Time=Shift_Times(Time),
                        Pub_Pri=Not_Ascertained_to_NA_Value(Pub_Pri,c(-9)),
                        Locale=Not_Ascertained_to_NA_Value(Locale,c(-9))) %>%
                 select(Child_ID,School_ID,Time,Dates,Teacher_Education,
                        Parent1_Education,Parent2_Education,Pub_Pri,Locale,Math_Score) %>%
                 subset(Dates %in% c("Spring 2011","Spring 2012","Spring 2013",
                                     "Spring 2014","Spring 2015","Spring 2016")) 
write.csv(dat_long,"Data_Long_Format.csv")

