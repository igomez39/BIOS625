library(mice)
setwd("//Users//nicholashartman//Downloads//Education Data")
data_long_org=read.csv("Data_Long_Format.csv")

data_long=data_long_org[,-c(1,5)]
data_long=data_long[,-c(6,10)]

id_te_9=which(data_long$Teacher_Education==-9)
id_pa1_9=which(data_long$Parent1_Education==-9)
id_math_9=which(data_long$Math_Score==-9)
data_long$Teacher_Education[id_te_9]=NA
data_long$Parent1_Education[id_pa1_9]=NA
data_long$Math_Score[id_math_9]=NA

data_raw=data_long[,-c(1,2)]
data_raw[["Teacher_Education"]]=factor(data_raw[["Teacher_Education"]])
data_raw[["Parent1_Education"]]=factor(data_raw[["Parent1_Education"]])
data_raw[["Pub_Pri"]]=factor(data_raw[["Pub_Pri"]])
data_raw[["Locale"]]=factor(data_raw[["Locale"]])

methods=c("norm.boot","polr","polr","logreg.boot","polyreg","norm.boot")
res.mice.md=mice(data_raw, m=100,maxit=5,method=methods)

for(i in 1:100) {
  dat=complete(res.mice.md,action=i)
  dat$Child_ID=data_long$Child_ID
  dat$Teacher_Education=as.numeric(dat$Teacher_Education)
  dat$Parent1_Education=as.numeric(dat$Parent1_Education)
  dat$Time=as.numeric(dat$Time)
  write.csv(dat,paste("MI",i,".csv",sep=""))
}




