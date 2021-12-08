library(data.table)
library(tidyverse)
library(reldist)
library(parallel)

#Takes in following arguments:
#x: data vector
#fun: function for the statistic of interest
#Returns bootstrap standard error for a statistic
bootstrap=function(x,fun,nit=10000) {
  out=rep(0,nit)
  for(i in 1:nit) {
    out[i]=fun(x[sample.int(length(x),replace=T)])
  }
  return(c(quantile(out,0.025),quantile(out,0.975)))
}

#Read Data
setwd("//Users//nicholashartman//Downloads//Education Data")
dat=as.data.frame(fread("Data_Long_Format.csv",sep=","))
dat=dat %>% select(Time,Math_Scale_Score,Pub_Pri,Locale) %>% na.omit %>% subset(Math_Scale_Score!=-9)

#Wrapper function for bootstrap function
boot_wrap=function(df,fun,nit=10000) {
  return(bootstrap(df[["Math_Scale_Score"]],fun,nit))
}

#Takes in dataset, stratification variable name, and 
#function name for statistic of interest
#Returns a list of bootstrap estimates for each strata x timepoint 
get_boots=function(dat,var,fun,nit=10000) {
  if(var=="Pub_Pri") {
    dat_list=dat %>% group_split(Time,Pub_Pri)
  }
  else {
    dat_list=dat %>% group_split(Time,Locale)
  }
  return(lapply(dat_list,boot_wrap,fun=fun,nit=nit))
}

#Wrapper function for point estimate function
est_wrap=function(df,fun) {
  return(fun(df[["Math_Scale_Score"]]))
}

#Takes in dataset, stratification variable name, and 
#function name for statistic of interest
#Returns a list of point estimates for each strata x timepoint 
get_ests=function(dat,var,fun) {
  if(var=="Pub_Pri") {
    dat_list=dat %>% group_split(Time,Pub_Pri)
  }
  else {
    dat_list=dat %>% group_split(Time,Locale)
  }
  return(mclapply(dat_list,est_wrap,fun=fun))
}

ests=matrix(unlist(get_ests(dat,"Pub_Pri",reldist::gini)),ncol=6)
boots=matrix(unlist(get_boots(dat,"Pub_Pri",reldist::gini,nit=10000)),ncol=12)
lower=matrix(boots[1,],ncol=6)
upper=matrix(boots[2,],ncol=6)

#Read from cluster-generated files
ests=as.matrix(read.csv("ests.csv")[,-1])
boots=as.matrix(read.csv("boots.csv")[,-1])
lower=as.matrix(read.csv("lower.csv")[,-1])
upper=as.matrix(read.csv("upper.csv")[,-1])
ggdata=data.frame(ests=c(ests[1,],ests[2,]),lower=c(lower[1,],lower[2,]),time=c(1:6,1:6),
                  upper=c(upper[1,],upper[2,]),school=c(rep("Public",6),rep("Private",6)))
g=ggplot(ggdata,aes(x=time,y=ests,group=school,color=school))+geom_line()+geom_point()
g=g+geom_errorbar(aes(ymin=lower, ymax=upper))+theme_classic()
g=g+ylab("Gini Index")+xlab("Cohort Assessment")
g=g+scale_x_continuous(breaks=1:6,labels=c("Spring 2011","Spring 2012","Spring 2013",
                                            "Spring 2014","Spring 2015","Spring 2016"))
g=g+theme(text=element_text(size=15))+labs(color="School Type")
g

