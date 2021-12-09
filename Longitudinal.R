library(nlme)
library(ggplot2)
library(knitr)
impdata=read.csv("Imputed_Data.csv")
impdata=impdata[,-1]
sum(is.na(impdata))

p1<-ggplot(impdata,aes(y=Math_Score,x=Time,group=as.factor(Pub_Pri)))+
  # geom_line(aes(group=factor(id)),col="red")+
  stat_summary(aes(color = as.factor(Pub_Pri),linetype=as.factor(Pub_Pri)), geom = "line", fun = mean,size = 1)+
  stat_summary(aes(shape = as.factor(Pub_Pri)), geom = "point", fun = mean, size = 2)+
  xlab("Time")+ylab("Math Score") 

p1$labels$fill="School Type"
p1$labels$colour="School Type"
p1$labels$linetype="School Type"
p1$labels$shape="School Type"

p2<-ggplot(impdata,aes(y=Math_Score,x=Time,group=as.factor(Locale)))+
    # geom_line(aes(group=factor(id)),col="red")+
    stat_summary(aes(color = as.factor(Locale),linetype=as.factor(Locale)), geom = "line", fun = mean,size = 1)+
    stat_summary(aes(shape = as.factor(Locale)), geom = "point", fun = mean, size = 2)+
    xlab("Time")+ylab("Math Score") 
p2$labels$fill="Locale"
p2$labels$colour="Locale"
p2$labels$linetype="Locale"
p2$labels$shape="Locale"

  

p3<- ggplot(impdata,aes(y=Math_Score,x=Time,fill=as.factor(Teacher_Education)))+
    stat_summary(aes(color = as.factor(Teacher_Education),linetype=as.factor(Teacher_Education)), geom = "line", fun = mean,size = 1)+
    stat_summary(aes(shape = as.factor(Teacher_Education)), geom = "point", fun = mean, size = 2)+
    xlab("Time")+ylab("Math Score") 

p3$labels$fill="Teacher Education"
p3$labels$colour="Teacher Education"
p3$labels$linetype="Teacher Education"
p3$labels$shape="Teacher Education"

p4<- ggplot(impdata,aes(y=Math_Score,x=Time,group=as.factor(Parent1_Education)))+
    # geom_line(aes(group=factor(id)),col="red")+
    stat_summary(aes(color = as.factor(Parent1_Education),linetype=as.factor(Parent1_Education)), geom = "line", fun = mean,size = 1)+
    stat_summary(aes(shape = as.factor(Parent1_Education)), geom = "point", fun = mean, size = 2)+
    xlab("Time")+ylab("Math Score") 

  
p4$labels$fill="Parent Education"
p4$labels$colour="Parent Education"
p4$labels$linetype="Parent Education"
p4$labels$shape="Parent Education"
  
  
margin = theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p1=p1+margin
p2=p2+margin
p3=p3+margin
p4=p4+margin
g<- grid.arrange(p1,p2,p3,p4,ncol=2)  # default settings


ggsave("Imputed.png",g,width=8,height=6)

#####
View(impdata)
attach(impdata)

model1<-lme(data=impdata,Math_Score ~ Time*Teacher_Education
           + Time*Locale + Time* Pub_Pri 
            +  Parent1_Education*Time 
            , random = ~ Time| Child_Id)
model_red<-lme(data=impdata,Math_Score ~ Time*Teacher_Education
            + Time*Locale + Time* Pub_Pri 
            +  Parent1_Education*Time 
            , random = ~ 1| Child_Id)

anova(model1,model_red)
summary=summary(model1)
kable(summary$tTable)














