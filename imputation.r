################################
library(mice)
data_long_org=read.csv("Data_Long_Format.csv")

id_Pub_rm=which(data_long_org$Pub_Pri==-1)
id_Loc_rm=which(data_long_org$Locale==-1)
id_rm=union(id_Loc_rm,id_Pub_rm)
data_long=data_long_org[-id_rm,]
data_long=data_long[,-c(1,5)]
data_long=data_long[,-6]


id_te_9=which(data_long$Teacher_Education==-9)
id_pa1_9=which(data_long$Parent1_Education==-9)
data_long$Teacher_Education[id_te_9]=NA
data_long$Parent1_Education[id_pa1_9]=NA

data_raw=data_long[,-c(1,2)]

res.mice.md <- mice(data_raw, m=1,maxit=5)
data_pred=complete(res.mice.md)

data_pred[,-ncol(data_pred)]=round(data_pred[,-ncol(data_pred)])
data_pred$Dates=data_long_org$Dates[-id_rm]
data_pred$Child_Id=data_long$Child_ID
data_pred$School_Id=data_long$School_ID
write.csv(data_pred,"Imputed_Data.csv")

