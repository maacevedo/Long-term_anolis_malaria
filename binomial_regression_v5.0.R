#setwd("/Volumes/QE/Working_folder/UPR_Prof/Luisa")
library(AICcmodavg)
library(extrafont)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
require(effects)
require(dplyr)


source('Enviro_summer.R')

source('Enviro_winter.R')

#print environmental data
enviro_summer
enviro_winter

#Input disease data
#anolis-plasmodium

data=read.csv("Data/Data_1991_2016.csv")


ds=data[data$season=="summer",]
dw=data[data$season=="winter",]

########################
#Format data
########################

#summer
ds_pos=c(length(ds$infection[ds$infection==1 & ds$year=="1990"]),length(ds$infection[ds$infection==1 & ds$year=="1996"]),length(ds$infection[ds$infection==1 & ds$year=="1997"]),length(ds$infection[ds$infection==1 & ds$year=="1998"]),length(ds$infection[ds$infection==1 & ds$year=="2015"]),length(ds$infection[ds$infection==1 & ds$year=="2016"]))

ds_neg=c(length(ds$infection[ds$infection==0 & ds$year=="1990"]),length(ds$infection[ds$infection==0 & ds$year=="1996"]),length(ds$infection[ds$infection==0 & ds$year=="1997"]),length(ds$infection[ds$infection==0 & ds$year=="1998"]),length(ds$infection[ds$infection==0 & ds$year=="2015"]),length(ds$infection[ds$infection==0 & ds$year=="2016"]))

prop_ds=ds_pos/(ds_pos+ds_neg)

#winter
dw_pos=c(length(dw$infection[dw$infection==1 & dw$year=="1991"]),length(dw$infection[dw$infection==1 & dw$year=="1997w"]),length(dw$infection[dw$infection==1 & dw$year=="1998w"]),length(dw$infection[dw$infection==1 & dw$year=="1999"]),length(dw$infection[dw$infection==1 & dw$year=="2001"]),length(dw$infection[dw$infection==1 & dw$year=="2002"]),length(dw$infection[dw$infection==1 & dw$year=="2016w"]))

dw_neg=c(length(dw$infection[dw$infection==0 & dw$year=="1991"]),length(dw$infection[dw$infection==0 & dw$year=="1997w"]),length(dw$infection[dw$infection==0 & dw$year=="1998w"]),length(dw$infection[dw$infection==0 & dw$year=="1999"]),length(dw$infection[dw$infection==0 & dw$year=="2001"]),length(dw$infection[dw$infection==0 & dw$year=="2002"]),length(dw$infection[dw$infection==0 & dw$year=="2016w"]))

prop_dw=dw_pos/(dw_pos+dw_neg)

enviro_summer$prevalence=prop_ds
enviro_summer$positive=ds_pos
enviro_summer$negative=ds_neg

enviro_winter$prevalence=prop_dw
enviro_winter$positive=dw_pos
enviro_winter$negative=dw_neg

enviro_all=rbind(enviro_summer,enviro_winter)
enviro_all=enviro_all[order(enviro_all[,1]),]

#####################################
#All together
#####################################

cand.models.all=list()

#single variables
cand.models.all[[1]]=glm(cbind(enviro_all$positive,enviro_all$negative)~1,family="binomial")

cand.models.all[[2]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_maxT30,family="binomial")

cand.models.all[[3]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_maxT30,family="binomial")

cand.models.all[[4]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_maxT120,family="binomial")

cand.models.all[[5]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_maxT120,family="binomial")

cand.models.all[[6]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_minT30,family="binomial")

cand.models.all[[7]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_minT30,family="binomial")

cand.models.all[[8]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_minT120,family="binomial")

cand.models.all[[9]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_minT120,family="binomial")

cand.models.all[[10]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30,family="binomial")

cand.models.all[[11]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_rain30,family="binomial")

cand.models.all[[12]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120,family="binomial")

cand.models.all[[13]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$var_rain120,family="binomial")

#combinations

cand.models.all[[14]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_maxT120,family="binomial")

cand.models.all[[15]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_maxT30,family="binomial")

#bestmodel
cand.models.all[[16]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_minT30,family="binomial")
#best

cand.models.all[[17]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_minT120,family="binomial")


cand.models.all[[18]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_maxT120,family="binomial")

cand.models.all[[19]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_maxT30,family="binomial")

cand.models.all[[20]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_minT30,family="binomial")

cand.models.all[[21]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_minT120,family="binomial")

#Squared effects################################################################
cand.models.all[[22]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_maxT30+I(enviro_all$mean_maxT30^2),family="binomial")

cand.models.all[[23]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_maxT120+I(enviro_all$mean_maxT120^2),family="binomial")

cand.models.all[[24]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_minT30+I(enviro_all$mean_minT30^2),family="binomial")

cand.models.all[[25]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_minT120+I(enviro_all$mean_minT120^2),family="binomial")

cand.models.all[[26]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+I(enviro_all$mean_rain30^2),family="binomial")

cand.models.all[[27]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+I(enviro_all$mean_rain120^2),family="binomial")

cand.models.all[[28]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_maxT120+I(enviro_all$mean_rain30^2)+I(enviro_all$mean_maxT120^2),family="binomial")

cand.models.all[[29]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_maxT30+I(enviro_all$mean_rain30^2)+I(enviro_all$mean_maxT30^2),family="binomial")


cand.models.all[[30]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_minT30+I(enviro_all$mean_rain30^2)+I(enviro_all$mean_minT30^2),family="binomial")


cand.models.all[[31]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain30+enviro_all$mean_minT120+I(enviro_all$mean_rain30^2)+I(enviro_all$mean_minT120^2),family="binomial")


cand.models.all[[32]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_maxT120+I(enviro_all$mean_rain120^2)+I(enviro_all$mean_maxT120^2),family="binomial")

cand.models.all[[33]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_maxT30+I(enviro_all$mean_rain120^2)+I(enviro_all$mean_maxT30^2),family="binomial")

cand.models.all[[34]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_minT30+I(enviro_all$mean_rain120^2)+I(enviro_all$mean_minT30^2),family="binomial")

cand.models.all[[35]]=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_minT120+I(enviro_all$mean_rain120^2)+I(enviro_all$mean_minT120^2),family="binomial")


modnames=c("Null","mean_maxT30","var_maxT30","mean_maxT120","var_maxT120","mean_minT30","var_minT30","mean_minT120","var_minT120","mean_rain30","var_rain30","mean_rain120","var_rain120","mean_rain30+mean_maxT120","mean_rain30+mean_maxT30","mean_rain30+mean_minT30","mean_rain30+mean_minT30","mean_rain120+mean_maxT120","mean_rain120+mean_maxT30","mean_rain120+mean_minT30","mean_rain120+mean_minT30","mean_maxT30^2","mean_maxT120^2","mean_minT30^2","mean_minT120^2","mean_rain30^2","mean_rain120","mean_rain30^2+mean_maxT120^2","mean_rain30^2+mean_maxT30^2","mean_rain30^2+mean_minT30^2","mean_rain30^2+mean_minT120^2","mean_rain120^2+mean_maxT120^2","mean_rain120^2+mean_maxT30^2","mean_rain120^2+mean_minT30^2","mean_rain120^2+mean_minT120^2")

aictab(cand.set=cand.models.all,modnames=modnames,sort=TRUE)
#32 is best model




############################
#shade figs
############################
#bm=glm(cbind(enviro_all$positive,enviro_all$negative)~enviro_all$mean_rain120+enviro_all$mean_maxT120+I(enviro_all$mean_rain120^2)+I(enviro_all$mean_maxT120^2),family="binomial")
x1=enviro_all$mean_rain120
x2=enviro_all$mean_maxT120
mod=glm(cbind(enviro_all$positive,enviro_all$negative)~x1+x2+I(x1^2)+I(x2^2),family="binomial")
y=cbind(enviro_all$positive,enviro_all$negative)

x1=enviro_all$mean_rain120
x2=enviro_all$mean_maxT120
mod=glm(cbind(enviro_all$positive,enviro_all$negative)~x1+x2+I(x1^2)+I(x2^2),family="binomial")
y=cbind(enviro_all$positive,enviro_all$negative)

fit.eff_x1 <- Effect("x1", mod,xlevels=1000) %>% as.data.frame() #mean_rain120
fit.eff_x2 <- Effect("x2", mod,xlevels=1000) %>% as.data.frame() #mean_max_temp120

P1=ggplot(fit.eff_x2,aes(x2))+
geom_line(aes(y=fit.eff_x2$fit),colour="black")+
geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2)+labs(x="temperature (°C)",y="proportion infected")+theme(plot.title=element_text(face="bold", size=16, vjust=2, family="Times New Roman"),axis.title.x=element_text(vjust=-1, size=12, family="Times New Roman"),axis.title.y=element_text(vjust=-1, size=12, family="Times New Roman"),axis.text.x=element_text(size=10,family="Times New Roman"),axis.text.y=element_text(size=10,family="Times New Roman"),panel.background = element_rect(fill = 'white', colour = 'gray'),
panel.grid.major=element_line(colour="gray98"),
legend.key = element_rect(fill = "white"))+annotate("text",x=25,y=0.6,hjust = 2.2, vjust = 0.4,label="(a)",family="Times New Roman")

#P1


P2=ggplot(fit.eff_x1,aes(x1))+
geom_line(aes(y=fit.eff_x1$fit),colour="black")+
geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.2)+labs(x="rainfall (mm)",y="proportion infected")+theme(plot.title=element_text(face="bold", size=16, vjust=2, family="Times New Roman"),axis.title.x=element_text(vjust=-1, size=12, family="Times New Roman"),axis.title.y=element_text(vjust=-1, size=12, family="Times New Roman"),axis.text.x=element_text(size=10,family="Times New Roman"),axis.text.y=element_text(size=10,family="Times New Roman"),panel.background = element_rect(fill = 'white', colour = 'gray'),
panel.grid.major=element_line(colour="gray98"),
legend.key = element_rect(fill = "white"))+annotate("text",x=6,y=0.6,hjust = 2,vjust = 0.4,label="(b)",family="Times New Roman")

#P2

ggplot2.multiplot(P1,P2,cols=1)





