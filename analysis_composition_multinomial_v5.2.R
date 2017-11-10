library(AICcmodavg)

library(extrafont)
font_import()
loadfonts()


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

library(devtools)
#install_github("easyGgplot2", "kassambara")

library(easyGgplot2)

#Input disease data
#anolis-plasmodium

data=read.csv("Data/Data_1991_2016.csv")
ds=data[data$season=="summer",]
dw=data[data$season=="winter",]

ds.inf=ds
dw.inf=dw

ds.inf$yearn[ds.inf$year=="1990"]<-1990 #denote summer
ds.inf$yearn[ds.inf$year=="1996"]<-1996 #denote summer
ds.inf$yearn[ds.inf$year=="1997"]<-1997 #denote summer
ds.inf$yearn[ds.inf$year=="1998"]<-1998.5 #denote summer
ds.inf$yearn[ds.inf$year=="2015"]<-2015 #denote summer
ds.inf$yearn[ds.inf$year=="2016"]<-2016.5 #denote summer

dw.inf$yearn[dw.inf$year=="1991"]<-1991
dw.inf$yearn[dw.inf$year=="1997w"]<-1997
dw.inf$yearn[dw.inf$year=="1998w"]<-1998
dw.inf$yearn[dw.inf$year=="1999"]<-1999
dw.inf$yearn[dw.inf$year=="2001"]<-2001
dw.inf$yearn[dw.inf$year=="2002"]<-2002
dw.inf$yearn[dw.inf$year=="2016w"]<-2016


###################################
#Data manipulation
###################################

ds.inf=subset(ds.inf, ds.inf$yearn>1997)#species id happened after 1997
dw.inf=subset(dw.inf, dw.inf$yearn>1997)#species id happened after 1997

#Format data for ordinal model
ds.inf$plasmodium=ifelse(ds.inf$fl==1,"fl",ifelse(ds.inf$azw==1,"azw",ifelse(ds.inf$azr==1,"azr",NA))) 
ds.inf$plasmodium[ds.inf$infection==0]="NI"

dw.inf$plasmodium=ifelse(dw.inf$fl==1,"fl",ifelse(dw.inf$azw==1,"azw",ifelse(dw.inf$azr==1,"azr",NA))) 
dw.inf$plasmodium[dw.inf$infection==0]="NI"

# #reduce data frame to essential columns
# ds.inf=ds.inf[,-c(1:5,7:8,11:18)]
# dw.inf=dw.inf[,-c(1:5,7:8,11:17)]


#concatenate summer and winter into single data frame and sort for year
d.inf=rbind(ds.inf,dw.inf)
d.inf=d.inf[order(d.inf$yearn),]
d.inf$year=as.factor(d.inf$yearn)

#removes 120 rows with NAs in plasmodium species
d.inf=d.inf[complete.cases(d.inf$plasmodium),] 
d.inf$plasmodium=as.factor(d.inf$plasmodium)
d.inf$plasmodium1=relevel(d.inf$plasmodium,ref="NI")

#relevel sex
d.inf$sex=as.factor(d.inf$sex)
d.inf$sex=relevel(d.inf$sex,ref="1")#males


x=d.inf$year
x2=d.inf$svl
x3=d.inf$sex
cand.models=list()

cand.models[[1]]=multinom(d.inf$plasmodium1~x)
cand.models[[2]]=multinom(d.inf$plasmodium1~1)
cand.models[[3]]=multinom(d.inf$plasmodium1~x+x2)
cand.models[[4]]=multinom(d.inf$plasmodium1~x*x2)
cand.models[[5]]=multinom(d.inf$plasmodium1~x2)
cand.models[[6]]=multinom(d.inf$plasmodium1~x3)
cand.models[[7]]=multinom(d.inf$plasmodium1~x3+x2)
cand.models[[8]]=multinom(d.inf$plasmodium1~x3+x2+x)#best model
cand.models[[9]]=multinom(d.inf$plasmodium1~x2+x*x3)
cand.models[[10]]=multinom(d.inf$plasmodium1~x2*x+x3)


modnames=c("Year","Null","svl+year","svl*year","svl","sex","sex+svl","year+svl+sex","year*sex+svl","year*slv+sex")
aictab(cand.set=cand.models,modnames=modnames,sort=TRUE)

mod1=multinom(d.inf$plasmodium1~x+x2+x3)


fit.eff_x <- Effect("x", mod1) %>% as.data.frame()
fit.eff_x2 <- Effect("x2", mod1,xlevels=1000) %>% as.data.frame()
fit.eff_x3 <- Effect("x3", mod1) %>% as.data.frame()

####################################
#Figure year
####################################

year=rep(unique(d.inf$year))
species=c(rep("Non Infected",8),rep("P. azurophilum",8),rep("P. leucocytica",8),rep("P. floridense",8))

probability=c(fit.eff_x$prob.NI,fit.eff_x$prob.azr,fit.eff_x$prob.azw,fit.eff_x$prob.fl)
se=c(fit.eff_x$se.prob.NI,fit.eff_x$se.prob.azr,fit.eff_x$se.prob.azw,fit.eff_x$se.prob.fl)

df=data.frame(year,species,probability,se)
df=df[-(1:8),] #removes non-infected from plot


P=ggplot(df, aes(x=as.factor(year), y=probability, group=species,colour=species))+
geom_line( size=1
) + geom_point()+
   scale_color_brewer(palette="Set1")+
geom_errorbar(aes(ymin=probability-se, ymax=probability+se), width=.2)+xlab("year")+theme(legend.position="top") + scale_x_discrete(labels=c("2016.5" = "2016s", "1998.5" = "1998s",
                              "2015" = "2015s","1998"="1998w","1999"="1999w","2001"="2001w","2002"="2002w","2016"="2016w"))
P+ theme(plot.title=element_text(face="bold", size=16, vjust=2, family="Times New Roman"),axis.title.x=element_text(vjust=-1, size=12, family="Times New Roman"),axis.title.y=element_text(vjust=-1, size=12, family="Times New Roman"),legend.text=element_text(face="italic",size=10, family="Times New Roman"),legend.title=element_text(size=10,family="Times New Roman"),axis.text.x=element_text(size=10,family="Times New Roman"),axis.text.y=element_text(size=10,family="Times New Roman"),
panel.background = element_rect(fill = 'white', colour = 'gray'),
panel.grid.major=element_line(colour="gray98"),
legend.key = element_rect(fill = "white")
)



###############################
#Figure svl
###############################


P2=ggplot(fit.eff_x2,aes(x2))+
geom_line(aes(y=prob.azr),colour="red")+
geom_ribbon(aes(ymin=L.prob.azr,ymax=U.prob.azr),alpha=0.2)+labs(x="SVL (mm)",y="Prob. of infection")+annotate("text", x = 48, y = 0.3, label = "P. azurophilum",hjust=1, size=4,fontface='italic'
)+scale_x_continuous(limits = c(35, 80))


#P2

P3=ggplot(fit.eff_x2,aes(x2))+
geom_line(aes(y=prob.fl),colour="blue")+
geom_ribbon(aes(ymin=L.prob.fl,ymax=U.prob.fl),alpha=0.2)+labs(x="SVL (mm)",y="Prob. of infection")+annotate("text", x = 46, y = 0.06, label = "P. floridense",hjust=1, size=4,fontface='italic'
)+scale_x_continuous(limits = c(35, 80))

#P3

P4=ggplot(fit.eff_x2,aes(x2))+
geom_line(aes(y=prob.azw),colour="green")+
geom_ribbon(aes(ymin=L.prob.azw,ymax=U.prob.azw),alpha=0.2)+labs(x="SVL (mm)",y="Prob. of infection")+annotate("text", x = 47, y = 0.3, label = "P. leucocytica",hjust=1, size=4,fontface='italic'
)+scale_x_continuous(limits = c(35, 80))

#P4

ggplot2.multiplot(P2,P3,P4,cols=1)

#####################################
#Figure Sex
#####################################
fit.eff_x3$Sex=as.factor(c("Males","Females"))

s.prob=c(fit.eff_x3$prob.azr,fit.eff_x3$prob.azw,fit.eff_x3$prob.fl)
s.se=c(fit.eff_x3$se.prob.azr,fit.eff_x3$se.prob.azw,fit.eff_x3$se.prob.fl)
s.sex=rep(c("Males","Females"),3)
s.plas=c("P. azurophilum","P. azurophilum","P. leucocytica","P. leucocytica","P. floridense","P. floridense")
Sex.df=data.frame(cbind(as.numeric(s.prob),as.numeric(s.se),s.sex,s.plas))


P5=ggplot(Sex.df,aes(x=s.plas,y=s.prob,fill=s.sex))+
geom_bar(position=position_dodge(), stat="identity")+
geom_errorbar(aes(ymin=s.prob-s.se, ymax=s.prob+s.se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9))+
                  xlab("Parasite species") +
                  ylab("Probability of infection")+theme(legend.position="top")+guides(fill=guide_legend(title=NULL))



P5

