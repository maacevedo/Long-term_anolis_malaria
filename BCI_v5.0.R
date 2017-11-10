data=read.csv("Data/Data_1991_2016.csv")

ds=data[data$season=="summer",]
dw=data[data$season=="winter",]

# ####################
# #Data manipulation
# ####################

 ds.df=as.data.frame(ds[,c(6:10)])
 dw.df=as.data.frame(dw[,c(6:10)])

 ds.df=ds.df[complete.cases(ds.df),]
 dw.df=dw.df[complete.cases(dw.df),]

#summer
matrixs.res=numeric()

for (j in 1:2){
for (i in unique(ds.df$year)){

temp=ds.df[ds.df$year==i & ds.df$sex==j,]
temp$residuals=residuals(lm(log10(temp$weight)~log10(temp$svl)))
matrixs.res=rbind(matrixs.res,temp)
}}

#winter
matrixw.res=numeric()

for (j in 1:2){
for (i in unique(dw.df$year)){

temp=dw.df[dw.df$year==i & dw.df$sex==j,]
temp$residuals=residuals(lm(log10(temp$weight)~log10(temp$svl)))
matrixw.res=rbind(matrixw.res,temp)
}}

##########################
#ANOVA
##########################
#normality test
par(mfrow=c(1,2))
hist(matrixs.res$residuals)
hist(matrixw.res$residuals)

matrixs.res$year=as.factor(matrixs.res$year)
matrixs.res$sex=factor(matrixs.res$sex,labels=c("males","females"))

matrixw.res$year=as.factor(matrixw.res$year)
matrixw.res$sex=factor(matrixw.res$sex,labels=c("males","females"))

#summer
av.summer.f=aov(matrixs.res$residuals[matrixs.res$sex=="females"]~matrixs.res$infection[matrixs.res$sex=="females"]*matrixs.res$year[matrixs.res$sex=="females"])
summary(av.summer.f)

av.summer.m=aov(matrixs.res$residuals[matrixs.res$sex=="males"]~matrixs.res$infection[matrixs.res$sex=="males"]*matrixs.res$year[matrixs.res$sex=="males"])
summary(av.summer.m)

#winter
av.winter.f=aov(matrixw.res$residuals[matrixw.res$sex=="females"]~matrixw.res$infection[matrixw.res$sex=="females"]*matrixw.res$year[matrixw.res$sex=="females"])
summary(av.winter.f)

av.winter.m=aov(matrixw.res$residuals[matrixw.res$sex=="males"]~matrixw.res$infection[matrixw.res$sex=="males"]*matrixw.res$year[matrixw.res$sex=="males"])
summary(av.winter.m)

########################
#Plot
########################

library(ggplot2)

matrixs.res$infection[matrixs.res$infection==1]<-"infected"
matrixs.res$infection[matrixs.res$infection==0]<-"non-infected"

matrixw.res$infection[matrixw.res$infection==1]<-"infected"
matrixw.res$infection[matrixw.res$infection==0]<-"non-infected"

p1 <- ggplot(matrixs.res, aes(x=year, y=residuals)) +
  geom_boxplot(aes(fill=infection), position=position_dodge(.9)) +
  facet_wrap(~sex) +
  stat_summary(fun.y=mean, aes(group=infection), position=position_dodge(.9),color="black", size=4)+xlab("")+ylab(expression(paste("body condition (",R[i],")")))+theme(legend.position="top")+theme(axis.title.x=element_text(vjust=-1, size=12, family="Times New Roman"),strip.text.x
=element_text(vjust=-1, size=12, family="Times New Roman"), axis.title.y=element_text(vjust=-1, size=12, family="Times New Roman"),axis.text.x=element_text(size=10,family="Times New Roman"),axis.text.y=element_text(size=10,family="Times New Roman"),panel.background = element_rect(fill = 'white', colour = 'gray'),strip.background=element_rect(fill="gray96"),
panel.grid.major=element_line(colour="gray98"),
legend.key = element_rect(fill = "white"),legend.text=element_text(size=10, family="Times New Roman"),legend.position="top",
        legend.justification="center",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-5,-5,-5,-5)
)+guides(fill=guide_legend(title=""))



ann_text.1 <- data.frame(year = 1.2,residuals = 0.7,lab = "Text",
                       sex = factor("males",levels = c("males","females")))
p1=p1 + geom_text(data = ann_text.1,label = "(a) summer")




p2 <- ggplot(matrixw.res, aes(x=year, y=residuals)) +
  geom_boxplot(aes(fill=infection), position=position_dodge(.9)) +
  facet_wrap(~sex) +
  stat_summary(fun.y=mean, aes(group=infection), position=position_dodge(.9),color="black", size=4)+xlab("year")+ylab(expression(paste("body condition (",R[i],")")))+theme(legend.position="")+theme(axis.title.x=element_text(vjust=-1, size=12, family="Times New Roman"),strip.text.x
=element_text(vjust=-1, size=12, family="Times New Roman"), axis.title.y=element_text(vjust=-1, size=12, family="Times New Roman"),axis.text.x=element_text(size=10,family="Times New Roman"),axis.text.y=element_text(size=10,family="Times New Roman"),panel.background = element_rect(fill = 'white', colour = 'gray'),strip.background=element_rect(fill="gray96"),
panel.grid.major=element_line(colour="gray98"),
legend.key = element_rect(fill = "white"),legend.text=element_text(size=10, family="Times New Roman"))+guides(fill=guide_legend(title=""))


ann_text.2 <- data.frame(year = 0.8,residuals = 0.7,lab = "Text",
                       sex = factor("males",levels = c("males","females")))
p2=p2 + geom_text(data = ann_text.2,label = "(b) winter")


require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

library(devtools)
#install_github("easyGgplot2", "kassambara")

library(easyGgplot2)

ggplot2.multiplot(p1,p2,cols=1)














