
#########################
#Input data
##########################

#Min_temperature
min_temp_90=read.csv("Data/evmn1990-1999.csv",header=TRUE)

min_temp_00=read.csv("Data/evmn2000-2009.csv",header=TRUE)

min_temp_10=read.csv("Data/evmn2010-current_30.csv",header=TRUE)

min_temp=rbind(min_temp_90,min_temp_00,min_temp_10)

#Max_temperature
max_temp_90=read.csv("Data/evmxOct1992-Dec1999.csv",header=TRUE)

max_temp_00=read.csv("Data/evmx2000-2009_0.csv",header=TRUE)

max_temp_10=read.csv("Data/evmx2010-current_33.csv",header=TRUE)

max_temp=rbind(max_temp_90,max_temp_00,max_temp_10)



#rainfall

rain_90=read.csv("Data/evra1990-1999_0.csv",header=TRUE)

rain_00=read.csv("Data/evra2000-2009.csv",header=TRUE)

rain_10=read.csv("Data/evra2010-current_34.csv",header=TRUE)
rain_10=rain_10[,1:4] #delete last column of notes

rain=rbind(rain_90,rain_00,rain_10)


################################
#subset rianfall data
################################


#julian dates
#may 1990 =121
#jul 1996 = 183
#jun 1997 = 152
#may 1998 = 121
# jun 2015 = 152
# may 2016 = 122

julian=c(1990.121,1996.183,1997.152,1998.121,2015.152,2016.122)

#TEMPERATURE (http://jme.oxfordjournals.org/content/39/1/221.abstract) mosquito competence increases with increasing temp.

#1 month (30 days before first census)

#max
max_temp90_mn_30=NA
max_temp90_var_30=NA

max_temp96_mn_30=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1996 & max_temp$JULIAN < 183 & max_temp$JULIAN >= 153],na.rm=TRUE)
max_temp96_var_30=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1996 & max_temp$JULIAN < 183 & max_temp$JULIAN >= 153],na.rm=TRUE)

max_temp97_mn_30=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1997 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 122],na.rm=TRUE)
max_temp97_var_30=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1997 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 122],na.rm=TRUE)

max_temp98_mn_30=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1998 & max_temp$JULIAN < 121 & max_temp$JULIAN >= 91],na.rm=TRUE)
max_temp98_var_30=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1998 & max_temp$JULIAN < 121 & max_temp$JULIAN >= 91],na.rm=TRUE)

max_temp15_mn_30=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==2015 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 122],na.rm=TRUE)
max_temp15_var_30=var(max_temp$MAXTEMP.C.[max_temp$YEAR==2015 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 122],na.rm=TRUE)

max_temp16_mn_30=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==2016 & max_temp$JULIAN < 122 & max_temp$JULIAN >= 92],na.rm=TRUE)
max_temp16_var_30=var(max_temp$MAXTEMP.C.[max_temp$YEAR==2016 & max_temp$JULIAN < 122 & max_temp$JULIAN >= 92],na.rm=TRUE)

mnmxt_30=c(max_temp90_mn_30,max_temp96_mn_30,max_temp97_mn_30,max_temp98_mn_30,max_temp15_mn_30,max_temp16_mn_30)

vrmxt_30=c(max_temp90_var_30,max_temp96_var_30,max_temp97_var_30,max_temp98_var_30,max_temp15_var_30,max_temp16_var_30)



#4 month (120 days before first census)

#max
max_temp90_mn_120=NA
max_temp90_var_120=NA

max_temp96_mn_120=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1996 & max_temp$JULIAN < 183 & max_temp$JULIAN >= 63],na.rm=TRUE)
max_temp96_var_120=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1996 & max_temp$JULIAN < 183 & max_temp$JULIAN >= 63],na.rm=TRUE)

max_temp97_mn_120=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1997 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 32],na.rm=TRUE)
max_temp97_var_120=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1997 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 32],na.rm=TRUE)

max_temp98_mn_120=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==1998 & max_temp$JULIAN < 121 & max_temp$JULIAN >= 1],na.rm=TRUE)
max_temp98_var_120=var(max_temp$MAXTEMP.C.[max_temp$YEAR==1998 & max_temp$JULIAN < 121 & max_temp$JULIAN >= 1],na.rm=TRUE)

max_temp15_mn_120=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==2015 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 32],na.rm=TRUE)
max_temp15_var_120=var(max_temp$MAXTEMP.C.[max_temp$YEAR==2015 & max_temp$JULIAN < 152 & max_temp$JULIAN >= 32],na.rm=TRUE)

max_temp16_mn_120=mean(max_temp$MAXTEMP.C.[max_temp$YEAR==2016 & max_temp$JULIAN < 122 & max_temp$JULIAN >= 2],na.rm=TRUE)
max_temp16_var_120=var(max_temp$MAXTEMP.C.[max_temp$YEAR==2016 & max_temp$JULIAN < 122 & max_temp$JULIAN >= 2],na.rm=TRUE)

mnmxt_120=c(max_temp90_mn_120,max_temp96_mn_120,max_temp97_mn_120,max_temp98_mn_120,max_temp15_mn_120,max_temp16_mn_120)

vrmxt_120=c(max_temp90_var_120,max_temp96_var_120,max_temp97_var_120,max_temp98_var_120,max_temp15_var_120,max_temp16_var_120)




#min
min_temp90_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1990 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 91],na.rm=TRUE)
min_temp90_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==1990 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 91],na.rm=TRUE)

min_temp96_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1996 & min_temp$JULIAN < 183 & min_temp$JULIAN >= 153],na.rm=TRUE)
min_temp96_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==1996 & min_temp$JULIAN < 183 & min_temp$JULIAN >= 153],na.rm=TRUE)

min_temp97_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1997 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 122],na.rm=TRUE)
min_temp97_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==1997 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 122],na.rm=TRUE)

min_temp98_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1998 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 91],na.rm=TRUE)
min_temp98_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==1998 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 91],na.rm=TRUE)

min_temp15_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==2015 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 122],na.rm=TRUE)
min_temp15_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==2015 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 122],na.rm=TRUE)

min_temp16_mn_30=mean(min_temp$MINTEMP.C.[min_temp$YEAR==2016 & min_temp$JULIAN < 122 & min_temp$JULIAN >= 92],na.rm=TRUE)
min_temp16_var_30=var(min_temp$MINTEMP.C.[min_temp$YEAR==2016 & min_temp$JULIAN < 122 & min_temp$JULIAN >= 92],na.rm=TRUE)

mnmit_30=c(min_temp90_mn_30,min_temp96_mn_30,min_temp97_mn_30,min_temp98_mn_30,min_temp15_mn_30,min_temp16_mn_30)

vrmit_30=c(min_temp90_var_30,min_temp96_var_30,min_temp97_var_30,min_temp98_var_30,min_temp15_var_30,min_temp16_var_30)





#4 month (120 days before first census)
min_temp90_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1990 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 1],na.rm=TRUE)
min_temp90_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==1990 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 1],na.rm=TRUE)

min_temp96_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1996 & min_temp$JULIAN < 183 & min_temp$JULIAN >= 63],na.rm=TRUE)
min_temp96_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==1996 & min_temp$JULIAN < 183 & min_temp$JULIAN >= 63],na.rm=TRUE)

min_temp97_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1997 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 32],na.rm=TRUE)
min_temp97_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==1997 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 32],na.rm=TRUE)

min_temp98_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==1998 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 1],na.rm=TRUE)
min_temp98_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==1998 & min_temp$JULIAN < 121 & min_temp$JULIAN >= 1],na.rm=TRUE)

min_temp15_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==2015 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 32],na.rm=TRUE)
min_temp15_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==2015 & min_temp$JULIAN < 152 & min_temp$JULIAN >= 32],na.rm=TRUE)

min_temp16_mn_120=mean(min_temp$MINTEMP.C.[min_temp$YEAR==2016 & min_temp$JULIAN < 122 & min_temp$JULIAN >= 2],na.rm=TRUE)
min_temp16_var_120=var(min_temp$MINTEMP.C.[min_temp$YEAR==2016 & min_temp$JULIAN < 122 & min_temp$JULIAN >= 2],na.rm=TRUE)

mnmit_120=c(min_temp90_mn_120,min_temp96_mn_120,min_temp97_mn_120,min_temp98_mn_120,min_temp15_mn_120,min_temp16_mn_120)

vrmit_120=c(min_temp90_var_120,min_temp96_var_120,min_temp97_var_120,min_temp98_var_120,min_temp15_var_120,min_temp16_var_120)



#RAINFALL###################################################
#1 month (30 days before first census)
rain90_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==1990 & rain$JULIAN < 121 & rain$JULIAN >= 91])
rain90_var_30=var(rain$RAINFALL..MM.[rain$YEAR==1990 & rain$JULIAN < 121 & rain$JULIAN >= 91])

rain96_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==1996 & rain$JULIAN < 183 & rain$JULIAN >= 153])
rain96_var_30=var(rain$RAINFALL..MM.[rain$YEAR==1996 & rain$JULIAN < 183 & rain$JULIAN >= 153])

rain97_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==1997 & rain$JULIAN < 152 & rain$JULIAN >= 122])
rain97_var_30=var(rain$RAINFALL..MM.[rain$YEAR==1997 & rain$JULIAN < 152 & rain$JULIAN >= 122])

rain98_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==1998 & rain$JULIAN < 121 & rain$JULIAN >= 91])
rain98_var_30=var(rain$RAINFALL..MM.[rain$YEAR==1998 & rain$JULIAN < 121 & rain$JULIAN >= 91])

rain2015_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==2015 & rain$JULIAN < 152 & rain$JULIAN >= 122])
rain2015_var_30=var(rain$RAINFALL..MM.[rain$YEAR==2015 & rain$JULIAN < 152 & rain$JULIAN >= 122])

rain2016_mn_30=mean(rain$RAINFALL..MM.[rain$YEAR==2016 & rain$JULIAN < 122 & rain$JULIAN >= 92])
rain2016_var_30=var(rain$RAINFALL..MM.[rain$YEAR==2016 & rain$JULIAN < 122 & rain$JULIAN >= 92])

srm_30=c(rain90_mn_30,rain96_mn_30,rain97_mn_30,rain98_mn_30,rain2015_mn_30,rain2016_mn_30)

srv_30=c(rain90_var_30,rain96_var_30,rain97_var_30,rain98_var_30,rain2015_var_30,rain2016_var_30)





#4 month (120 days before first census)
rain90_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==1990 & rain$JULIAN < 121 & rain$JULIAN >= 1])
rain90_var_120=var(rain$RAINFALL..MM.[rain$YEAR==1990 & rain$JULIAN < 121 & rain$JULIAN >= 1])

rain96_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==1996 & rain$JULIAN < 183 & rain$JULIAN >= 63])
rain96_var_120=var(rain$RAINFALL..MM.[rain$YEAR==1996 & rain$JULIAN < 183 & rain$JULIAN >= 63])

rain97_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==1997 & rain$JULIAN < 152 & rain$JULIAN >= 32])
rain97_var_120=var(rain$RAINFALL..MM.[rain$YEAR==1997 & rain$JULIAN < 152 & rain$JULIAN >= 32])

rain98_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==1998 & rain$JULIAN < 121 & rain$JULIAN >= 1])
rain98_var_120=var(rain$RAINFALL..MM.[rain$YEAR==1998 & rain$JULIAN < 121 & rain$JULIAN >= 1])

rain2015_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==2015 & rain$JULIAN < 152 & rain$JULIAN >= 32])
rain2015_var_120=var(rain$RAINFALL..MM.[rain$YEAR==2015 & rain$JULIAN < 152 & rain$JULIAN >= 32])

rain2016_mn_120=mean(rain$RAINFALL..MM.[rain$YEAR==2016 & rain$JULIAN < 122 & rain$JULIAN >= 2])
rain2016_var_120=var(rain$RAINFALL..MM.[rain$YEAR==2016 & rain$JULIAN < 122 & rain$JULIAN >= 2])

srm_120=c(rain90_mn_120,rain96_mn_120,rain97_mn_120,rain98_mn_120,rain2015_mn_120,rain2016_mn_120)

srv_120=c(rain90_var_120,rain96_var_120,rain97_var_120,rain98_var_120,rain2015_var_120,rain2016_var_120)


enviro_summer=data.frame(julian,mnmxt_30,vrmxt_30,mnmxt_120,vrmxt_120,mnmit_30,vrmit_30,mnmit_120,vrmit_120,srm_30,srv_30,srm_120,srv_120)
colnames(enviro_summer)=c("julian","mean_maxT30","var_maxT30","mean_maxT120","var_maxT120","mean_minT30","var_minT30","mean_minT120","var_minT120","mean_rain30","var_rain30","mean_rain120","var_rain120")
