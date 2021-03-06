setwd("C:/Assessment/2014/r")
load("Summary.Rdata")

	GBmwMay.dat<-read.csv("data/GBMaymw.csv")
	GBmwAug.dat<-read.csv("data/GBa/GBmwAugData.csv")
	survGBMay.dat<-read.csv("data/GBMaySurvey8413.csv")

	# Strata areas
	GBa.strata.areas<-read.csv("data/GBa/GBaStrataAreas.csv")
	GBb.strata.areas<-read.csv("data/GBb/GBbStrataAreas.csv")


	fish.dat<-read.csv("data/RawFisheryData1981-2013.csv")
		source("fn/fishery.dat.r")
		fishery.obj<-fishery.dat(fish.dat,bk='GBa',yr=1986:2013,Gin=T,method='jackknife',model.out=T,export=F)				# fishery data 	

	
	# Survey Data
	survGBa.dat<-read.csv("data/GBa/GBaSurvey8113.csv")
	survGBaClap.dat<-subset(survGBa.dat,state=='dead')
	survGBaLive.dat<-subset(survGBa.dat,state=='live')
	survGBaRandom.dat<-subset(survGBa.dat,random==1&state=='live') # live only
	
	CS = c(rep(80,5),rep(90,10),rep(100,18))# CS = Shell height for knife-edge recriutment   
	RS = c(rep(65,5),rep(80,10),rep(90,18))# RS = Shell height 1 year previous to CS (new LVB parameters)
	years=1981:2013

	load('surveyObj.Rdata')

	DDGBa.dat<-read.csv("GBaModelData.csv")
		
		yrs<-1986:2013
		NY<- length(yrs)
		
		DDGBa.lst<-as.list(subset(DDGBa.dat,year%in%yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers","clappersR")))
		DDGBa.lst$NY<-length(DDGBa.lst$C)
		#DDGBa.lst$C.p<-seq(2000,6000,500)+965
		#DDGBa.lst$NC<-length(DDGBa.lst$C.p)
		
		
		
		# Set up Priors
		uI=log(DDGBa.lst$I.cv^2+1)
		Ip.a=2+(uI/uI)^2
		Ip.b=uI*((uI/uI)^2+1)
		
		uIR=log(DDGBa.lst$IR.cv^2+1)
		IRp.a=2+(uIR/uIR)^2
		IRp.b=uIR*((uIR/uIR)^2+1)
		
		uU=log(DDGBa.lst$U.cv^2+1)
		Up.a=2+(uU/uU)^2
		Up.b=uU*((uU/uU)^2+1)
		
		DDGBapriors=list(
				logK=			list(a=7,		b=7,		d="dnorm",		i1=8,	i2=10,	l=1		),		# scaler to total biomass
				r=				list(a=0, 		b=1,		d="dlnorm",		i1=0.3,	i2=0.9,	l=NY	),		# recruit index
				q=				list(a=20, 		b=40, 		d="dbeta",		i1=0.2,	i2=0.5,	l=1		),		# catchability for survey fully recruited
				qU=				list(a=0, 		b=1,		d="dunif",		i1=0.4,	i2=0.7,	l=1		),		# catchability for commercial CPUE
				m=				list(a=-2,		b=2,		d="dlnorm",		i1=0.1,	i2=0.3,	l=NY	),		# natural mortality fully recruited
				mR=				list(a=-2,		b=2,		d="dlnorm",		i1=0.2,	i2=0.4,	l=NY	),		# natural mortality  recruits
				S=				list(a=8, 		b=11, 		d="dbeta",		i1=0.5,	i2=0.8,	l=1		),		# clapper dissolution rate
				sigma=			list(a=0, 		b=5,		d="dunif",		i1=2,	i2=3,	l=1		),		# process error (SD)
				sigma.tau=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited from survey (precision)
				sigma.rho=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
				sigma.upsilon=	list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
				ikappa.tau2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited clappers (precision)
				ikappa.rho2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruit clappers (precision)
				I.precision=	list(a=Ip.a,	b=Ip.b,		d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for fully recruited from survey (precision)
				IR.precision=	list(a=IRp.a,	b=IRp.b,	d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for recruits from survey (precision)
				U.precision=	list(a=Up.a,	b=Up.b,		d="dgamma",		i1=15,	i2=30,	l=NY	)		# measurement error for recruits from survey (precision)
			)
				
		
		source("delayBUGS.r")
		DDGBa.out<-delayBUGS("DDwSE3", DDGBa.lst, DDGBapriors, 1986:2013, n = 300000, burn =200000, thin = 10,debug=F,add.parameters=c('Imed','Ipred','Irep','IRmed','IRpred','IRrep','sIresid','sIRresid','sPresid','Iresid','IRresid','Presid'))
		
		plotsGo<-"C:/Assessment/2014/r/Update/GBa/"
		write.csv(DDGBa.out$summary,paste0(plotsGo,"PostSumGBa.csv"))
	
	 	
		source("fn/post.plt.R")
		post.plt(DDGBa.out,DDGBapriors,years=1986:2013, graphic='pdf',nr=3,nc=4,wd=15,multi=F,path=plotsGo)
	
		source("fn/exploit.plt.r")	
		exploit.plt(DDGBa.out, years=1986:2013, plt=c('f','m','mR'),graphic='pdf',path=plotsGo)
	
		source("fn/fit.plt.R")
		fit.plt(DDGBa.out, CI=T,graphic='pdf',path=plotsGo,CV=T)
			
		source("fn/diag.plt.R")
		diag.plt(DDGBa.out, 1986:2013,graphic='pdf',path=plotsGo)
		
	 	source("fn/peR.r")
	 	peR("DDwSE3", DDGBa.lst, DDGBapriors, 1986:2013, pe=2013:2000,n = 60000, burn = 30000, thin = 10, plot=0,g2=survey.obj[[1]]$g2[6:33],gR2=survey.obj[[1]]$gR2[6:33],lab='GBa')
	 	peR("DDwSE3", DDGBa.lst, DDGBapriors, 1986:2013, pe=2013:2001,run=F, plot=3,proj.from="BUGS",graphic='R',lab='GBa',path=plotsGo)
	 	
	 	peR("DDwSE3", DDGBa.lst, DDGBapriors, 1986:2013, pe=2013:2000,n = 60000, burn = 30000, thin = 10, plot=0,lab='GBag1')
	 	peR("DDwSE3", DDGBa.lst, DDGBapriors, 1986:2013, pe=2013:2001,run=F, plot=3,proj.from="BUGS",graphic='R',lab='GBag1',path=plotsGo)
			

			
	source("fn/projections.r")
	DDGBa.out<-projections(DDGBa.out,C.p=seq(2000,8000,500)+530)
	source("fn/biomass.plt.r")
	biomass.plt(DDGBa.out,years=1986:2013, graphic='pdf',TAC=4000+530,path=plotsGo,index=1:24,avg.line=median)	
	 	
		
	source("fn/decision.r")
	DtabGBa<-decision("GBa",DDGBa.out, mu=0.15,refs=c(12789,4796),post.survey.C=530)
 	write.csv(DtabGBa,paste0(plotsGo,"Decision1_GBa.csv"),row.names=F)

 	
 	
 ########################### BRowns North ###############
 
 		BBnmw.dat<-read.csv("data/BBn/BBnmwData.csv")
	
 	
		# Survey Data
		survBBn.dat<-read.csv("data/BBn/BBnSurvey9113.csv")
		survBBnClap.dat<-subset(survBBn.dat,state=='dead')
		survBBnLive.dat<-subset(survBBn.dat,state=='live')
		BBn.strata.areas<-read.csv("data/BBn/BBnStrataAreasHCR.csv")
		
		
		# Shell height for knife edge recruitment based on portsampling data
		CS = 100 # CS = Shell height for knife-edge recriutment   
		RS = 90 # RS = Shell height 1 year previous to CS
		# years
		years=1991:2013

		source('surveyBBnObj.R')
				
		# prepare survey index data obj
		#source("fn/survey.dat.r")
		#survey.obj<-survey.dat(survBBnRandom.dat, years=1991:2013, RS=RS, CS=CS, bk="BBn", areas=BBn.strata.areas$area, mw.par="CFh")		# survey data for model input 
		#clap.obj<-survey.dat(survBBnClap.dat, years=1991:2013, RS=RS, CS=CS, bk="BBn", areas=BBn.strata.areas$area, mw.par="CFh")		# survey data for model input 
		survey.obj[[1]]$CF<-sapply(1:length(years),function(x){with(subset(survBBnLive.dat,year==years[x]&random),weighted.mean(CFh,com.bm,na.rm=T))})
		survey.obj[[1]]$clappers<-clap.obj[[1]]$N
		survey.obj[[1]]$clappersR<-clap.obj[[1]]$NR
		
		BBnlvb.par<-c(148, 0.19, 0.11)
		names(BBnlvb.par)<-c("linf","k","t0")
				
		# growth by height adjusted for condition
		waa.tm1 <- survey.obj[[1]]$CF*(survey.obj[[1]]$l.bar/100)^3
		laa.t <- BBnlvb.par[1]*(1-exp(-BBnlvb.par[2]))+exp(-BBnlvb.par[2])*survey.obj[[1]]$l.bar
		waa.t <- c(survey.obj[[1]]$CF[-1],survey.obj[[1]]$CF[nrow(survey.obj[[1]])])*(laa.t/100)^3
		waa.t2 <- survey.obj[[1]]$CF*(laa.t/100)^3
		survey.obj[[1]]$g <- waa.t/waa.tm1
		survey.obj[[1]]$g2 <- waa.t2/waa.tm1
		
		# for recruits
		waa.tm1 <- survey.obj[[1]]$CF*(survey.obj[[1]]$l.k/100)^3
		laa.t <- BBnlvb.par[1]*(1-exp(-BBnlvb.par[2]))+exp(-BBnlvb.par[2])*survey.obj[[1]]$l.k
		waa.t <- c(survey.obj[[1]]$CF[-1],survey.obj[[1]]$CF[nrow(survey.obj[[1]])])*(laa.t/100)^3
		waa.t2 <- survey.obj[[1]]$CF*(laa.t/100)^3
		survey.obj[[1]]$gR <- waa.t/waa.tm1
		survey.obj[[1]]$gR2 <- waa.t2/waa.tm1
		

		
		fish.dat<-read.csv("data/RawFisheryData1981-2013.csv")
		
		source("fn/fishery.dat.r")
		BBnfishery.obj<-fishery.dat(fish.dat,bk='BBn',yr=1991:2013,method='jackknife',model.out=T,export=F,period='survyr',surv='May')				# fishery data 	
		
		DDBBn.dat<-merge(survey.obj[[1]],BBnfishery.obj)
		write.csv(DDBBn.dat,"BBnModelData.csv",row.names=F)
		DDBBn.dat<-read.csv("BBnModelData.csv")
		
		yrs<-1991:2013
		NY<- length(yrs)
		
		DDBBn.lst<-as.list(subset(DDBBn.dat,year%in%yrs,c("I","I.cv","IR","IR.cv","g","gR","C","U","U.cv","N","NR","clappers","clappersR")))
		DDBBn.lst$NY<-length(DDBBn.lst$C)
		#DDBBn.lst$C.p<-seq(2000,6000,500)+965
		#DDBBn.lst$NC<-length(DDBBn.lst$C.p)
		
		
		
		# Set up Priors
		uI=log(DDBBn.lst$I.cv^2+1)
		Ip.a=2+(uI/uI)^2
		Ip.b=uI*((uI/uI)^2+1)
		
		uIR=log(DDBBn.lst$IR.cv^2+1)
		IRp.a=2+(uIR/uIR)^2
		IRp.b=uIR*((uIR/uIR)^2+1)
		
		uU=log(DDBBn.lst$U.cv^2+1)
		Up.a=2+(uU/uU)^2
		Up.b=uU*((uU/uU)^2+1)
		
		DDBBnpriors=list(
				logK=			list(a=7,		b=7,		d="dnorm",		i1=8,	i2=10,	l=1		),		# scaler to total biomass
				r=				list(a=0, 		b=1,		d="dlnorm",		i1=0.3,	i2=0.9,	l=NY	),		# recruit index
				q=				list(a=20, 		b=40, 		d="dbeta",		i1=0.2,	i2=0.5,	l=1		),		# catchability for survey fully recruited
				qU=				list(a=0, 		b=1,		d="dunif",		i1=0.4,	i2=0.7,	l=1		),		# catchability for commercial CPUE
				m=				list(a=-2,		b=2,		d="dlnorm",		i1=0.1,	i2=0.3,	l=NY	),		# natural mortality fully recruited
				mR=				list(a=-2,		b=2,		d="dlnorm",		i1=0.2,	i2=0.4,	l=NY	),		# natural mortality  recruits
				S=				list(a=8, 		b=11, 		d="dbeta",		i1=0.5,	i2=0.8,	l=1		),		# clapper dissolution rate
				sigma=			list(a=0, 		b=5,		d="dunif",		i1=2,	i2=3,	l=1		),		# process error (SD)
				sigma.tau=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited from survey (precision)
				sigma.rho=		list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
				sigma.upsilon=	list(a=2, 		b=1,		d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruits from survey (precision)
				ikappa.tau2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for fully recruited clappers (precision)
				ikappa.rho2=	list(a=3, 		b=0.44629,	d="dgamma",		i1=15,	i2=30,	l=1		),		# measurement error for recruit clappers (precision)
				I.precision=	list(a=Ip.a,	b=Ip.b,		d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for fully recruited from survey (precision)
				IR.precision=	list(a=IRp.a,	b=IRp.b,	d="dgamma",		i1=15,	i2=30,	l=NY	),		# measurement error for recruits from survey (precision)
				U.precision=	list(a=Up.a,	b=Up.b,		d="dgamma",		i1=15,	i2=30,	l=NY	)		# measurement error for recruits from survey (precision)
			)
				
		
		source("delayBUGS.r")
		DDBBn.out<-delayBUGS("DDwSE3", DDBBn.lst, DDBBnpriors, 1991:2013, n = 300000, burn =200000, thin = 10,debug=F,add.parameters=c('Imed','Ipred','Irep','IRmed','IRpred','IRrep','sIresid','sIRresid','sPresid','Iresid','IRresid','Presid'))
		
		plotsGo<-"C:/Assessment/2014/Update/BBn/"
		write.csv(DDBBn.out$summary,paste0(plotsGo,"PostSumBBn.csv"))
	
	 	
		source("fn/post.plt.R")
		post.plt(DDBBn.out,DDBBnpriors,years=1991:2013, graphic='pdf',nr=3,nc=4,wd=15,multi=F,path=plotsGo)
	
		source("fn/exploit.plt.r")	
		exploit.plt(DDBBn.out, years=1991:2013, plt=c('f','m','mR'),graphic='pdf',path=plotsGo)
	
		source("fn/fit.plt.R")
		fit.plt(DDBBn.out, CI=T,graphic='pdf',path=plotsGo,CV=T)
			
		#source("fn/diag.plt.R")
		#diag.plt(DDBBn.out, 1991:2013,graphic='pdf',path=plotsGo)
		
	 	source("fn/peR.r")
	 	peR("DDwSE3", DDBBn.lst, DDBBnpriors, 1991:2013, pe=2013:2000,n = 60000, burn = 30000, thin = 10, plot=0,g2=survey.obj[[1]]$g2,gR2=survey.obj[[1]]$gR2,lab='BBn')
	 	peR("DDwSE3", DDBBn.lst, DDBBnpriors, 1991:2013, pe=2013:2001,run=F, plot=3,proj.from="BUGS",graphic='R',lab='BBn',path=plotsGo)
	 	
	 	peR("DDwSE3", DDBBn.lst, DDBBnpriors, 1991:2013, pe=2013:2000,n = 60000, burn = 30000, thin = 10, plot=0,lab='BBng1')
	 	peR("DDwSE3", DDBBn.lst, DDBBnpriors, 1991:2013, pe=2013:2001,run=F, plot=3,proj.from="BUGS",graphic='R',lab='BBng1',path=plotsGo)
			

			
	source("fn/projections.r")
	DDBBn.out<-projections(DDBBn.out,C.p=seq(0,2000,50)+83)
	source("fn/biomass.plt.r")
	biomass.plt(DDBBn.out,years=1991:2013, graphic='R',TAC=750+83,path=plotsGo)	
	biomass.plt(DDBBn.out,years=1991:2013, TAC=750+83, graphic='pdf',refs=NULL,lab='BBn',avg.line=median,path=plotsGo)	
	 	
		
	source("fn/decision.r")
	DtabBBn<-decision("BBn",DDBBn.out, mu=0.15,post.survey.C=83)
 	write.csv(DtabBBn,paste0(plotsGo,"Decision1_BBn.csv"),row.names=F)

 	
 	
 	
