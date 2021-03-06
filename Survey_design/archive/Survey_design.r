# An attempt by DK to make the survey design scripts fit within the current structure, we are currently using 
# a variety of files for this which contain the same information as what we use for the survey/modelling and 
# if we leave it like this the duplication will make us look like chumps eventually...
#####  DK August 26th, 2016
# Update history
# Commented, checked  and revised by DK August 2016
# March 2017 : Added option for text points or not on GBa, and changed Ger to have 20 extra random stations so Ginette and Tech can "randomly" remove 20 of them...
              # also fixed "zoomed in" GBa figures so the Northeast and Northwest labels were correct...
# June 2017:  Added an option for adding in extra stations to the figures.  These would need to be in the file extra.stations.csv, see below.
#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  None, this is a top level file whose output is either csv files or figures.
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 1: source(paste(direct,"Assessment_fns/Survey_design/alloc.poly_new.r",sep=""))
# 2: source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
# 3: source(paste(direct,"Assessment_fns/Survey_design/genran.r",sep=""))
# 4: source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
###############################################################################################################

###############################################################################################################
# Arguments
# yr:            The year of interest for making survey design.  Default is the current year as.numeric(format(Sys.time(), "%Y"))
# direct:        The directory to load/save all the data, Default is the network offshore scallop "Y:/Offshore scallop/Assessment_fns/"
# export:        Do you want to export the survey design locations.  T/F Default is F
# seed:          If you want to reproduce results you can specify the random seed used for allocating these.  Default = NULL which will use R's 
#                internal random number generators.  
# plot:          Do you want to plot the results.  T/F Default is T
# fig:           Where do you want to plot the figures.  Three options, includes the default print to your "screen" optionally can do "pdf" or "png".
# legend:        Add a legend to the figure.  T/F Default  is T
# zoom:          Do you want to produce magnified maps for GBa tow locations.  T/F default is T
# banks:         What banks do you want to run this on.  Default banks are "BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger" (note no option for Ban yet)
# relief.plots:  For German bank do you want to make the "relief plots", note these take a long time to make!!.  T/F and default is F
# digits:        For the relief plots this controls the smoothing of the surface.  Basically this says how many digits to retain in the X and Y locations
#                Default is 4 (which is very detailed, using 3 makes a very smooth surface.)
# text.points:   Do you want to have the points in the zoomed in GBa plots text of the numbers or just cute little cexy circles?  T/F, default = F (which is circles)
# ger.new:       Number of new stations to generate on German bank, default is 60 (this must be <= 80, generally we only use 60 or 80), change the
#                alloc.poly number of stations below if you need > 80 new tows
# add.extras:    Do we want to add the extra stations to the figures, the coordinates of these extra stations 
#                would need to be in the file Data/Survey_data/Extra_stations.csv.  T/F with a default of F.
##### SURVEY DESIGN

Survey.design <- function(yr = as.numeric(format(Sys.time(), "%Y")) ,direct = "Y:/Offshore scallop/Assessment/",export = F,seed = NULL, text.points = F,
                          plot=T,fig="screen",legend=T, zoom = T,banks = c("BBs","BBn","GBa","GBb","Sab","Mid","GB","Ger"),
                          add.extras = F,relief.plots = F,digits=4,ger.new = 60)
{

# Make sure data imported doesn't become a factor
options(stringsAsFactors=F)
# load required packages
require(PBSmapping) || stop("Install PBSmapping Package bub")

# load in the functions we need to do the survey design
# Note I put the survey design functions in a "Survey_Design" folder with the other functions, and putting the figures in the "Survey_Design" folder 
source(paste(direct,"Assessment_fns/Survey_design/alloc.poly.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
source(paste(direct,"Assessment_fns/Survey_design/genran.r",sep=""))
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
#source(paste(direct,"Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r",sep=""))

# Bring in flat files we need for this to work, they are survey polyset, survey information, extra staions and the seedboxes.
surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
areas <- read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),stringsAsFactors = F) #Read1
surv.polydata <- read.csv(paste(direct,"Data/Survey_data/survey_information.csv",sep=""),stringsAsFactors = F)#Read2
seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                     stringsAsFactors = F,header=T) # Read3
# DK created this file by extracting the extra tows from the survey results in years past
#Read4 The idea would be to add new extra stations to this over time so they are all maintained in one location
if(add.extras == T) extra.tows <- read.csv(paste(direct,"Data/Survey_data/Extra_stations.csv",sep=""),stringsAsFactors = F)
# Note the folder structure is using the "Survey_Data" directory, there are two essentially static files stored in the top level directory
# the file with information about fixed station location are in the top level folder (fixed_station_banks_towlst.csv)
# the extra stations for all the banks are also stored in the same location (Extra_stations.csv), my hope is this is the file in which
# all of the new extra stations are stored so that we have the details regarding the extra station locations for all banks in all years
# in one location.

# Define some variables
surv.poly <- NULL
polydata <- NULL
towlst <- NULL

# Close all open plot devices
if(plot == T && !is.null(dev.list())) dev.off(dev.list())
# Get the index for the loop.
num.banks <- length(banks)
for(i in 1:num.banks)
{
  # Grab the bank
  bnk <- banks[i]
  # Grab any extra tows selected for that bank and make them a PBS mapping object
  if(add.extras ==F) extras <- data.frame(bill = NULL) # Did it this way to minimize the amount of code I need to change...
  if(add.extras == T) 
  {
    extras <- subset(extra.tows,year == yr,select = c("tow","lon","lat","bank"))
    names(extras) <- c("EID","X","Y","bank")
    extras$EID <- 1:nrow(extras)
    if(bnk != "Mid") key <-findPolys(extras, subset(areas,bank == bnk))
    if(bnk == "Mid") key <-findPolys(extras, subset(areas,bank == "Sab")) # This will put extras with Middle if any on Sable, but they won't show up on figure so ok.
    extras <- extras[extras$EID %in% key$EID,]
    attr(extras,"projection") <- "LL"
  } # end if(add.extras == T)
  # Grab any seedboxes that were closed during the current year.
  sb <- subset(seedboxes,Bank == bnk & Open >= paste(yr,"-01-01",sep=""))
  if(bnk == "GB") sb <- subset(seedboxes,Bank == "GBa" & Open >= paste(yr,"-01-01",sep=""))
  # Now for the banks that have survey strata we get the allocation.
  if(bnk %in% c("BBs","BBn","GBa","GBb","Sab"))
  {
    # Get the correct survey polygons
    surv.poly[[i]] <- subset(surv.polyset,label==bnk)
    attr(surv.poly[[i]],"projection")<-"LL"
    polydata[[i]] <- subset(surv.polydata,label==bnk)
    # Now allocate the tows, each bank has it's own settings for this allocation.
    #DK Note, I question the use of mindist of 3 on Sable, I'm sure there is good reason for it, but how much does this limit the 
    #actual randomization of the tow locations?
    if(bnk == "BBn") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=100,pool.size=3,mindist=1,seed=seed)
    if(bnk == "BBs") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=25,seed=seed)
    if(bnk == "Sab") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=100,pool.size=3,mindist=3,seed=seed)
    if(bnk == "GBb") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=30,pool.size=5,seed=seed)
    if(bnk == "GBa") towlst[[i]]<-alloc.poly(poly.lst=list(surv.poly[[i]], polydata[[i]]),ntows=200,pool.size=5,mindist=1,seed=seed)
      
	  
	  # if you want to save the tow lists you can export them to csv's.
  	if(export == T && bnk %in% c("BBn","BBs","GB","Mid","Sab")) 
  	{
  	  write.csv(towlst[[i]]$Tows,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Survey_design_Tow_locations.csv",sep=""),row.names=F) #Write1
  	} # end if(export == T && bnk %in% c("BBn","BBs","GB","Ger","Mid","Sab","Ban")) 
  	if(export == T && bnk %in% c("GBa","GBb")) 
  	{  
  	  write.csv(towlst[[i]]$Tows,paste(direct,"Data/Survey_Data/",yr,"/Summer/",bnk,"/Survey_design_Tow_locations.csv",sep=""),row.names=F) #Write2
  	} # end if(export == T && bnk %in% c("GBa,GBb")) 
	  
  	# Now if you want to make the plots do all of this.
  	if(plot == T)
  	{
  	  # Where do yo want the plot to go?
  	  if(fig=="screen") windows(11,8.5)
  	  if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".png",sep=""),width = 11, units="in", res=420,
  	                        height = 8.5,bg = "transparent")
  	  if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".pdf",sep=""),width = 11, 
  	                        height = 8.5,bg = "transparent")
  	  # Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
  	  ScallopMap(bnk,poly.lst=list(surv.poly[[i]],polydata[[i]]),plot.bathy = T,plot.boundries = T,dec.deg = F)
  	  title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
  	  addPoints(towlst[[i]]$Tows,pch=21, cex=1, bg = polydata[[i]]$col[towlst[[i]]$Tows$Poly.ID])
  	  if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
  	  if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	  if(legend == T && bnk != "GBa" && bnk!= "GBb") legend('bottomright',legend=polydata[[i]]$PName,pch=21,pt.bg=polydata[[i]]$col,bty='n',cex=0.9, inset = .01)
  	  if(legend == T && bnk %in% c("GBa","GBb")) legend('bottomleft',legend=polydata[[i]]$PName,pch=21,pt.bg=polydata[[i]]$col,bty='n',cex=0.9, inset = .01)
  	  if(!is.null(seed)) legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	  # Turn the device off if necessary.  
  	  if(fig != "screen") dev.off()
  	  
  	  # For Georges Bank in the summer we also create some maps that focus in on certain areas, if you set zoom = T this will happen.
  	  if(zoom == T && bnk == "GBa") 
  	  {
  	    # This looks closely at GBa south.
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"South.png",sep=""),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"South.pdf",sep=""),width = 11, 
  	                          height = 8.5,bg = "transparent")
  	    ScallopMap(ylim=c(41.25,41.833),xlim=c(-66.6,-65.85),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),dec.deg = F,
  	               poly.lst=list(surv.poly[[i]],polydata[[i]]),title=paste("GBa August Survey South (",yr,")",sep=""),cex=1.2)
  	    if(text.points == T) text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    if(text.points == F) points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21,cex=1)
  	    if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    if(!is.null(seed)) legend('bottomright',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	    
  	    # This looks closely at GBa Northwest
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"Northwest.png",sep=""),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"Northwest.pdf",sep=""),width = 11, 
  	                          height = 8.5,bg = "transparent")
  	    ScallopMap(ylim=c(41.833,42.2),xlim=c(-67.2,-66.6),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),dec.deg = F,
  	               poly.lst=list(surv.poly[[i]],polydata[[i]]),title=paste("GBa August Survey Northwest (",yr,")",sep=""),cex=1.2)
  	    if(text.points == T) text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    if(text.points == F) points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21,cex=1)  	
  	    if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    if(!is.null(seed)) legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	    
  	    # And this looks closely at GBa in the Northeast.
  	    if(fig=="screen") windows(11,8.5)
  	    if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"Northeast.png",sep=""),width = 11, units="in", 
  	                          res=420,height = 8.5,bg = "transparent")
  	    if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,"Northeast.pdf",sep=""),width = 11, 
  	                          height = 8.5,bg = "transparent")  	    
  	    ScallopMap(ylim=c(41.833,42.2),xlim=c(-66.6,-66),bathy.source="usgs",isobath='usgs',bathcol=rgb(0,0,1,0.3),dec.deg=F,
  	               poly.lst=list(surv.poly[[i]],polydata[[i]]),title=paste("GBa August Survey Northeast (",yr,")",sep=""),cex=1.2)
  	    if(text.points == T) text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
  	    if(text.points == F) points(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,pch=21,cex=1)
  	    if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
  	    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
  	    if(!is.null(seed)) legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
  	    
  	    # Turn the device off if necessary.  
  	    if(fig != "screen") dev.off()
  	  } # end if(zoom == T)
  	} # end if(plot == T)
  } # end if(bnk %in% c("BBs","BBn","GBa","GBb","Sab"))

######  Now do this for Middle and Georges Bank 
#####   Middle has fixed stations so we call in these 15 fixed stations from a flat file
#####   Georges Spring monitoring stations has 30 fixed stations 
  if(bnk %in% c("Mid","GB")) 
  {
    #Read5
    towlst[[i]] <-   subset(read.csv(paste(direct,"Data/Survey_data/fixed_station_banks_towlst.csv",sep="")),Bank == bnk)
    attr(towlst[[i]],"projection") <- "LL"
    if(plot == T)
    {
      if(fig=="screen") windows(11,8.5)
      if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".png",sep=""),width = 11, units="in", res=420,
                            height = 8.5,bg = "transparent")
      if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".pdf",sep=""),width = 11, 
                            height = 8.5,bg = "transparent")
    ScallopMap(bnk,plot.bathy = T,plot.boundries = T,dec.deg=F)
    title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
    addPoints(towlst[[i]],pch=21, cex=1)
    if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
    if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
    if(legend == T) legend('bottomleft',paste("Fixed stations (n = ",length(towlst[[i]]$EID),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
    if(fig != "screen") dev.off()
    }# end if(plot==T)
  } # end if(bnk %in% c("Mid","GB")) 
  
# Finally this will run German bank.
if(bnk == "Ger")
  {
    #Read6 Be careful here, I'm assuming that when you run this you are looking at the Ger data from "last year" and looking to identify the
    # tows that you want to repeat for this year, if trying to reproduce something specific for past years you'd need to change yr
    # to the year that you are interested in (if going back before 2012 you may need to create this csv file)
    survey.dat <- read.csv(paste(direct,"Data/Survey_data/",(yr-1),"/Spring/Ger/Survey1985-",(yr-1),".csv",sep=""))
    lastyearstows <- subset(survey.dat,state=='live'& year==(yr-1),c('tow','slon','slat','stratum'))
    lastyearstows$stratum<-1
    #Read7 This contains the boundary polygon from the file "GerSurvPoly1.csv".
    Ger.polyset <- subset(read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),stringsAsFactors = F),label==bnk)
    Ger.polyset$PID <- 1 # Force the PID to be 1, since it is a boundary there is only 1 unique PID...
    attr(Ger.polyset,"projection")<-"LL"
    # This gets us the tows for German bank, note we have 60 new tows and 20 repeats when we call it this way.
    Ger.tow.lst<-alloc.poly(poly.lst=list(Ger.polyset, data.frame(PID=1,PName="Ger",border=NA,col=rgb(0,0,0,0.2),repeats=20)),
                            ntows=80,pool.size=3,mindist=1,repeated.tows=lastyearstows,seed=seed)
    
    # Now subset the "new tows" down to what is specified in the above
    set.seed(seed)
    ger.tows <- sample(Ger.tow.lst$Tows$new.tows$EID,size=ger.new,replace=F)
    Ger.tow.lst$Tows$new.tows <- Ger.tow.lst$Tows$new.tows[Ger.tow.lst$Tows$new.tows$EID %in% ger.tows,]
    Ger.tow.lst$Tows$new.tows$EID <- 1:nrow(Ger.tow.lst$Tows$new.tows)
    # Rename and tidy up the data
    Ger.tow.lst$Tows$new.tows$STRATA="new"
    Ger.tow.lst$Tows$repeated.tows$STRATA="repeated"
    Ger.tow.lst$Tows$new.tows$Poly.ID=1
    Ger.tow.lst$Tows$repeated.tows$Poly.ID=4
    Ger.tow.dat<-do.call("rbind",Ger.tow.lst$Tows)
    
    #Write3 If you want to save the data here's where it will go
    if(export == T)  write.csv(Ger.tow.dat,paste(direct,"Data/Survey_Data/",yr,"/Spring/",bnk,"/Survey_design_Tow_locations.csv",sep=""),row.names=F)
    
    # Plot this bad boy up if you want to do such things
    if(plot==T)
    {
      if(fig=="screen") windows(11,8.5)
      if(fig =="png")   png(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".png",sep=""),width = 11, units="in", res=420,
                            height = 8.5,bg = "transparent")
      if(fig =="pdf")   pdf(paste(direct,yr,"/Survey_Design/",bnk,"/Survey_allocation-",bnk,".pdf",sep=""),width = 11, 
                            height = 8.5,bg = "transparent")  
      ScallopMap(bnk,plot.bathy = T,plot.boundries = T,dec.deg=F)
      # Add the German bank boundary and then add the survey points
      addPolys(Ger.polyset,border=NA,col=rgb(0,0,0,0.2))
      addPoints(Ger.tow.dat,pch=Ger.tow.dat$Poly.ID+20)
      title(paste("Survey (",bnk,"-",yr,")",sep=""),cex.main=2,line=1)
      # If there are extra tows or seedboxes plot them
      if(nrow(extras) > 0) addPoints(extras,pch=24, cex=1)
      if(nrow(sb) > 0) addPolys(sb,lty=2,lwd=2)
      # If the seed was set display this on the plot so you know later how you made that plot!!
      if(!is.null(seed)) legend('bottomleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
      legend('top',legend=c('new','repeated'),bty='n',pch=unique(Ger.tow.dat$Poly.ID+20), inset = .02)
      # Turn off the plot device if not plotting to screen
      if(fig != "screen") dev.off()
    } # end if(plot==T)
    
    # Now if you want to make these new fangled relief plots... source(paste(direct,"Assessment_fns/Survey_design/Relief.plots.r",sep=""))
    if(relief.plots == T)  Relief.plots(Ger.tow.dat,fig = fig,digits=digits)
  }# end if(bnk== "Ger")
} # end for(i in 1:num.banks)
} # end function