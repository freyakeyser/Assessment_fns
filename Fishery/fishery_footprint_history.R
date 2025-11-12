fishery_footprint_history <- function(bnk, years, months=NULL, high=25, highcpue = 30, subarea=NULL, survey, other, fish.dat, direct_fns) {
  
  source(paste(direct_fns,"Fishery/fishery.dat.r",sep=""))
  source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
  source(paste(direct_fns,"Survey_and_OSAC/gridPlot.r",sep=""))
  source(paste(direct_fns,"Maps/github_spatial_import.R",sep=""))
  
  require(sf)
  require(PBSmapping)
  #require(maptools)
  
  # Read2 Get the survey boundary polygons for all banks.
  # survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
  #                              header=T,stringsAsFactors = F)
  # newAreaPolys<-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep="")
  #                        ,stringsAsFactors = F,header=T)
  
  if(!is.null(subarea)) {
    if(subarea=="none") subarea <- NULL
  }
  survey$ID <- gsub(x=survey$ID, pattern=".shp", replacement="", fixed=T)
  survey <- survey[survey$ID==bnk,]
  if(!is.null(subarea)) other <- other[other$ID==subarea,]
  
  
  yr <- max(years)
  lvls <-  c(10,50,100,500,1000,5000,10000,50000)   # Set the levels, not important for the moment...
  lvls.eff <- c(10,50,100,1000)
  grids = sqrt(5)*1000 # 0.5km2
  if(is.null(months)) time <- data.frame(year=years)
  if(!is.null(months)) time <- arrange(expand.grid(year=years, month=months), year)
  
  fish.dat$month <- month(fish.dat$date)
  fish.dat_sf <- st_as_sf(fish.dat, coords=c("lon", "lat"), remove=F, crs=4326)
  fish.dat_sf <- st_intersection(fish.dat_sf, survey)
  
  # subset out subarea if necessary
  if(!is.null(subarea)) {
    fish.dat_sf <- st_intersection(fish.dat_sf, other)
  }
  
  source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/create_grid.R")
  #sf_use_s2(FALSE)
  r <- create_grid(gridsize = grids, polygon = st_transform(st_as_sf(survey, crs=4326, coords=c("X", "Y")), 32619))
  #sf_use_s2(TRUE)
  
  fish.dat_sf <- st_intersection(st_transform(fish.dat_sf, 32619), r)
  
  foot_grid <- fish.dat_sf %>%
    st_transform(32619) %>%
    group_by(sfa, cell, year) %>%
    dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                     hm = sum(hm, na.rm=T),
                     cpue = kg/hm) %>%
    mutate(fleet="all")
  
  foot_grid_ft <- fish.dat_sf %>%
    filter(fleet=="FT") %>%
    st_transform(32619) %>%
    group_by(sfa, cell, year) %>%
    dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                     hm = sum(hm, na.rm=T),
                     cpue = kg/hm)%>%
    mutate(fleet="FT")
  
  foot_grid_wf <- fish.dat_sf %>%
    filter(fleet=="WF") %>%
    st_transform(32619) %>%
    st_intersection(r) %>%
    group_by(sfa, cell, year) %>%
    dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                     hm = sum(hm, na.rm=T),
                     cpue = kg/hm)%>%
    mutate(fleet="WF")
  
  foot_grid <- rbind(foot_grid, foot_grid_ft, foot_grid_wf)
  st_geometry(foot_grid) <- NULL
  foot_grid <- left_join(r, foot_grid)
  
  #ggplot() + geom_sf(data=foot_grid[foot_grid$year==2025 & foot_grid$fleet=="all" & foot_grid$kg>0,], aes(fill=kg))
  
  # Turn catch into tonnage...
  foot_grid$catch <- foot_grid$kg/1000
  
  ## Now create some annual data summaries
  # Quick check that I'm getting what I want...
  tots <- foot_grid %>%
    group_by(year, fleet) %>%
    summarize(landings=sum(catch),
              effort=sum(hm),
              med.catch=median(catch,na.rm=T),
              med.effort=median(hm,na.rm=T),
              med.cpue=median(cpue),
              cells=n(),
              area=units::set_units(sum(st_area(geometry)), "km2")) %>%
    st_drop_geometry()
 
  tots[tots$fleet=="all",]
  
  # Get the number and proportion of cells that experienced high catches...
  high.catch <- foot_grid %>%
    filter(catch >= high) %>%
    group_by(year,fleet) %>%
    summarize(high.cells=n()) %>%
    st_drop_geometry() %>%
    full_join(tots) %>%
    mutate(high.prop = high.cells/cells)
  
  # Get the number and proportion of cells that experienced high CPUE
  high.cpue <- foot_grid %>%
    filter(cpue >= highcpue) %>%
    group_by(year,fleet) %>%
    summarize(highcpue.cells=n()) %>%
    st_drop_geometry() %>%
    full_join(tots) %>%
    mutate(highcpue.prop = highcpue.cells/cells)
  
  if(is.null(subarea)) {
    basemap <- ggplot() + 
      geom_sf(data=survey[survey$ID==bnk,], fill=NA) + 
      theme_bw()
  }
  
  if(!is.null(subarea)) {
    bound <- st_buffer(st_as_sfc(st_bbox(other[other$ID==subarea,])), 10)
    survey_plot <- st_crop(survey[survey$ID==bnk,], bound)
    basemap <- ggplot() + 
      geom_sf(data=survey_plot, fill=NA) + 
      geom_sf(data=other[other$ID==subarea,]) +
      theme_bw()
  }
  
  foot_grid <- foot_grid[!is.na(foot_grid$year),]
  
  map_catch <- basemap +
    geom_sf(data=foot_grid[foot_grid$catch<=high,], colour="grey") + 
    geom_sf(data=foot_grid[foot_grid$catch>high,]) + 
    facet_wrap(~year) +
    ggtitle(paste0(unique(foot_grid$sfa), ", ", "cells above ", high, "t"))
  
  map_cpue <- basemap + 
    geom_sf(data=foot_grid[foot_grid$cpue<=highcpue,], colour="grey") + 
    geom_sf(data=foot_grid[foot_grid$cpue>highcpue,]) + 
    facet_wrap(~year) +
    ggtitle(paste0(unique(foot_grid$sfa), ", ", "cells above ", highcpue, "kg/hm"))

  
  
  
  ## Now take a summary of these summaries...
  annual.summary <- full_join(high.catch, high.cpue)
  annual.summary <- select(annual.summary, year, fleet, cells, area, landings, effort, med.catch, med.effort, med.cpue, high.cells, high.prop, highcpue.cells, highcpue.prop)
 
  # if(!is.null(months)) {
  #   annual.summary$month <- substr(annual.summary$year, 6, nchar(annual.summary$year))
  #   annual.summary$year <- substr(annual.summary$year, 1, 4)
  #   annual.summary$date <- ymd(paste0(annual.summary$year, "-", annual.summary$month, "-01"))
  # }
  
  if(is.null(months)) {
    annual.summary$date <- ymd(paste0(annual.summary$year, "-01-01"))
  }
  
  annual.summary <- annual.summary[annual.summary$fleet=="all",]
  
  a <- ggplot() + geom_point(data=annual.summary, aes(date, med.catch)) + 
    geom_line(data=annual.summary, aes(date, med.catch)) + theme_bw() + ylab("Median catch\nper cell (t)")+
    scale_x_date(date_labels = "%b %Y") + theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  b <- ggplot() + geom_point(data=annual.summary, aes(date, med.effort)) + 
    geom_line(data=annual.summary, aes(date, med.effort)) + theme_bw() + ylab("Median effort\nper cell (hm)")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  c <- ggplot() + geom_point(data=annual.summary, aes(date, cells)) + 
    geom_line(data=annual.summary, aes(date, cells)) + 
    theme_bw() + ylab("Number of\ncells fished")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  d <- ggplot() + geom_point(data=annual.summary, aes(date, high.cells)) + 
    geom_line(data=annual.summary, aes(date, high.cells)) + theme_bw() + ylab("Number of\ncells above\nhigh threshold")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  e <- ggplot() + geom_point(data=annual.summary, aes(date, high.prop)) + 
    geom_line(data=annual.summary, aes(date, high.prop)) + theme_bw() + ylab("Proportion of\ncells above\nhigh threshold")+
    scale_x_date(date_labels = "%b %Y")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  # fplot <- ggplot() + geom_point(data=annual.summary, aes(date, num_cells_75perc_catch)) + 
  #   geom_line(data=annual.summary, aes(date, num_cells_75perc_catch)) + theme_bw() +
  #   scale_x_date(date_labels = "%b %Y") + ylab("Number of\ncells with\n75% of catch")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  # g <- ggplot() + geom_point(data=annual.summary, aes(date, prop_cells_fished_75perc)) + 
  #   geom_line(data=annual.summary, aes(date, prop_cells_fished_75perc)) + theme_bw() +
  #   scale_x_date(date_labels = "%b %Y") + ylab("Proportion of\ncells fished with\n75% of catch")+ theme(axis.title.y = element_text(angle=360, vjust=0.5, hjust=1))
  
  require(patchwork)
  cell_stats <- a/b + plot_annotation(
    title = "Gridded area summary",
    subtitle = paste0(unique(foot_grid$sfa), ", high threshold = ", high, "t")
  )
  
  threshold_stats <- c/d/e + plot_annotation(
    title = "Fishery footprint summary",
    subtitle = paste0(unique(foot_grid$sfa), ", high threshold = ", high, "t")
  )
  
  # perc_catch_stats <- fplot/g + plot_annotation(
  #   title = "Percentage of catch",
  #   subtitle = paste0(area, ", 75% of total catch"))
  
  # medcatch <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
  #                                    c(median_catch, median_effort, cells_fished, prop_high, num_high)),
  #                       annual.summary[,c("date", "median_catch")], by="date")
  # medcatch$name <- factor(medcatch$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  # f <- ggplot() + geom_text(data=medcatch, aes(median_catch, value, label=date, colour=date)) +
  #   geom_path(data=medcatch, aes(median_catch, value)) + ylab(NULL)+
  #   facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
  #   theme_bw()+
  #   theme(strip.placement = "outside", strip.background = element_blank()) +
  #   scale_colour_continuous(guide="none")
  # 
  # medeff <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
  #                                  c(median_catch, median_effort, cells_fished, prop_high, num_high)),
  #                     annual.summary[,c("date", "median_effort")], by="date")
  # medeff$name <- factor(medeff$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  # g <- ggplot() + geom_text(data=medeff, aes(median_effort, value, label=date, colour=date)) +
  #   geom_path(data=medeff, aes(median_effort, value)) +ylab(NULL)+
  #   facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
  #   theme_bw()+
  #   theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
  #   scale_colour_continuous(guide="none")
  # 
  # cellsf <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
  #                                  c(median_catch, median_effort, cells_fished, prop_high, num_high)),
  #                     annual.summary[,c("date", "cells_fished")], by="date")
  # cellsf$name <- factor(cellsf$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  # h <- ggplot() + geom_text(data=cellsf, aes(cells_fished, value, label=date, colour=date)) +
  #   geom_path(data=cellsf, aes(cells_fished, value)) +ylab(NULL)+
  #   facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
  #   theme_bw()+
  #   theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
  #   scale_colour_continuous(guide="none")
  # 
  # proph <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
  #                                 c(median_catch, median_effort, cells_fished, prop_high, num_high)),
  #                    annual.summary[,c("date", "prop_high")], by="date")
  # proph$name <- factor(proph$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  # i <- ggplot() + geom_text(data=proph, aes(prop_high, value, label=date, colour=date)) +
  #   geom_path(data=proph, aes(prop_high, value)) +ylab(NULL)+
  #   facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
  #   theme_bw()+
  #   theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank()) +
  #   scale_colour_continuous(guide="none")
  # 
  # numh <- left_join(pivot_longer(annual.summary[,c("date", "median_catch", "median_effort", "cells_fished", "prop_high", "num_high")], 
  #                                c(median_catch, median_effort, cells_fished, prop_high, num_high)),
  #                   annual.summary[,c("date", "num_high")], by="date")
  # numh$name <- factor(numh$name, levels=c("median_catch", "median_effort", "cells_fished", "prop_high", "num_high"))
  # j <- ggplot() + geom_text(data=numh, aes(num_high, value, label=date, colour=date)) +
  #   geom_path(data=numh, aes(num_high, value)) +ylab(NULL)+
  #   facet_wrap(~name, ncol=1, scales="free_y", strip.position="left") +
  #   theme_bw()+
  #   theme(strip.text = element_blank(), strip.placement = "outside", strip.background = element_blank())
  # 
  # footprint_crosscor <- (f|g|h|i|j) + plot_annotation(
  #   title = "Fishery footprint cross-correlation",
  #   subtitle = paste0(area, ", high threshold = ", high, "t"))
  
  return(list(map_catch = map_catch, map_cpue = map_cpue, cell_stats=cell_stats, threshold_stats=threshold_stats, annual.summary=annual.summary))#, perc_catch_stats=perc_catch_stats, footprint_crosscor=footprint_crosscor, annual_summary=annual.summary))
}
