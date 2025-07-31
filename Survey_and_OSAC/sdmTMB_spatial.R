require(sdmTMB)
dat <- surv.Rand$BBs[surv.Rand$BBs$year==2025,]
summary(dat$pre.bm)
summary(dat$rec)
summary(dat$com)
#dat <- dat[dat$pre<1000,]
loc.sf <- st_as_sf(dat, coords=c(x="lon", y="lat"), crs=4326)
loc.sf <- st_transform(loc.sf, 32620)
st_geometry(loc.sf) <- st_geometry(loc.sf)/1000
loc.df <- as.data.frame(st_coordinates(loc.sf))
loc.sf <- cbind(as.data.frame(loc.sf), st_coordinates(loc.sf))

mesh <- sdmTMB::make_mesh(data = loc.df, 
                           xy_cols = c("X", "Y"), 
                           fmesher_func = fmesher::fm_mesh_2d_inla,
                           cutoff = 5, # minimum triangle edge length
                           max.edge = c(5, 5), # inner and outer max triangle lengths
                           offset = c(5, 5)) # inner and outer border widths)
plot(mesh)

fit_spatial <- sdmTMB(
  rec.bm ~ 1, #?
  data = loc.sf,
  family = poisson(),
  mesh = mesh,
  spatial = "on")#,
  #control = sdmTMBcontrol(newton_loops = 1)#
  # silent = FALSE
#)

sanity(fit_spatial)
fit_spatial

newdata <- expand.grid(X=seq(min(loc.sf$X), max(loc.sf$X), 0.1),
                       Y=seq(min(loc.sf$Y), max(loc.sf$Y), 0.1))

overall <- predict(fit_spatial, newdata=newdata)

ggplot() + geom_sf() +
  geom_raster(data = overall, aes(x = X, y = Y, fill = exp(est))) +
  #xlim(281, 284) +
  #ylim(4709,4711) +
  scale_fill_viridis_c() +
  theme_light() +
  labs(fill = "Predicted\ndensity") +
  labs(x = "Longitude", y = "Latitude")

summary(exp(overall$est))
# PRE: tweedie gives all the same; BBn has variability though. BBs Pre works if you remove tows >1000
# REC: tweedie gives all the same; BBn has variability though
# COM: tweedie works
# PRE.BM works!
# REC.BM fails


fit_spatial_pois <- sdmTMB(
  pre.bm ~ 1, #?
  data = loc.sf,
  family = poisson(link="log"),#tweedie(link = "log"),
  mesh = mesh2,
  spatial = "on")#,
#control = sdmTMBcontrol(newton_loops = 1)#
# silent = FALSE
#)

sanity(fit_spatial_pois)
fit_spatial_pois

newdata <- expand.grid(X=seq(min(loc.sf$X), max(loc.sf$X), 0.1),
                       Y=seq(min(loc.sf$Y), max(loc.sf$Y), 0.1))

overall <- predict(fit_spatial_pois, newdata=newdata)

ggplot() + geom_sf() +
  geom_raster(data = overall, aes(x = X, y = Y, fill = exp(est))) +
  #xlim(281, 284) +
 # ylim(4709,4711) +
  scale_fill_viridis_c() +
  theme_light() +
  labs(fill = "Predicted\ndensity") +
  labs(x = "Longitude", y = "Latitude")

summary(overall$est)
# PRE: poisson does not, but numbers are crazy? Same for BBn.
# REC: variability is fine, numbers seem high
# PRE.BM works
# REC.BM fails

##########
fit_spatial_nb2 <- sdmTMB(
  pre.bm ~ 1, #?
  data = loc.sf,
  family = nbinom2(),#tweedie(link = "log"),
  mesh = mesh2,
  spatial = "on")#,
#control = sdmTMBcontrol(newton_loops = 1)#
# silent = FALSE
#)

sanity(fit_spatial_nb2)
fit_spatial_nb2

newdata <- expand.grid(X=seq(min(loc.sf$X), max(loc.sf$X), 0.1),
                       Y=seq(min(loc.sf$Y), max(loc.sf$Y), 0.1))

overall <- predict(fit_spatial_nb2, newdata=newdata)

ggplot() + geom_sf() +
  geom_raster(data = overall, aes(x = X, y = Y, fill = exp(est))) +
  #xlim(281, 284) +
  # ylim(4709,4711) +
  scale_fill_viridis_c() +
  theme_light() +
  labs(fill = "Predicted\ndensity") +
  labs(x = "Longitude", y = "Latitude")

summary(overall$est)
# PRE: nbinom2 no good
# PRE.BM: works
