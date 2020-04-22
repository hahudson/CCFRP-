# CCFRP_BML_17-19.r
# ========================================================================================================================
#  Setup
# ========================================================================================================================
	rm(list=ls())
	require(dplyr)
	require(ggplot2)
	require(reshape2)
	require(scales)
	theme_tsg <- theme(axis.text.x = element_text(angle= 90),
					panel.grid.major = element_line(color = "grey55", size = 0.5), panel.grid.minor = element_line(color = "grey90"))
	scale_x_datetime_tsg <- function(...){
		structure(list(
			scale_x_datetime(expand = c(0,0), breaks = date_breaks(width = "1 day"), minor_breaks = date_breaks(width = "1 hour"), date_labels = "%m-%d", ...)
			))
	}
	se <- function(x, ...) sqrt(var(x, na.rm = TRUE)/length(x[!is.na(x)]))
	dname <- function(df, old, new){
		new.names <- names(df)
		new.names[which(new.names == old)] <- new
		return(new.names)
	}
	'%nin%' <- Negate('%in%')
# ========================================================================================================================
#  Data Import
# ========================================================================================================================
# Grid Cells
	grids <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Sampling_Design/2018_Final_GridCells/Final_Bodega_Cell_DecimalDegree_180417_melt.csv",
		header = TRUE, stringsAsFactors = FALSE)
	grid.cell.list <- data.frame(ID = grep('_1', unique(grids$ID), value = TRUE))
	grid.cell.list$Site <- substr(grid.cell.list$ID, 1,2)
	grid.cell.list$Cell <- substr(grid.cell.list$ID, 3,4)
	# Fish Codes
	fish.codes.2018 <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/2018_Data_Excel/2018_CCFRP_Fish_List.csv",
		header = TRUE, stringsAsFactors = FALSE)
	# GPS Waypoints
	# wpts.2018 <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/2018_Data_Excel/5-Waypoints.csv",
	# 	header = TRUE, stringsAsFactors = FALSE)
	# Trip Info
	trip <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/trip_info_17-19.csv",
		header = TRUE, stringsAsFactors = FALSE)
	# Angler Info
		# anglers.2018 <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/2018_CCFRP_Angler_Info.csv",
		# 	header = TRUE, stringsAsFactors = FALSE)
	# anglers.e.2018 <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/2-Angler_Info_Expanded.csv",
		# header = TRUE, stringsAsFactors = FALSE)
		# QAQC this
			# ang18 <- dplyr::filter(anglers.e, Dates.Fished.2018 != "")
			# ang18[grep('BHT01_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT01_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT02_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT03_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('BHT02_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('BHT03_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('BHT04_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT04_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('BHT05_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT05_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('SPT06_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
			# ang18[grep('BHT06_18', ang18$Dates.Fished.2018),] %>% select(Angler.ID, Angler.Name, Station.., Dates.Fished.2018)
	# Drift Info
	# drifts <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/2018_CCFRP_Drift_Info.csv",
	# 	header = TRUE, stringsAsFactors = FALSE)
	drifts <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/drift_info_17-19.csv",
		header = TRUE, stringsAsFactors = FALSE)
		# QAQC
			# Need a melted version of drifts with Lat/Lon pairs in a single row for each drift start and each drift stop.
				drifts.Lats <- drifts %>% select(Drift.ID, Trip.ID, Grid.Cell.ID, Site..MPA..REF., ST_LatDD, End_LatDD ) %>% melt(id.vars = c("Drift.ID", "Trip.ID", "Grid.Cell.ID", "Site..MPA..REF."))
				names(drifts.Lats) <- dname(drifts.Lats, "variable", "Latitude_v")
				names(drifts.Lats) <- dname(drifts.Lats, "value", "Latitude")
				drifts.Lats$Start <- ifelse(substr(drifts.Lats$Latitude_v, 1,2) == "ST", "Start", "End")
				drifts.Lons <- drifts %>% select(Drift.ID, Trip.ID, Grid.Cell.ID, Site..MPA..REF., ST_LonDD, End_LonDD) %>% melt(id.vars = c("Drift.ID", "Trip.ID", "Grid.Cell.ID", "Site..MPA..REF."))
				names(drifts.Lons) <- dname(drifts.Lons, "variable", "Longitude_v")
				names(drifts.Lons) <- dname(drifts.Lons, "value", "Longitude")
				drifts.Lons$Start <- ifelse(substr(drifts.Lons$Longitude_v, 1,2) == "ST", "Start", "End")
					# put drifts into a melted table so they can be mapped
				drifts.LL <- left_join(drifts.Lats, drifts.Lons, by = c("Drift.ID", "Trip.ID", "Grid.Cell.ID", "Site..MPA..REF.", "Start"))
			# plot the drifts
				# plot all (don't add legends as they will over plot)
				ggplot(drifts.LL) +
					geom_point(data = grids, aes(x = Lon, y = Lat, fill = ID), alpha = 0.25, shape = 21, size = 0.1) +
					geom_path(aes(x = Longitude, y = Latitude, color = Drift.ID, group = Drift.ID)) +
					coord_equal() + theme_bw() + guides(color = 'none', fill = 'none')
				# plot zoom- use this to narrow in on certain areas
					min.lat <- 38.2
					max.lat <- 38.6
				ggplot(drifts.LL %>% dplyr::filter(Latitude < max.lat, Latitude > min.lat, Trip.ID == "BHT01_18")) +
					geom_path(aes(x = Longitude, y = Latitude, color = Drift.ID, group = Drift.ID)) +
					geom_point(data = grids %>% dplyr::filter(Lat < max.lat, Lat > min.lat), aes(x = Lon, y = Lat, fill = ID), alpha = 0.5, shape = 21) +
					coord_equal() + theme_bw() + guides(color = 'none')	
				# Look at the drifts within the latitudinal range defined above in the plot zoom.
				drifts %>% dplyr::filter(ST_LatDD < max.lat, ST_LatDD > min.lat) %>% select(Trip.ID, Drift.ID, ST_LatDD, ST_LonDD, End_LatDD, End_LonDD)
	# Caught Fish
	fish <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/BML_CCFRP_17-19/fishes_17-19.csv",
		header = TRUE, stringsAsFactors = FALSE)
		# can change the names so they are more readable, but probably not worth it b/c of need to go back and forth to Access.
			# names(fish) <- c("Fish.ID", "Drift.ID", "Station", "Species.Code", "Length.cm", "Tag.ID", "Gear.Type",
			# 	"Angler.ID", "Depth.Released.ft", "Lat.Released.DD","Lon.Released.DD", "Sex","C0","C1","C2","C3","C4","C5","C6","C7","C8","AllC",
			# 	"Retained","Recapture", "Comments", "GPS.Waypoint", "waypoint_link")
	# Add some more readable site information
	fish$site <- ifelse(substr(fish$Drift.ID, 3,3) == "M", "MPA", "REF")
	fish$loc <- substr(fish$Drift.ID, 1,2)
	fish$loc <- ifelse(fish$loc == "BH", "Bodega Head", "Stewart's Point")


	fish$site <- ifelse(substr(fish$drift.ID, 3,3) == "M", "MPA", "REF")
	fish$loc <- substr(fish$drift.ID, 1,2)
	fish$Year <- as.numeric(paste0("20",substr(fish$drift.ID, 8, 9)))
	fish.cpue <- fish %>% dplyr::select(Year, drift.ID, species, length, loc, site) %>%
	  group_by(drift.ID, Year, loc, site, species) %>%
	  summarize_if(.predicate = is.numeric , .funs = funs(Mean = mean(., na.rm = TRUE), SE = se(., na.rm = TRUE), N = n()))

fish.cpue$Total.Angler.hrs <- lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, Drift.ID == x)$Total.Angler.Hrs)
#View(fish.cpue)
# fish.cpue$Total.Angler.hrs.Corrections <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Total.......Time..hrs.))
#fish.cpue$Drift.Time.hrs <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Drift.Time..hrs.))
fish.cpue$Drift.Time.hrs <- lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, Drift.ID == x)$Drift.Time..hrs.)
#fish.cpue$Effort <- fish.cpue$Number_Anglers * fish.cpue$Drift.Time..hrs. + fish.cpue$Correction_Mins/60
#unlist(fish.cpue$N, recursive = TRUE, use.names = TRUE)
fish.cpue$Total.Angler.hrs <- as.numeric(fish.cpue$Total.Angler.hrs)
fish.cpue$CPUE <- fish.cpue$N / fish.cpue$Total.Angler.hrs


Water.column.fish.list <- list('BLA', 'BLU', 'CNY', 'CSA', 'DEA', 'OLV', 'YTL')
print(Water.column.fish.list)
fish.cpue$Vertical_Dist <- ifelse(fish.cpue$species %in% Water.column.fish.list, "WC", "BT")

df.sum.across.guilds.per.drift <- fish.cpue %>% group_by(loc, site, Vertical_Dist, drift.ID) %>% 
  summarize(CPUE_by_Guild = sum(CPUE, na.rm = TRUE)) %>% ungroup()
#View(df.sum.across.guilds.per.drift)
df.mean.per.site <- df.sum.across.guilds.per.drift %>% group_by(loc, site, Vertical_Dist) %>% 
  summarize_if(.predicate = is.numeric, .funs = c(Mean = mean, SE = se))

df.sum.across.guilds.per.drift$site <- as.factor(df.sum.across.guilds.per.drift$site)
df.sum.across.guilds.per.drift$loc <- as.factor(df.sum.across.guilds.per.drift$loc)
df.sum.across.guilds.per.drift$Vertical_Dist <- as.factor(df.sum.across.guilds.per.drift$Vertical_Dist)
df.sum.across.guilds.per.drift$drift.ID <- as.factor(df.sum.across.guilds.per.drift$drift.ID)

m1 <- glm((CPUE_by_Guild + 1) ~ Vertical_Dist + site + loc + site:loc + site:loc:Vertical_Dist, family = Gamma(link = log), data = df.sum.across.guilds.per.drift)
m1 <- glm((CPUE_by_Guild + 1) ~ Vertical_Dist + site + loc + site:loc, family = Gamma(link = log), data = df.sum.across.guilds.per.drift)

# refGrid = emmeans::ref_grid(m1, at = list(Vertical_Dist = 0:1, site = 0:1, loc = 0:1), cov.keep = c("Vertical_dist", "site", "loc"))
# refGrid = emmeans::ref_grid(m1, cov.keep = c("Vertical_dist", "site", "loc"))
refGrid <- emmeans::ref_grid(m1, at = c(Vertical_Dist = 0:1, site = 0:1, loc = 0:1))
refGrid.s <- emmeans::emmeans(refGrid, specs = c("Vertical_Dist", "site", "loc"))
refGrid.s <- emmeans::emmeans(m1, specs = c("Vertical_Dist", "site", "loc"))
refGrid.s
'''
 Vertical_Dist site loc emmean     SE  df asymp.LCL asymp.UCL
 BT            MPA  BH   1.644 0.1048 Inf     1.438      1.85
 WC            MPA  BH   1.596 0.1615 Inf     1.280      1.91
 BT            REF  BH   1.330 0.1002 Inf     1.133      1.53
 WC            REF  BH   1.886 0.1390 Inf     1.614      2.16
 BT            MPA  SP   1.341 0.0929 Inf     1.159      1.52
 WC            MPA  SP   2.817 0.0917 Inf     2.637      3.00
 BT            REF  SP   0.559 0.1027 Inf     0.358      0.76
 WC            REF  SP   1.771 0.0957 Inf     1.583      1.96
'''
WC_MPA_SP = c(0, 0, 0, 0, 0, 1, 0, 0)
WC_REF_SP = c(0, 0, 0, 0, 0, 0, 0, 1)

BT_MPA_SP = c(0, 0, 0, 0, 1, 0, 0, 0)
BT_REF_SP = c(0, 0, 0, 0, 0, 0, 1, 0)

WC_MPA_BH = c(0, 1, 0, 0, 0, 0, 0, 0)
WC_REF_BH = c(0, 0, 0, 1, 0, 0, 0, 0)

BT_MPA_BH = c(1, 0, 0, 0, 0, 0, 0, 0)
BT_REF_BH = c(0, 0, 1, 0, 0, 0, 0, 0)

emmeans::contrast(refGrid.s, method = list(WC_MPA_SP - WC_REF_SP) )
emmeans::contrast(refGrid.s, method = list(BT_MPA_SP - BT_REF_SP) )
emmeans::contrast(refGrid.s, method = list(WC_MPA_BH - WC_REF_BH) )
emmeans::contrast(refGrid.s, method = list(BT_MPA_BH - BT_REF_BH) )


emmeans::test(refGrid.s, adjust = "bonferroni") # or adjust = "tukey"
pairs(refGrid.s)

'''
WC,MPA,SP - WC,REF,SP <.0001
BT,MPA,SP - BT,REF,SP <.0001
WC,MPA,BH - WC,REF,BH 0.8746
BT,MPA,BH - BT,REF,BH 0.3730
'''

# Model Simulation Functions

fsim.glmm <- function(m1, nsim = 1000, cont.pred.expansion = 1, n.sim.vals = 10, inv_link_function = NULL){ # takes a glm fit model object and an integer for the number of simulations to run.
	'%nin%' <- Negate('%in%')
	if("glmerMod" %in% class(m1)){
		mdata <- m1@frame
		vars <- c(as.character(attr(attr(mdata, 'terms'), 'predvars.fixed')),
			as.character(attr(attr(mdata, 'terms'), 'predvars.random'))[3:length(as.character(attr(attr(mdata, 'terms'), 'predvars.random')))] )
		model.residual.variance <- sd(residuals(m1))
	}
	if("glm" %in% class(m1) | "lm" %in% class(m1) & 'gam' %nin% class(m1)){
		mdata <- m1$data
		vars <- as.character(attr(m1$terms, 'variables'))
		model.residual.variance <- sd(residuals(m1))
	}
	if("gam" %in% class(m1)){
		mdata <- m1$model
		vars <- as.character(attr(m1$terms, 'variables'))
		model.residual.variance <- m1$sig2
	}
	vars <- vars[-which(vars == "list")]
	y <- vars[1] # resposne variable
	x <- vars[2:length(vars)] # predictor variable(s)

	dpred <- expand.grid(
		sapply(x,
			function(x.i){
				# print(class(mdata[[x.i]]))
				if(class(mdata[[x.i]])[1] == "numeric" | class(mdata[[x.i]])[1] == "integer"){
					out <- seq(range(mdata[[x.i]], na.rm = TRUE)[1] * cont.pred.expansion, range(mdata[[x.i]], na.rm = TRUE)[2] * cont.pred.expansion, length.out = n.sim.vals)
				}
				if(class(mdata[[x.i]])[1] == "character"){
					out <- unique(mdata[[x.i]])
				}
				if(class(mdata[[x.i]])[1] == "factor"){
					out <- unique(mdata[[x.i]])
				}
				if("POSIXct" %in% class(mdata[[x.i]])){
					out <- unique(mdata[[x.i]])
				}
				return(out)
			},
		simplify = FALSE
		)
	)
	dpred$predTC.respScale <- predict(m1, newdata = dpred[ , x], type = "response", allow.new.levels = TRUE) # model predictions for new data values (linear predictors only)
	dpred$predTC.linkScale <- predict(m1, newdata = dpred[ , x], type = "link", allow.new.levels = TRUE) # model predictions for new data values (linear predictors only)
	# dsim$simTC <- simulate(m1, nsim = 100)
	# Residuals are on the link scale (log link). Inverse link is exp(). Residuals are not really well defined anyway.
	# residuals.sim <- DHARMa::simulateResiduals(m1,  n = 500)
	dpred$simTC <- dpred$predTC.linkScale + rnorm(1, mean = 0, sd = model.residual.variance) # model simulation (just one sim) for new data values (linear predictors plus uncertainty)
	dsim <- matrix(NA, nrow = nrow(dpred), ncol = nsim)
	for(i in 1:nsim){
		dsim[,i] <- dpred$predTC.linkScale + rnorm(1, mean = 0, sd = model.residual.variance)
		if(is.null(inv_link_function) == FALSE){
			# get simulation vals back to response scale instead of link scale.
			dsim[ , i] <- do.call(inv_link_function, list(dsim[ , i]))
		}
	}
	return(list(dpred = dpred, dsim = dsim))
}	
simsum <- function(sim.df){
	simsum.out <- data.frame(sim.mean = rowMeans(sim.df$dsim))
	simsum.out$lower.95 <- apply(sim.df$dsim, 1, function(x){max(sort(x)[1:round(ncol(sim.df$dsim)*0.025)])})
	simsum.out$upper.95 <- apply(sim.df$dsim, 1, function(x){max(sort(x)[1:round(ncol(sim.df$dsim)*0.975)])})
	simsum.out <- cbind(sim.df$dpred[,1:(ncol(sim.df$dpred) - 3)], simsum.out)
	return(simsum.out)
}
plot.simsum <- function(simsum.in, env_predictor){
	plot.simsum.out <- ggplot(simsum.in) + geom_hline(aes(yintercept = 0)) + geom_vline(aes(xintercept = 0)) +
		geom_ribbon(data = simsum.in, aes(x = !!sym(env_predictor), ymin = lower.95, ymax = upper.95, fill = Location), alpha = 0.2) +
		geom_line(data = simsum.in, aes(x = !!sym(env_predictor), y = sim.mean,
			color = Location)) +
		# geom_point(data = m.jet1.sim$dpred, aes(x = env_predictor, y = predTC.respScale,
		# 	color = Location, shape = catDep)) +
		# geom_point(data = cbind(m.jet1.sim$dpred[,1:3], m.jet1.sim$dsim) %>% melt(id.vars = c('catDep', 'Location', 'env_predictor')), aes(x = env_predictor, y = value,
		# 	color = Location, shape = catDep)) +
		# facet_wrap(catDep ~ ., nrow = 2) +
		facet_wrap(catDep ~ Location, nrow = 2, scales = 'free') +
		ylab("GAM simulated NMDS value (mean +/- 95% CI)") +
		theme_bw()
	return(plot.simsum.out)
}

s1 <- fsim.glmm(m1)
s2 <- simsum(s1)

ggplot(s2) + 
	geom_bar(aes(x = Vertical_Dist, y = sim.mean, fill = site), stat = 'identity', position = 'dodge') +
	geom_errorbar(aes(x = Vertical_Dist, ymin = lower.95, ymax = upper.95, fill = site), stat = 'identity', position = 'dodge') +
	facet_wrap(loc~., nrow = 2)

