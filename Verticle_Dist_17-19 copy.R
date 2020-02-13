# CCFRP_statewide.r
# ========================================================================================================================
#  Setup
# ========================================================================================================================
rm(list=ls())
#install.packages('sf')
#install.packages('ggplot2')
#install.packages('mvtnorm')
#install.packages('multcomp')
#install.packages('emmeans')
require(multcomp)
require(sf)
require(dplyr)
require(ggplot2)
require(reshape2)
require(scales)
library("readxl")
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
#load(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/fishes_17-19 (1).csv")
#load(file = '/Users/haleyhudson/Desktop/CCFRP Data Analysis /Zip Files/Wseaboard_df')
# ========================================================================================================================
#  BML 2017-2019
# ========================================================================================================================
# Grid Cells
# Ex:grids <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/Grid_Cell_Locations.csv",
grids <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/Grid_Cell_Locations.csv",
                  header = TRUE, stringsAsFactors = FALSE)
# Fish Codes
# EX: fish.codes <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/Fish_Species.csv",
fish.codes <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/Fish_Species.csv",
                       header = TRUE, stringsAsFactors = FALSE)
# Ex: sites <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/Monitoring_Areas.csv",
sites <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/Monitoring_Areas.csv",
                  header = TRUE, stringsAsFactors = FALSE)
sites <- sites[-which(sites$area == ""), ]
# Trip Info
#Ex: trip <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/1-Trip_Information.csv",
trip <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/trip_info_17-19.csv",
                 header = TRUE, stringsAsFactors = FALSE)
# Angler Info
# EX: anglers <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/2-Angler_Information.csv",
anglers <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/2-Angler_Information.csv",
                    header = TRUE, stringsAsFactors = FALSE)
# Drift Info
#EX: drifts <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/3-Drift_Information.csv",
drifts <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/drift_info_17-19.csv",
                   header = TRUE, stringsAsFactors = FALSE)
drifts$loc <- substr(drifts$drift.ID, 1,2)
drifts$Year <- as.numeric(paste0("20",substr(drifts$drift.ID, 8, 9)))
View(drifts)
# drifts$Inst <- sites$Monitoring.Group[which(sites$Area.code == drifts$loc)]
#drifts$Inst <- unlist(lapply(drifts$loc, function(x)sites$Monitoring.Group[which(sites$area == x)]))

# Caught Fish
# Ex: fish <- read.csv(file = "/Users/Connor/Documents/Graduate School/Dibble_Research/CCFRP_2017/Data/Statewide_through2018/4-Caught_Fishes.csv",
fish <- read.csv(file = "/Users/haleyhudson/Desktop/CCFRP Data Analysis /Data/fishes_17-19 (1).csv",
                 header = TRUE, stringsAsFactors = FALSE)
# Add some more readable site information
fish$site <- ifelse(substr(fish$drift.ID, 3,3) == "M", "MPA", "REF")
fish$loc <- substr(fish$drift.ID, 1,2)
fish$Year <- as.numeric(paste0("20",substr(fish$drift.ID, 8, 9)))
#fish$Inst <- unlist(lapply(fish$loc, function(x)sites$Monitoring.Group[which(sites$area == x)]))
#fish$Trip.ID <- unlist(lapply(fish$drift.ID, function(x)drifts[which(drifts$drift.ID == x), "Trip.ID"]))
# get avg. lat/lon of each place so can arrange data by it.
sites <- left_join(sites,
                   fish %>% group_by(loc, site) %>% select(loc, site) %>%
                     #summarize_all(.funs = mean, na.rm = TRUE) %>% dplyr::filter(site == "MPA") %>% select(loc, Lat.Released..DD., Lon.Released..DD.),
                     summarize_all(.funs = mean, na.rm = TRUE) %>% dplyr::filter(site == "MPA") %>% select(loc),
                   by = c("area" = "loc"))
sites <- left_join(sites,
                   #fish %>% group_by(loc, site) %>% select(loc, site, Lat.Released..DD., Lon.Released..DD.) %>%
                   fish %>% group_by(loc, site) %>% select(loc, site) %>%
                     #summarize_all(.funs = mean, na.rm = TRUE) %>% dplyr::filter(site == "REF") %>% select(loc, Lat.Released..DD., Lon.Released..DD.),
                     summarize_all(.funs = mean, na.rm = TRUE) %>% dplyr::filter(site == "REF") %>% select(loc),
                   by = c("area" = "loc"), suffix = c(".MPA", ".REF"))
#fish$loc <- factor(fish$loc, levels = sites %>% arrange(Lat.Released..DD..MPA) %>% select(Area.code) %>% unlist %>% as.character())
#fish$loc <- factor(fish$loc, levels = sites %>% select(area) %>% unlist %>% as.character())
View(fish)
# ========================================================================================================================
#  Computing CPUE
# ========================================================================================================================
# Overall (across gear types) CPUE
se <- function(x, ...) sqrt(var(x, na.rm = TRUE)/length(x[!is.na(x)]))
fish.cpue <- fish %>% dplyr::select(Year, drift.ID, species, length, loc, site) %>%
  group_by(drift.ID, Year, loc, site, species) %>%
  summarize_if(.predicate = is.numeric , .funs = funs(Mean = mean(., na.rm = TRUE), SE = se(., na.rm = TRUE), N = n()))
View(fish.cpue)
#Anglers <- drifts$Total.Anglers.Fishing
#fish.cpue$Number_Anglers <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Total.Anglers.Fishing))
#fish.cpue$Number_Anglers <- trip$Number.Volunteer.Anglers
# Total.Angler.hrs has already been corrected
#View(drifts)
#fish.cpue$Total.Angler.hrs <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Total.Angler.Hrs))
fish.cpue$Total.Angler.hrs <- lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Total.Angler.Hrs)
#View(fish.cpue)
# fish.cpue$Total.Angler.hrs.Corrections <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Total.......Time..hrs.))
#fish.cpue$Drift.Time.hrs <- unlist(lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Drift.Time..hrs.))
fish.cpue$Drift.Time.hrs <- lapply(fish.cpue$drift.ID, function(x)dplyr::filter(drifts, drift.ID == x)$Drift.Time..hrs.)
#fish.cpue$Effort <- fish.cpue$Number_Anglers * fish.cpue$Drift.Time..hrs. + fish.cpue$Correction_Mins/60
#unlist(fish.cpue$N, recursive = TRUE, use.names = TRUE)
fish.cpue$Total.Angler.hrs <- as.numeric(fish.cpue$Total.Angler.hrs)
fish.cpue$CPUE <- fish.cpue$N / fish.cpue$Total.Angler.hrs
View(fish.cpue)
#fish.cpue$BPUE <- fish.cpue$CPUE*fish.cpue$length
# ========================================================================================================================
#  Orgainze by Vertical Distribution 
# ========================================================================================================================
Water.column.fish.list <- list('BLA', 'BLU', 'CNY', 'CSA', 'DEA', 'OLV', 'YTL')
print(Water.column.fish.list)
fish.cpue$Vertical_Dist <- ifelse(fish.cpue$species %in% Water.column.fish.list, "WC", "BT")
#View(fish.cpue)
# ========================================================================================================================
#  Summarize data for ggplot
# ========================================================================================================================
df.sum.across.guilds.per.drift <- fish.cpue %>% group_by(loc, site, Vertical_Dist, drift.ID) %>% 
  summarize(CPUE_by_Guild = sum(CPUE, na.rm = TRUE)) %>% ungroup()
#View(df.sum.across.guilds.per.drift)
df.mean.per.site <- df.sum.across.guilds.per.drift %>% group_by(loc, site, Vertical_Dist) %>% 
  summarize_if(.predicate = is.numeric, .funs = c(Mean = mean, SE = se))
#View(df.mean.per.site)
# ========================================================================================================================
#  Split summarized data into BH and SP
# ========================================================================================================================
BH_summary <- filter(df.mean.per.site, loc=='BH')
#View(BH_summary)
SP_summary <- filter(df.mean.per.site, loc=='SP')
#View(SP_summary)
# ========================================================================================================================
#  ggplot BH
# ========================================================================================================================
ggplot(BH_summary) + geom_bar(aes(x = Vertical_Dist, y = Mean, fill = site), 
                                    stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(x = Vertical_Dist, ymin = Mean - SE, ymax = Mean + 
                      SE, group = site), position = 'dodge')+ ggtitle("Bodega Head") +
  theme_classic( base_size = 22) + scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod1"))+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Vertical Distribution", y = "Mean CPUE (+/- 1 SD)") + scale_x_discrete(labels=c("Bottom Fish", "Water Column Fish")) + 
  guides(fill = guide_legend(title = "", direction = "horizontal")) + theme(legend.position = c(0.25, 0.85))
# ========================================================================================================================
#  ggplot SP
# ========================================================================================================================
ggplot(SP_summary) + geom_bar(aes(x = Vertical_Dist, y = Mean, fill = site), 
                                    stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(x = Vertical_Dist, ymin = Mean - SE, ymax = Mean + 
                      SE, group = site), position = 'dodge')+ ggtitle("Stewarts Point") +
theme_classic( base_size = 22) + scale_fill_manual(values=c("cornflowerblue", "darkgoldenrod1"))+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Vertical Distribution", y = "Mean CPUE (+/- 1 SD)") + scale_x_discrete(labels=c("Bottom Fish", "Water Column Fish")) + guides(fill = guide_legend(title = "", direction = "horizontal")) + 
  theme(legend.position = c(0.25, 0.85))
# ========================================================================================================================
#  Linear Model 
# ========================================================================================================================
m1 <- lm(CPUE_by_Guild ~ Vertical_Dist + site + loc + site:loc + site:loc:Vertical_Dist, data = df.sum.across.guilds.per.drift)
m1_tukey <- TukeyHSD(aov(m1))
library(multcomp)
summary(multcomp::glht(m1_tukey, mcp(rank="Tukey")))
#m1_tukey
#View(m1_tukey)

library(multcomp)
m1 <- lm(CPUE_by_Guild ~ Vertical_Dist + site + loc + site:loc + site:loc:Vertical_Dist, data = df.sum.across.guilds.per.drift)
m1
glht(m1, linfct = c(" Vertical_Dist = 0","site = 0","loc = 0", "site:loc", "site:loc:Vertical_Dist"))

summary(multcomp::glht(m1_tukey, mcp(rank="Tukey")))

#mult_comps <- m1_tukey$`Vertical_Dist:site:loc`
#mult_comps$comparison <- row.names(mult_comps)
#my_comparisons <- c('WC:REF:SP-WC:MPA:SP','BT:REF:SP-BT:MPA:SP','WC:REF:BH-WC:MPA:BH','BT:REF:BH-BT:MPA:BH')
#dplyr::filter(mult_comps, comparison %in% my_comparisons)
#View(my_comparisons)
#con.data <- df.sum.across.guilds.per.drift(x = c(rnorm(75), c(rnorm(75)+5)),

#m1_turkey$category = rep(c("BT","WC","MPA","REF","SP","BH"), each=30)
#contrasts(m1_tukey$category) <- cbind(c(1,-1/4,-1/4,-1/4,-1/4),
                                      #c(0,-1/2,-1/2,1/2,1/2),
                                      #c(0,0,0,1/2,-1/2),
                                      #c(0,-1/2,1/2,0,0))

m1 <- lm(CPUE_by_Guild ~ Vertical_Dist + site + loc + site:loc + site:loc:Vertical_Dist, data = df.sum.across.guilds.per.drift)
emmeans::ref_grid(m1, at = list(Vertical_Dist = 0:1, site = 0:1, loc = 0:1))

# ========================================================================================================================
# summarys 
# ========================================================================================================================


