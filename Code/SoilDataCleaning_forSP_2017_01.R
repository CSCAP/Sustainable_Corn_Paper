# VERSION 3
# this code contains data-cleaning for SOC and TN.
# most of the cleaning is done according to my (Gio) understanding of the data.
# few are done in order to incorproate suggestions given by PIs.
# for example: CC plots from SERF (8 plots) were removed from analysis based on Matt's request. 
# this code calculates average BD over whole site (intead of plot) by depths (like VERSION 2).
# Unlike pervious version, where BDL = (Ditection Limit / 2), 
# here we assume that BDL values are equal to the detection limit itself.
# this was done because of the erroneous C:N ratio values found in some cases (for example, at ORR)



setwd("C:/Users/Gio/Documents")

load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/soil.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/plots.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/agro.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/crot.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/metadata.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/var_names.RData")


library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(dplyr)

# BELOW DETACTION LIMIT data -------------------------------------------------------
# get rid off "< " sign in front of the BDL values and convert all values to numeric
SOIL$value <- as.double(sub("< ", "", SOIL$value))     #substitude "<" with ""
# save as the df as soil data
soil <- SOIL
rm(SOIL)


# select "soil" data with the variables of interest (BD, SOC, TN)
soil %>% 
  filter(varname %in% c("SOIL1", "SOIL13", "SOIL14")) -> soil

# get rid of ONFARM sites
soil %>% 
  filter(!grepl("ONFARM", site)) -> soil
  
# in STJOHNS soil pH and CEC were measured at 0-30, 30-60, and 60-90 cm intervals
# while BD, SOC, and TN was measured at "regular" depths corresponding to the protocol 
# remove those artifactrs from the "soil_all_data"
soil %>% 
  filter(!depth %in% c("0 to 30", "30 to 60", "60 to 90")) -> soil

# AGGREGATE 20-30 and 30-40 depths into 20-40
# WOOSTER.LTR, HOYTVILLE.LTR, and NAEW.WS (109, 111, 113 and 118) sites have mismatching depths for soil BD and SOC
# BD measurement were done at 20-30 and 30-40 cm depth intervals, while SOC is at 20-40cm
soil %>% 
  filter(depth %in% c("20 to 30", "30 to 40")) %>%        # spread(key=depth, value = value)
  group_by(site, plotid, varname, year, subsample) %>% 
  summarise_at(vars(value), mean, na.rm = T) %>% 
  mutate(depth = "20 to 40", updated = Sys.time(), sampledate = NA) -> tempo

bind_rows(soil, tempo[ , names(soil)]) %>%
  filter(!depth %in% c("20 to 30", "30 to 40")) %>% 
  arrange(site, plotid, varname, year, depth, subsample) -> soil




# CLEANING of the DATA  ================================================

# get rid off  CC plots in SERF
soil[-which(soil$site == "SERF" & grepl("C", soil$plotid)), ] -> soil


# remove 2015 data in ARL
soil[-which(soil$site == "ARL" & soil$year == "2015"), ] -> soil


# remove 2011 TN data in BRADFORD.C 
soil[-which(soil$site == "BRADFORD.C" & soil$varname == "SOIL14" & soil$year == "2011"), ] -> soil

# replace SOC value of 8.6 with 0.86 in FREEMAN in 2011                         <<<<<<<<<<<<< REMOVE WHEN DATA UPDATED
soil$value[which(soil$site == "FREEMAN" & soil$value > 8)] <- 0.86

# remove 2015 TN in FREEMAN
soil[-which(soil$site == "FREEMAN" & soil$varname == "SOIL14" & soil$year == "2015"), ] -> soil

# remove 2015 SOC at 0-10 cm in HOYTVILLE.LTR
soil$value[which(soil$site == "HOYTVILLE.LTR" & soil$varname == "SOIL13" & soil$year == "2015" & 
             soil$depth == "0 to 10")] <- NA

# remove 2011 TN at 0-10 and 10-20 cm in KELLOGG
soil$value[which(soil$site == "KELLOGG" & soil$varname == "SOIL14" & soil$year == "2011" & 
                   soil$depth %in% c("0 to 10", "10 to 20"))] <- NA

# remove 2011 TN at 0-10 and 10-20 cm in MASON
soil$value[which(soil$site == "MASON" & soil$varname == "SOIL14" & soil$year == "2011" & 
                   soil$depth %in% c("0 to 10", "10 to 20"))] <- NA

# replace BD value of 2.74 with 1.74 in NAEW.WS118 in 2015                         <<<<<<<<<<<<< REMOVE WHEN DATA UPDATED
soil$value[which(soil$site == "NAEW.WS118" & soil$value > 2)] <- 1.74

# remove 2011 SOC at 40-60 cm in STJOHNS
soil$value[which(soil$site == "STJOHNS" & soil$varname == "SOIL13" & soil$year == "2011" & 
                   soil$depth == "40 to 60")] <- NA




# SELECT PLOTS -------------------------------------------------------------
# select "soil" data with the sites and plots of interest
# average over replicates (calculate mean of subsamples)
soil %>% 
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) %>% 
  spread(key = varname, value = value) -> soil_all







# GET SOC data from 'soil_all' ------------------------------------------
# find sites with no or only 1 year SOC data
soil_all %>%
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%     # see if there is at least one measurement that year at any depth
  group_by(site, plotid) %>%
  summarise(BD_count = sum(!is.na(SOIL1)), 
            SOC_count = sum(!is.na(SOIL13)), 
            TN_count = sum(!is.na(SOIL14))) %>%     # count how many years of measurement are available 
  filter(SOC_count < 2) -> no_SOC_data_sites       # filter plots with 1 or no year measurement of SOC
  

# gives the list of sites that have plot(s) with not enough soil data (and number of plots with insufficient data)
no_SOC_data_sites %>% group_by(site) %>% summarise(n())
no_SOC_data_sites$id <- paste(no_SOC_data_sites$site, no_SOC_data_sites$plotid, sep = "_")

# list number of plots by crop rotation groups and sites after removing plots with no SOC data
plots[!plots$id %in% no_SOC_data_sites$id, ] %>% 
  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> plots_with_SOC_summary



# get renewed soil plots and values by removing plots with no SOC data from "soil"
soil %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% no_SOC_data_sites$id) %>%              #additional filter to remove plots with no or 1 year soil data
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) %>% 
  spread(key= varname, value = value) -> soil_SOC







# GET TN data from 'soil_all' ------------------------------------------
# find sites with no or only 1 year TN  data
soil_all %>%
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%     # see if there is at least one measurement that year at any depth
  group_by(site, plotid) %>%
  summarise(BD_count = sum(!is.na(SOIL1)), 
            SOC_count = sum(!is.na(SOIL13)), 
            TN_count = sum(!is.na(SOIL14))) %>%     # count how many years of measurement are available 
  filter(TN_count < 2) -> no_TN_data_sites       # filter plots with 1 or no year measurement of TN


# gives the list of sites that have plot(s) with not enough soil data (and number of plots with insufficient data)
no_TN_data_sites %>% group_by(site) %>% summarise(n())
no_TN_data_sites$id <- paste(no_TN_data_sites$site, no_TN_data_sites$plotid, sep = "_")

# list number of plots by crop rotation groups and sites after removing plots with no TN data
plots[!plots$id %in% no_TN_data_sites$id, ] %>% 
  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> plots_with_TN_summary



# get renewed soil plots and values by removing plots with no TN data from "soil"
soil %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% no_TN_data_sites$id) %>%              #additional filter to remove plots with no or 1 year soil data
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) %>% 
  spread(key= varname, value = value) -> soil_TN







# PLOTTING for CHECKING ------------------------------------------------

# name of site to be plotted
sitename <- "ORR"

# SOC plotting .........................................................
soil_SOC %>% 
  filter(site == sitename) %>% 
  ggplot(aes(x=depth, y=SOIL13)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle(paste0("Soil Organic Carbon in ", sitename, "\n(plot averages)")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# BD plotting .........................................................
soil_SOC %>% 
  filter(site == sitename) %>% 
  ggplot(aes(x=depth, y=SOIL1)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "Bulk Density (g/cm3)") +
  ggtitle(paste0("Soil Bulk Density in ", sitename, "\n(plot averages)")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# TN plotting .........................................................
soil_TN %>% 
  filter(site == sitename) %>% 
  ggplot(aes(x=depth, y=SOIL14)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "TN (%)") +
  ggtitle(paste0("Soil Total Nitrogen in ", sitename, "\n(plot averages)")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))






# average BD calculation --------------------------------------------------------------

# calculate average BD per site per depth
soil_SOC %>%
  group_by(site, depth) %>% 
  mutate(SOIL1 = mean(SOIL1, na.rm = TRUE)) -> soil_SOC

# check if there is any missing BD values
soil_SOC %>% filter(is.na(SOIL1))
# get list of sites+plots that are missing data
soil_SOC %>% filter(is.na(SOIL1)) %>% ungroup() %>% select(site, plotid) %>% unique()

# there was no soil sample collected at 40-60 depth at any given year in NAEW
# - assign 20-40 cm BD data to the 40-60 cm
soil_SOC[grepl("NAEW", soil_SOC$site) & soil_SOC$depth == "40 to 60", ]$SOIL1 <- 
  soil_SOC[grepl("NAEW", soil_SOC$site) & soil_SOC$depth == "20 to 40", ]$SOIL1



# SOC ----------------------------------------------------------------------------  DO for TN
# SUBSTITUTING MISSING SOC DATA --------------------------------------------------



# FIND years with no SOIL13 data for each plot 
soil_SOC %>%           
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%
  ungroup() %>%
  mutate(id = paste(site, plotid, year, sep = "_")) %>%
  filter(is.na(SOIL13)) -> soil_no_data_years

# remove plot-years with no SOIL13 data from "soil_SOC" 
soil_SOC %>% 
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) -> soil_SOC


## FIRST YEAR of SITES/PLOTS --------------------------
# find the first and the last soil years 
soil_SOC %>% 
  group_by(site) %>% 
  summarise('First Year' = min(year), 
            'Last Year' = max(year),
            'delta(SOC)' = (max(year) - min(year))) -> first_last_year

soil_SOC %>% 
  group_by(site, plotid) %>% 
  summarise('First_Year' = min(year), 
            'Last_Year' = max(year),
            'delta(SOC)' = (max(year) - min(year))) -> first_last_year_new

# find sites that have plots with different first years for different plots
first_last_year_new %>% 
  group_by(site, First_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1) -> tempo
# list of plots with the First Year different than the rest of the Site plots
first_last_year_new %>% 
  filter(site %in% tempo$site, First_Year != 2011)


# find sites that have plots with different last years for different plots
first_last_year_new %>% 
  group_by(site, Last_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1) -> tempo
# list of plots with the First Year different than the rest of the Site plots
first_last_year_new %>% 
  filter(site %in% tempo$site, Last_Year != 2015)




# creat ids for "first_last_year_new" and stor as "fly_first" and "fly_last"
first_last_year_new %>% 
  mutate(id  = paste(site, First_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>% 
  mutate(id5 = paste(site, plotid, First_Year, sep = "_")) -> fly_first
first_last_year_new %>% 
  mutate(id  = paste(site, Last_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, Last_Year, sep = "_")) -> fly_last







# set "id"to be used for sorting of "soil_all_new_data"
soil_SOC %>% 
  mutate(id = paste(site, year, sep = "_")) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id3 = paste(site, plotid, year, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> soil_SOC


# find rows with no SOC data in the "First Year"
soil_SOC %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL13)) -> first_missing

# find values corresponding to the "first_missing" data in non-first-year
soil_SOC %>% 
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  select(id, id2, id3, SOIL13) -> first_missing_subs_13

# replace missing SOIL13 values in 2011 with substitutes from 2013
soil_SOC$SOIL13[soil_SOC$id3 %in% (first_missing %>% filter(id2 %in% first_missing_subs_13$id2))$id3] <- 
  first_missing_subs_13$SOIL13[first_missing_subs_13$id2 %in% first_missing$id2]


# there is one more value that need to be substituded in 2011 data from 2015
setdiff(first_missing$id2, first_missing_subs_13$id2)

soil_SOC[soil_SOC$site == "ORR" & 
           soil_SOC$plotid == "1102" & 
           soil_SOC$depth == "10 to 20" & 
           soil_SOC$year == 2012, "SOIL13"] <-
  soil_SOC[soil_SOC$site == "ORR" & 
             soil_SOC$plotid == "1102" & 
             soil_SOC$depth == "10 to 20" & 
             soil_SOC$year == 2015, "SOIL13"]



# find rows with no SOC data in the "Last Year"
soil_SOC %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL13)) -> last_missing

# find values corresponding to the "last_missing" data in non-last-year
soil_SOC %>%
  filter(!id5 %in% fly_last$id5) %>%   #choose all no-first-year data
  filter(id2 %in% last_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2011) %>%    # 2011 was choosen since there is no data in 2013
  select(id, id2, id3, SOIL13) -> last_missing_subs_13

# replace missing SOIL13 values in 2015 with substitutes from 2011
soil_SOC$SOIL13[soil_SOC$id3 %in% (last_missing %>% filter(id2 %in% last_missing_subs_13$id2))$id3] <- 
  last_missing_subs_13$SOIL13[last_missing_subs_13$id2 %in% last_missing$id2]


# there is one more value that need to be substituded in 2011 data from 2015
setdiff(last_missing$id2, last_missing_subs_13$id2)

soil_SOC[soil_SOC$site == "ORR" & 
           soil_SOC$plotid == "3012" & 
           soil_SOC$depth == "40 to 60" & 
           soil_SOC$year == 2015, "SOIL13"] <-
  soil_SOC[soil_SOC$site == "ORR" & 
             soil_SOC$plotid == "3012" & 
             soil_SOC$depth == "40 to 60" & 
             soil_SOC$year == 2012, "SOIL13"]

soil_SOC[soil_SOC$site == "ORR" & 
           soil_SOC$plotid == "3102" & 
           soil_SOC$depth == "40 to 60" & 
           soil_SOC$year == 2015, "SOIL13"] <-
  soil_SOC[soil_SOC$site == "ORR" & 
             soil_SOC$plotid == "3102" & 
             soil_SOC$depth == "40 to 60" & 
             soil_SOC$year == 2012, "SOIL13"]







# find missing BD

# find rows with no BD data in the "First Year"
# iteration 1
soil_SOC %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD

# find rows with no BD data in the "Last Year"
# iteration 1
soil_SOC %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL1)) -> last_missing_BD

# the END of the missing value substitutions








# C STOCK =================================================================================

# filter all data so only first and last year values remain
first_last_year_new %>% 
  mutate(`delta(SOC)` = NULL) %>%
  ungroup() %>%
  gather(key = FLY, value = year, First_Year:Last_Year) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> tempo

# calculate net Carbon Stoack in the soil profile 0-60 cm
soil_SOC %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL1, SOIL13) %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>% 
  # convert BD from g/cm3 to kg/ha by multiplying it with depth and 100,000
  # multiply above value by SOC (decimals) to get CS (carbon stocks) in kg C per ha (kg C/ha)
  mutate(CS = SOIL1*(bot_depth-top_depth)*100000*SOIL13/100) %>%
  group_by(site, plotid, year) %>%
  summarize(CS = sum(CS)) %>%
  mutate(id = paste(site, plotid, sep = "_")) -> CS

# calculate net Carbon Stoack in the top (0-20cm) and bottom (20-60cm) layers
soil_SOC %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL1, SOIL13) %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(CS = SOIL1*(bot_depth-top_depth)*100000*SOIL13/100) %>% 
  mutate(layer = ifelse(bot_depth < 30, "top", "bottom" )) %>%
  group_by(site, plotid, year, layer) %>%
  summarize(CS = sum(CS)) %>%
  mutate(id = paste(site, plotid, sep = "_")) -> CS2


merge(CS, plots[- which(names(plots) %in% c("plotid"))], by = "id", all.x = TRUE) %>%
  select(site, plotid, year, CS, crot, crot_name, rotation, tillage, drainage, nitrogen) -> CS
merge(CS2, plots[- which(names(plots) %in% c("plotid"))], by = "id", all.x = TRUE) %>%  
  select(site, plotid, year, CS, layer, crot, crot_name, rotation, tillage, drainage, nitrogen) -> CS2


# calculate average plot site for each site
site_metadata$`Plot Size (ha)` %>% 
  as.numeric() %>%
  is.na() %>%
  site_metadata[., c("UniqueID", "Number of Plots/ Subplots", "Site Area (ha)", "Plot Size (ha)")] 
site_metadata$`Ave Plot Size (ha)` <- as.numeric(site_metadata$`Plot Size (ha)`)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "HICKS.B"] <- round(40.8 / 6, 1)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "HICKS.G"] <- round(20.2 / 12, 1)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "SERF"] <- round(17 / 24, 1)


merge(CS, site_metadata[which(names(site_metadata) %in% c("UniqueID", 
                                                          "Latitude", 
                                                          "Longitude", 
                                                          "County", 
                                                          "State", 
                                                          "Ave Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> CS


merge(CS2, site_metadata[which(names(site_metadata) %in% c("UniqueID", 
                                                           "Latitude", 
                                                           "Longitude", 
                                                           "County", 
                                                           "State", 
                                                           "Ave Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> CS2











# PLOT TIME ========================================================================================
new_dir <- paste0(getwd(), "/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Soil_Figs/", Sys.Date())
dir.create(new_dir)
setwd(new_dir)
shell.exec(new_dir)    #opens new directory as window


CS %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> CS_summary
# to print 
CS_summary[is.na(CS_summary)] <- "-"
CS_summary

# aggregated CS by sites --------------------
CS %>%
  group_by(site) %>%
  select(site, CS, Latitude, Longitude, State, County, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) -> CS_site_plot

CS2 %>%
  group_by(site, layer) %>%
  select(site, CS, layer, Latitude, Longitude, State, County, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude)]))) %>%
  mutate(State = as.factor(State))  -> CS2_site_plot



# PLOT 1
CS_site_plot %>%
  ggplot(aes(x=site, y=CS)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT1

#1.a
PLOT1 + ggtitle("Average CS by Site")
ggsave(filename = "plot01.png", width = 12, dpi = 300)

#1.b
PLOT1 + 
  facet_grid(~ State) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot01b.png", width = 12, dpi = 300)

#1.c
CS_site_plot %>%
  ggplot(aes(x=site, y=CS, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light()
ggsave(filename = "plot01c.png", width = 12, dpi = 300)


# aggregated CS by sites and first-last year (fly) -------------------
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(site) %>%
  select(site, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) -> CS_site_fly_plot

# PLOT 2
CS_site_fly_plot %>%
  ggplot(aes(x=site, y=CS_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-100000, 100000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT2

#2.a
PLOT2 + ggtitle("Average CS Difference between the First and the Last Data Years by Site")
ggsave(filename = "plot02.png", width = 12, dpi = 300)

#2.b
PLOT2 + 
  facet_grid(~ State) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot02b.png", width = 12, dpi = 300)

#2.c
CS_site_fly_plot %>%
  ggplot(aes(x=site, y=CS_diff, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-100000, 100000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light()
ggsave(filename = "plot02c.png", width = 12, dpi = 300)






# aggregated CS by State --------------------
CS %>%
  group_by(State) %>%
  select(CS, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) -> CS_state_plot

# PLOT 3
CS_state_plot %>%
  ggplot(aes(x=State, y=CS)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT3

#3.a
PLOT3 + ggtitle("Average CS by State")
ggsave(filename = "plot03.png", width = 12, dpi = 300)


# aggregated CS by State and first-last year (fly) -------------------
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(State) %>%
  select(State, CS_diff, first, last, Latitude, Longitude) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) -> CS_state_fly_plot

# PLOT 4
CS_state_fly_plot %>%
  ggplot(aes(x=State, y=CS_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-50000, 50000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT4

#4.a
PLOT4 + ggtitle("Average CS Difference between the First and the Last Data Years by State")
ggsave(filename = "plot04.png", width = 12, dpi = 300)



# aggregated CS by Rotation --------------------
CS %>%
  group_by(crot) %>%
  select(crot, crot_name, CS) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) -> CS_crot_plot

# PLOT 5
CS_crot_plot %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS, fill=I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotation ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 150000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  theme_bw() -> PLOT5

#5.a
PLOT5 + 
  ggtitle("Average CS by Crop Rotation") + 
  geom_text(aes(label=str_wrap(crot_name, width=8)), vjust=-0.5, size = 4)
ggsave(filename = "plot05.png", width = 12, dpi = 300)


# aggregated CS by Rotation and first-last year (fly) -------------------
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(crot) %>%
  select(crot, crot_name, CS_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) -> CS_crot_fly_plot

# PLOT 6
CS_crot_fly_plot %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS_diff, fill = I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotaion ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-15000, 15000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT6

#6.a
PLOT6 + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation") +
  geom_text(aes(label=str_wrap(crot_name, width = 10)), hjust = "outward", size = 4)
ggsave(filename = "plot06.png", width = 12, dpi = 300)





# aggregated CS by Rotation and others --------------------
CS %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  group_by(crot, tillage) %>%
  select(crot, crot_name, tillage, CS) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage))  -> CS_crot_plus_plot

# PLOT 7
CS_crot_plus_plot %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS, fill=I(color))) + 
  geom_bar(stat = "identity") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(0, 200000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  theme_bw() -> PLOT7

#7.a
PLOT7 + 
  ggtitle("Average CS by Crop Rotation grouped by Drainage and Tillage") + 
  geom_text(aes(label=str_wrap(crot_name, width=12)), vjust=-0.5, size = 2.5)
ggsave(filename = "plot07.png", width = 12, dpi = 300)


# aggregated CS by Rotation and other in first-last year (fly) -------------------
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  group_by(crot, tillage) %>%
  select(crot, crot_name, tillage, CS_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) -> CS_crot_plus_fly_plot


# PLOT 8
CS_crot_plus_fly_plot %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS_diff, fill=I(color))) + 
  geom_bar(stat = "identity") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotaion ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-40000, 28000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT8

#8.a
PLOT8 + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation grouped by Drainage and Tillage") +
  geom_text(aes(label=str_wrap(crot_name, width = 10)), hjust = "outward", size = 2.5)
ggsave(filename = "plot08.png", width = 12, dpi = 300)


#





# # PLOT 1 with 2 layers
CS2_site_plot %>%
  ggplot(aes(x=site, y=CS)) + 
  geom_bar(aes(fill = layer), stat = "identity", position = "dodge") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 150000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT1a

#1.a
PLOT1a + ggtitle("Average CS by Site")
ggsave(filename = "plot01a.png", width = 12, dpi = 300)

#1.b
PLOT1a + 
  facet_grid(~ State) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot01ab.png", width = 12, dpi = 300)


# aggregated CS by sites and first-last year (fly) -------------------
CS2 %>%
  group_by(site, plotid, layer) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, layer, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(site, layer) %>%
  select(site, layer, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>% 
  mutate(layer = factor(layer)) %>% 
  mutate(site = factor(site, levels = unique(site[order(Latitude)]))) %>%
  mutate(State = as.factor(State)) -> CS2_site_fly_plot



# PLOT 2
CS2_site_fly_plot %>%
  ggplot(aes(x=site, y=CS_diff, fill = layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "Change in Carbon Stock (kg C/ha)",
                     #limits = c(-80000, 80000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT2a

#2.a
PLOT2a + 
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle("Average CS Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "plot02a.png", width = 12, dpi = 300)

#2.b
PLOT2a + 
  facet_grid(~ State) + 
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot02ab.png", width = 12, dpi = 300)


# aggregated CS by State --------------------
CS2 %>%
  group_by(State, layer) %>%
  select(CS, layer, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude)]))) -> CS2_state_plot

# PLOT 3
CS2_state_plot %>%
  ggplot(aes(x=State, y=CS, fill = layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 150000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT3a

#3.a
PLOT3a + 
  ggtitle("Average CS by State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
  
ggsave(filename = "plot03a.png", width = 12, dpi = 300)


# aggregated CS2 by State and first-last year (fly) -------------------
CS2 %>%
  group_by(site, plotid, layer) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, layer, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(State, layer) %>%
  select(State, layer, CS_diff, first, last, Latitude, Longitude) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude)]))) -> CS2_state_fly_plot

# PLOT 4
CS2_state_fly_plot %>%
  ggplot(aes(x=State, y=CS_diff, fill = layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-30000, 30000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT4a

#4.a
PLOT4a + 
  ggtitle("Average CS Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot04a.png", width = 12, dpi = 300)



# aggregated CS by Rotation --------------------
CS2 %>%
  group_by(crot, layer) %>%
  select(crot, layer, crot_name, CS) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) -> CS2_crot_plot

# PLOT 5
CS2_crot_plot %>%
  ggplot(aes(x=crot_name, y=CS, fill=layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10), 
                   name = NULL) +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 70000),
                     labels = comma) + 
  theme_bw() -> PLOT5a

#5.a
PLOT5a + 
  ggtitle("Average CS by Crop Rotation") + 
  theme(#axis.text.x = element_text(angle = 90, size = 10),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot05a.png", width = 12, dpi = 300)


# aggregated CS by Rotation and first-last year (fly) -------------------
CS2 %>%
  group_by(site, plotid, layer) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, layer, crot, crot_name, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(crot, layer) %>%
  select(crot, layer, crot_name, CS_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) -> CS2_crot_fly_plot

# PLOT 6
CS2_crot_fly_plot %>%
  ggplot(aes(x=crot_name, y=CS_diff, fill = layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = NULL, 
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-13000, 13000),
                     labels = comma) + 
  coord_flip() +
  theme_light() -> PLOT6a

#6.a
PLOT6a + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot06a.png", width = 12, dpi = 300)





# aggregated CS by Rotation and others --------------------
CS2 %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  group_by(crot, tillage, layer) %>%
  select(crot, crot_name, tillage, CS, layer) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) -> CS2_crot_plus_plot

# PLOT 7
CS2_crot_plus_plot %>%
  ggplot(aes(x=crot_name, y=CS, fill=layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = NULL,
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(0, 125000),
                     labels = comma) +
  theme_bw() -> PLOT7a

#7.a
PLOT7a + 
  ggtitle("Average CS by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90))
ggsave(filename = "plot07a.png", width = 12, dpi = 300)


# aggregated CS by Rotation and other in first-last year (fly) -------------------
CS2 %>%
  group_by(site, plotid, layer) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, layer, plotid, crot, crot_name, tillage, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  group_by(crot, tillage, layer) %>%
  select(crot, crot_name, tillage, CS_diff, first, last, layer) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) -> CS2_crot_plus_fly_plot


# PLOT 8
CS2_crot_plus_fly_plot %>%
  ggplot(aes(x=crot_name, y=CS_diff, fill=layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = NULL, 
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "CS (kg C/ha)",
                     #limits = c(-24000, 14000),
                     labels = comma) + 
  coord_flip() +
  theme_light() -> PLOT8a

#8.a
PLOT8a + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot08a.png", width = 12, dpi = 300)



#



