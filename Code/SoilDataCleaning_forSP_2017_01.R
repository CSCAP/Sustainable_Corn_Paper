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

# get rid of WATERMAN site
soil %>%
  filter(site != "WATERMAN") -> soil

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


# remove 2011 SOC at 40-60 cm in STJOHNS
soil$value[which(soil$site == "STJOHNS" & soil$varname == "SOIL13" & soil$year == "2011" & 
                   soil$depth == "40 to 60")] <- NA

# replace 2013 and 2015 SOC and TN at 40-60 cm for plot 113T in NAEW.WS113
soil[soil$site == "NAEW.WS113" & 
       soil$plotid == "113T" & 
       soil$depth == "40 to 60" & 
       soil$varname == "SOIL13" &
       soil$year %in% c(2013, 2015), "value"] <- soil[soil$site == "NAEW.WS113" & 
                                                        soil$plotid == "113T" & 
                                                        soil$depth == "40 to 60" & 
                                                        soil$varname == "SOIL13" &
                                                        soil$year == 2014, "value"]
soil[soil$site == "NAEW.WS113" & 
       soil$plotid == "113T" & 
       soil$depth == "40 to 60" & 
       soil$varname == "SOIL14" &
       soil$year %in% c(2013, 2015), "value"] <- soil[soil$site == "NAEW.WS113" & 
                                                        soil$plotid == "113T" & 
                                                        soil$depth == "40 to 60" & 
                                                        soil$varname == "SOIL14" &
                                                        soil$year == 2014, "value"]



# SELECT PLOTS -------------------------------------------------------------
# get rid of continuous soy and continuous soy with rye cover
plots %>% filter(!crot %in% c("CR02", "CR04")) -> plots

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

plots %>% 
  select(id, tillage) %>% 
  mutate(id = as.character(id)) %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) -> tempo

# calculate average BD per site per depth
soil_SOC %>%
  mutate(id = paste(site, plotid, sep = "_")) %>% 
  left_join(tempo, by = "id") %>%
  group_by(site, depth, tillage) %>%
  mutate(SOIL1 = mean(SOIL1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL1, SOIL13, SOIL14) -> soil_SOC

# check if there is any missing BD values
soil_SOC %>% filter(is.na(SOIL1))
# get list of sites+plots that are missing data
soil_SOC %>% filter(is.na(SOIL1)) %>% ungroup() %>% select(site, plotid) %>% unique()

# there was no soil sample collected at 40-60 depth at any given year in NAEW
# - assign 20-40 cm BD data to the 40-60 cm
soil_SOC[grepl("NAEW", soil_SOC$site) & soil_SOC$depth == "40 to 60", ]$SOIL1 <- 
  soil_SOC[grepl("NAEW", soil_SOC$site) & soil_SOC$depth == "20 to 40", ]$SOIL1



# SOC ----------------------------------------------------------------------------
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
# list of plots with the Last Year different than the rest of the Site plots
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







# set "id"to be used for sorting of "soil_SOC"
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


# check if there is any missing value with no substitution found in 2013
setdiff(first_missing$id2, first_missing_subs_13$id2)



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


# check if there is any missing value with no substitution found in 2013
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

# calculate net Carbon Stoack in the top (0-20cm), middle (20-40cm) and bottom (40-60cm) layers
soil_SOC %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL1, SOIL13) %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(CS = SOIL1*(bot_depth-top_depth)*100000*SOIL13/100) %>% 
  mutate(layer = ifelse(bot_depth < 30, "top", ifelse(bot_depth > 50, "bottom", "middle"))) %>%
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








# TN ----------------------------------------------------------------------------  
# SUBSTITUTING MISSING TN DATA --------------------------------------------------



# FIND years with no SOIL14 data for each plot 
soil_TN %>%           
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%
  ungroup() %>%
  mutate(id = paste(site, plotid, year, sep = "_")) %>%
  filter(is.na(SOIL14)) -> soil_no_data_years

# remove plot-years with no SOIL14 data from "soil_TN" 
soil_TN %>% 
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) -> soil_TN


## FIRST YEAR of SITES/PLOTS --------------------------
# find the first and the last soil years 
soil_TN %>% 
  group_by(site) %>% 
  summarise('First Year' = min(year), 
            'Last Year' = max(year),
            'delta(TN)' = (max(year) - min(year))) -> first_last_year

soil_TN %>% 
  group_by(site, plotid) %>% 
  summarise('First_Year' = min(year), 
            'Last_Year' = max(year),
            'delta(TN)' = (max(year) - min(year))) -> first_last_year_new

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
# list of plots with the Last Year different than the rest of the Site plots
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







# set "id"to be used for sorting of "soil_TN"
soil_TN %>% 
  mutate(id = paste(site, year, sep = "_")) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id3 = paste(site, plotid, year, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> soil_TN


# find rows with no TN data in the "First Year"
soil_TN %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL14)) -> first_missing

# find values corresponding to the "first_missing" data in non-first-year
soil_TN %>% 
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  select(id, id2, id3, SOIL14) -> first_missing_subs_14

# replace missing SOIL14 values in 2011 with substitutes from 2013
soil_TN$SOIL14[soil_TN$id3 %in% (first_missing %>% filter(id2 %in% first_missing_subs_14$id2))$id3] <- 
  first_missing_subs_14$SOIL14[first_missing_subs_14$id2 %in% first_missing$id2]


# check if there is any missing value with no substitution found in 2013
setdiff(first_missing$id2, first_missing_subs_14$id2)



# find rows with no TN data in the "Last Year"
soil_TN %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL14)) -> last_missing

# find values corresponding to the "last_missing" data in non-last-year
soil_TN %>%
  filter(!id5 %in% fly_last$id5) %>%   #choose all no-first-year data
  filter(id2 %in% last_missing$id2) %>% 
  ungroup() %>%
  filter(year < 2013) %>%    # 2011 was choosen since there is no data in 2013
  select(id, id2, id3, SOIL14) -> last_missing_subs_14

# replace missing SOIL14 values in 2015 with substitutes from 2011
soil_TN$SOIL14[soil_TN$id3 %in% (last_missing %>% filter(id2 %in% last_missing_subs_14$id2))$id3] <- 
  last_missing_subs_14$SOIL14[last_missing_subs_14$id2 %in% last_missing$id2]


# check if there is any missing value with no substitution found in 2013
setdiff(last_missing$id2, last_missing_subs_14$id2)



# filter all data so only first and last year values remain
first_last_year_new %>% 
  mutate(`delta(TN)` = NULL) %>%
  ungroup() %>%
  gather(key = FLY, value = year, First_Year:Last_Year) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> tempo

# calculate average Total Nitrogen in the soil profile 0-60 cm
soil_TN %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL14) %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(value = SOIL14 * (bot_depth - top_depth) / 60) %>%
  group_by(site, plotid, year) %>%
  summarize(TN = sum(value)) %>%
  mutate(id = paste(site, plotid, sep = "_")) -> TN

# calculate average Total Nitrogen in the top (0-20cm), middle(20-40cm) and bottom (20-60cm) layers
soil_TN %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL14) %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(layer = ifelse(bot_depth < 30, "top", ifelse(bot_depth > 50, "bottom", "middle"))) %>%
  group_by(site, plotid, year, layer) %>%
  summarize(TN = mean(SOIL14)) %>%
  mutate(id = paste(site, plotid, sep = "_")) -> TN2


merge(TN, plots[- which(names(plots) %in% c("plotid"))], by = "id", all.x = TRUE) %>%
  select(site, plotid, year, TN, crot, crot_name, rotation, tillage, drainage, nitrogen) -> TN
merge(TN2, plots[- which(names(plots) %in% c("plotid"))], by = "id", all.x = TRUE) %>%  
  select(site, plotid, year, TN, layer, crot, crot_name, rotation, tillage, drainage, nitrogen) -> TN2


merge(TN, site_metadata[which(names(site_metadata) %in% c("UniqueID", 
                                                          "Latitude", 
                                                          "Longitude", 
                                                          "County", 
                                                          "State", 
                                                          "Ave Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> TN


merge(TN2, site_metadata[which(names(site_metadata) %in% c("UniqueID", 
                                                           "Latitude", 
                                                           "Longitude", 
                                                           "County", 
                                                           "State", 
                                                           "Ave Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> TN2







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
# Plots and saves CS figures in the working directory
source("C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Code/CS_plotting_2017_01.R")

TN %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> TN_summary
# to print 
TN_summary[is.na(TN_summary)] <- "-"
TN_summary
# Plots and saves TN figures in the working directory
source("C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Code/TN_plotting_2017_01.R")

















# PLOT CS by LAYERS -----------------------------------------------------------------------
# PLOT 1 -------------------- CS average by site
CS2 %>%
  select(site, CS, Latitude, Longitude, State, County, `Ave Plot Size (ha)`, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = CS, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_wrap( ~ layer) +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot01.png", width = 12, height = 8, dpi = 300)


# PLOT 2 -------------------- CS difference by site
CS2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(site, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`, layer) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = CS_diff, fill = State)) +
  facet_wrap( ~ layer) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot02.png", width = 12, height = 8, dpi = 300)

CS2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly, layer) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(site, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`, layer) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = CS_diff, fill = layer)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot02D.png", width = 12, height = 8, dpi = 300)




# PLOT 3 -------------------- CS average by State
CS2 %>%
  select(CS, Latitude, Longitude, State, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = CS)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_wrap( ~ layer) +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot03.png", width = 12, height = 8, dpi = 300)



# PLOT 4 -------------------- CS difference by State
CS2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(State, CS_diff, first, last, Latitude, Longitude, layer) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = CS_diff)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  facet_grid( ~ layer) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot04.png", width = 12, height = 8, dpi = 300)



# PLOT5 --------------------- CS by rotation
CS2 %>%
  select(crot, crot_name, CS, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = CS, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ layer) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot05.png", width = 12, height = 8, dpi = 300)


 
# PLOT6 --------------------- CS difference by rotation
CS2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, CS, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(crot, crot_name, CS_diff, first, last, layer) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = CS_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ layer) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years\nby Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot06.png", width = 12, height = 8, dpi = 300)



# PLOT7 --------------------- CS by rotation and Tillage
CS2 %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, CS, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = CS, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid(layer ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average CS by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12))
ggsave(filename = "3Lplot07.png", width = 12, height = 8, dpi = 300)



# PLOT8 --------------------- CS difference by rotation and tillage
CS2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, CS, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, CS_diff, first, last, layer) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = CS_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid(layer ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years\nby Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3Lplot08.png", width = 10, height = 12, dpi = 300)











# PLOT TN by LAYERS -----------------------------------------------------------------------
# PLOT 1 -------------------- TN average by site
TN2 %>%
  select(site, TN, Latitude, Longitude, State, County, `Ave Plot Size (ha)`, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = TN, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_wrap( ~ layer) +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot01.png", width = 12, height = 8, dpi = 300)


# PLOT 2 -------------------- TN difference by site
TN2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, TN, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(site, TN_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`, layer) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = TN_diff, fill = State)) +
  facet_wrap( ~ layer) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot02.png", width = 12, height = 8, dpi = 300)




# PLOT 3 -------------------- TN average by State
TN2 %>%
  select(TN, Latitude, Longitude, State, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = TN)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_wrap( ~ layer) +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot03.png", width = 12, height = 8, dpi = 300)



# PLOT 4 -------------------- TN difference by State
TN2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, TN, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(State, TN_diff, first, last, Latitude, Longitude, layer) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = TN_diff)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  facet_grid( ~ layer) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot04.png", width = 12, height = 8, dpi = 300)



# PLOT5 --------------------- TN by rotation
TN2 %>%
  select(crot, crot_name, TN, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ layer) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot05.png", width = 12, height = 8, dpi = 300)



# PLOT6 --------------------- TN difference by rotation
TN2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, TN, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(crot, crot_name, TN_diff, first, last, layer) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ layer) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot06.png", width = 12, height = 8, dpi = 300)



# PLOT7 --------------------- TN by rotation and Tillage
TN2 %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, TN, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid(layer ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12))
ggsave(filename = "3L_TN_plot07.png", width = 12, height = 8, dpi = 300)



# PLOT8 --------------------- TN difference by rotation and tillage
TN2 %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, TN, fly, layer) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, TN_diff, first, last, layer) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid(layer ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "3L_TN_plot08.png", width = 10, height = 12, dpi = 300)


















# an OLD plot

# PLOT 8
CS2_crot_plus_fly_plot %>%
  ggplot(aes(x=crot_name, y=CS_diff, fill=layer)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "CS (kg C/ha)", limits = c(-24000, 14000), labels = comma) + 
  coord_flip() +
  theme_light() + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot08a.png", width = 12, dpi = 300)





