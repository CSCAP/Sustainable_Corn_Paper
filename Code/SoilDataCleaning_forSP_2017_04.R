# VERSION 5
# This code is modified for Lori's yield vs carbon stock analysis
# this code contains data-cleaning for SOC and TN.
# most of the cleaning is done according to my (Gio) understanding of the data.
# few are done in order to incorproate suggestions given by PIs.
# for example: CC plots from SERF (8 plots) were removed from analysis based on Matt's request. 
# this code calculates average BD over whole site (instead of plot) by depths (like VERSION 2) and 
# tillage treatment (one set of BD for No Till and one for Conv. Till plots when both are present at site).
# Unlike pervious version, where BDL = (Ditection Limit / 2), 
# here we assume that BDL values are equal to the detection limit itself.
# this was done because of the erroneous C:N ratio values found in some cases (for example, at ORR)
# NEW code was added to incoprotate Eileen Kaldivkos suggestions, inlcuding:
# selecting data years with the same crop (phases), 
# grouping plots by crop when crop phases in not matching within the plot



library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(dplyr)



setwd("C:/Users/Gio/Documents")

load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/soil.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/plots.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/agro.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/crot.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/metadata.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/var_names.RData")



# FILTERING SOIL data -------------------------------------------------------

SOIL %>%
  # get rid off "< " sign in front of the BDL values and convert all values to numeric
  mutate(value = sub("<", "", value)) %>%
  mutate(value = as.double(value)) %>% 
  # select "soil" data with the variables of interest (BD, SOC, TN)
  filter(varname %in% c("SOIL1", "SOIL13", "SOIL14")) %>%
  # get rid of WATERMAN site
  filter(site != "WATERMAN") %>%
  # get rid of VICMS site
  filter(site != "VICMS") %>%
  # get rid of ONFARM sites
  filter(!grepl("ONFARM", site)) %>%
  # get rid of organic NAEW sites
  filter(!site %in% paste0("NAEW.WS", c(127, 123, 115))) %>%
  # remove 0-30, 30-60, and 60-90 cm intervals in STJOHNS used for soil pH and CEC samples
  filter(!depth %in% c("0 to 30", "30 to 60", "60 to 90")) %>%
  select(-c(updated, sampledate)) -> soil

#rm(SOIL)

# combine 20-30 and 30-40 depths into 20-40
# WOOSTER.LTR and HOYTVILLE.LTR sites have mismatching depths for soil BD and SOC
# BD measurement were done at 20-30 and 30-40 cm depth intervals, while SOC is at 20-40cm
soil %>% 
  filter(depth %in% c("20 to 30", "30 to 40")) %>% 
  group_by(site, plotid, varname, year, subsample) %>% 
  summarise_at(vars(value), mean, na.rm = T) %>% 
  mutate(depth = "20 to 40") -> tempo

bind_rows(soil, tempo[ , names(soil)]) %>%
  filter(!depth %in% c("20 to 30", "30 to 40")) %>% 
  arrange(site, plotid, varname, year, depth, subsample) -> soil




# CLEANING of the DATES  ================================================

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





# SELECT PLOTS -------------------------------------------------------------
# get rid of continuous soy and continuous soy with rye cover
plots %>% filter(!crot %in% c("CR02", "CR04")) -> plots              #878 plots > 850 plots

# select "soil" data with the sites and plots of interest
# average over replicates (calculate mean of subsamples)
soil %>% 
  mutate(id = paste(site, plotid, sep = "_")) %>%
  #filter(id %in% unique(plots$id)) %>% 
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) %>% 
  spread(key = varname, value = value) -> soil_all



# PLOTTING for CHECKING ------------------------------------------------

# name of site to be plotted
sitename <- "ISUAG.USB"

# SOC plotting .........................................................
soil_all %>% 
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
soil_all %>% 
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
soil_all %>% 
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
soil_all %>%
  mutate(id = paste(site, plotid, sep = "_")) %>% 
  group_by(site, depth) %>%
  mutate(BD = mean(SOIL1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(site, plotid, year, depth, BD, SOIL13) -> soil_SOC

# substityde missing BD data from 40 to 60 cm depth at NAEW sites with 40 to 60 cm BD data
soil_SOC %>% 
  filter(grepl("NAEW", site) & depth == "20 to 40") %>%
  select(site, BD) %>% unique() -> temp

soil_SOC %>%
  left_join(temp, by = "site") %>%
  mutate(BD = ifelse(is.na(BD.x), BD.y, BD.x)) %>%
  select(site, plotid, year, depth, BD, SOIL13) -> soil_SOC

# check if there is any missing BD values
soil_SOC %>% 
  filter(is.na(BD)) 


# Select Plots and Average over Reps and Crop Rotations  ------------------------------------
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/plots_complete.RData")
plot_mng %>%
  select(uniqueid, plotid, rotation, rep, tillage, drainage, nitrogen) %>%
  arrange(uniqueid, plotid, rotation, rep) -> plot_mng
plot_mng %>%
  filter(!nitrogen %in% c("NIT1", "NIT3", "NIT4")) %>%
  mutate(croprot = ifelse(rotation %in% c("ROT1", "ROT38", "ROT56"), "CC",
                          ifelse(rotation %in% paste0("ROT", c(6,7,8,15,16,17,62)), "CSW",
                                 ifelse(rotation %in% paste0("ROT", c(4,5,36,37,54,55,57,59,60,61)), "CS", 
                                        "other")))) %>%
  filter(croprot != "other") %>%
  rename(site = uniqueid) -> plots

# join plot IDs with SOC data and 
# get rid off plots with Crop Rotation and Nitrogen treatment that are not of research interest
soil_SOC  %>%
  left_join(plots, by = c("site", "plotid")) %>%
  filter(!is.na(croprot)) -> soil_SOC

# Fix deviations in field management
soil_SOC %>%
  mutate(cover = ifelse(rotation %in% paste0("ROT", c(15, 16, 17, 36, 37, 38, 55)), "Rye", "No Rye")) %>%
  mutate(tillage = ifelse(site == "SEPAC" & year == 2011, "TIL2", tillage),
         tillage = ifelse(site == "SEPAC" & year > 2011, "TIL1", tillage),
         tillage = ifelse(site == "STJOHNS" & year == 2011, "TIL1", tillage)) %>%
  select(-nitrogen) -> soil_SOC


# C STOCK =================================================================================
# calculate net Carbon Stoack in the top (0-20cm), middle (20-40cm) and bottom (40-60cm) layers
soil_SOC %>% 
  group_by(site, croprot, depth) %>%
  summarise(BD  = mean(BD, na.rm = TRUE),
            SOC = mean(SOIL13, na.rm = TRUE))%>%
  ungroup() %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(CS = BD*(bot_depth-top_depth)*100000*SOC/100) %>% 
  mutate(layer = ifelse(bot_depth < 30, "top", ifelse(bot_depth > 50, "bottom", "middle"))) %>%
  group_by(site, croprot, layer) %>%
  summarize(CS = sum(CS, na.rm = TRUE),
            SOC = mean(SOC, na.rm = TRUE),
            BD = mean(BD, na.rm = TRUE)) %>%
  mutate(layer = factor(layer, levels = c("top", "middle", "bottom"))) %>%
  arrange(site, croprot, layer) -> CS_croprot


save(CS_croprot, file = "~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/CS_croprot.R")
save(soil_SOC, file = "~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/soil_SOC.R")




