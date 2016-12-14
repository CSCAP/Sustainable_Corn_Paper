# TN
# this code shows the change in TN between first and last soil data years  

setwd("C:/Users/Gio/Documents")

load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/plots.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/crot.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/metadata.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/var_names.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/big_soil_data.RData")

library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(dplyr)

# BELOW DETACTION LIMIT data ---------------------------------------- <<< NEED TO ADD TO THE MAIN CODE
# get row numbers of BDL values
BDL_rows <- grep("<", SOIL_data$value)
# get rid off "< " sign in front of the BDL values and convert all values to numeric
SOIL_data$value <- as.double(sub("< ", "", SOIL_data$value))     #substitude "<" with ""
# substitude BDL data with half of its values
SOIL_data$value[BDL_rows] <- SOIL_data$value[BDL_rows] / 2
# save as the df as soil data
soil <- SOIL_data
rm(BDL_rows, SOIL_data)

# Restore ARL data that was accidentally deleted from Google Sheets
# these values were also added back to the Google Sheet
# WHEN CSCAP DB IS UPDATED THIS CHUNK OF CODE WILL BE ABSOLETE
tempo <- data.frame(site = rep("ARL", 2), 
                    plotid = rep(113, 2), 
                    depth = rep("20 - 60", 2),
                    varname = c("SOIL13", "SOIL14"),
                    year = rep(2015, 2),
                    value = c(0.959591, 0.105528), 
                    subsample = rep(1, 2),
                    updated = rep("2016-12-08", 2),
                    sampledate = rep(NA, 2)) 
soil <- rbind(soil, tempo)
soil %>% arrange(site, plotid, varname, year, depth, subsample) -> soil


# select variable of interest (SOIL1 = BD, SOIL13 = SOC, SOIL14 = TN)
VAR <- "SOIL14"

# select "soil" data with the (a) variables, and (b) sites and plots of interest
# average over replicates 
soil %>% 
  filter(varname == VAR) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) -> soil_all


# find sites with no or only 1 year TN data
soil_all %>% 
  group_by(site, plotid, year) %>%
  summarise_at(vars(value), mean, na.rm = T) %>%      # see if there is at least one measurement that year at any depth
  ungroup() %>% 
  group_by(site, plotid) %>%
  summarise(count_years = sum(!is.na(value))) %>%     # count how many years of measurement are available 
  filter(count_years < 2) %>%                         # filter plots with 1 or no year measurement of TN
  mutate(id = paste(site, plotid, sep = "_")) -> soil_no_data_sites

# gives the list of sites that have plot(s) with not enough soil data (and number of plots with insufficient data)
soil_no_data_sites %>% group_by(site) %>% summarise(n())

# remove plots with no soil data from "plots"
plots_data <- plots[!plots$id %in% soil_no_data_sites$id, ]
## get rid off  CC plots with continous corn rotation (ROT1) in SERF
plots_data <- 
  plots_data[-which(plots_data$uniqueid == "SERF" & plots_data$rotation == "ROT1"), ]

# list number of plots by crop rotation groups and sites
plots_data %>% 
  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> plots_summary


# get renewed soil plots and values by removing plots with no soil data from "soil"
soil %>%
  filter(varname == VAR) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%              #additional filter to remove plots with no or 1 year soil data
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) -> soil_all
## get rid off  CC plots with continous corn rotation (ROT1) in SERF
soil_all %>% filter(!(site == "SERF" & grepl("C", plotid))) -> soil_all

# EXPLOR DATA 
setdiff(soil_all$site[soil_all$year == "2015"], soil_all$site[soil_all$year == "2011"])
soil_all$site[soil_all$year == "2012"] %>% unique()
# all 28 sites have data collected in 2015
# only 25 sites have first year soil data collected in 2011
# 2 sites (ORR, WOOSTER.COV) have the first soil data collected in 2012
# BRADFORD.A collected its first soil data in 2013
# END OF EXPLOR DATA

# find YEARS with no soil data for each plot
soil_all %>%    
  group_by(site, plotid, year) %>%
  summarise_at(vars(value), mean, na.rm = T) %>%
  ungroup() %>%
  mutate(id = paste(site, plotid, year, sep = "_")) %>%
  filter(is.na(value)) -> soil_no_data_years

# remove plot-years with no soil data from "soil_all" 
soil_all %>% 
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) -> soil_data

# in STJOHNS soil pH and CEC were measured at 0-30, 30-60, and 60-90 cm intervals
# while BD, SOC, and TN was measured at "regular" depths corresponding to the protocol 
# remove those artifactrs from the "soil_all_data"
if (VAR %in% c("SOIL13", "SOIL14")) {
  soil_data %>% filter(!depth %in% c("0 - 30", "30 - 60", "60 - 90")) -> soil_data
}



# FIRST YEAR of SITES/PLOTS --------------------------
# find the first and the last soil years 
soil_data %>%
  group_by(site) %>% 
  summarise('First Year' = min(year), 
            'Last Year' = max(year),
            'Diff Year' = (max(year) - min(year))) -> first_last_year_SITE

soil_data %>% 
  group_by(site, plotid) %>% 
  summarise('First_Year' = min(year), 
            'Last_Year' = max(year),
            'Diff Year' = (max(year) - min(year))) -> first_last_year_PLOT

# find sites that have plots with different first years for different plots
first_last_year_PLOT %>% 
  group_by(site, First_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1) -> tempo
# list of plots with the First Year different than the rest of the Site plots
first_last_year_PLOT %>% 
  filter(site %in% tempo$site, First_Year != 2011)


# find sites that have plots with different last years for different plots
first_last_year_PLOT %>% 
  group_by(site, Last_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1)


# creat ids for "first_last_year_PLOT" and stor as "fly_first" and "fly_last"
first_last_year_PLOT %>% 
  mutate(id  = paste(site, First_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>% 
  mutate(id5 = paste(site, plotid, First_Year, sep = "_")) -> fly_first
first_last_year_PLOT %>% 
  mutate(id  = paste(site, Last_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, Last_Year, sep = "_")) -> fly_last




# TN ---------------------------------------------------------------
# set "id"to be used for sorting of "soil_data"
soil_data %>%
  mutate(id = paste(site, year, sep = "_")) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id3 = paste(site, plotid, year, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> soil_data


# find rows with no soil data in the "First Year"
soil_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(value)) -> first_missing

# find values corresponding to the "first_missing" data in non-first-year
soil_data %>% 
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  select(id, id2, id3, value) -> first_missing_subs

# replace missing values in 2011 with substitutes from 2013
soil_data$value[soil_data$id3 %in% first_missing$id3] <- 
  first_missing_subs$value[first_missing_subs$id2 %in% first_missing$id2]


# find rows with no soil data in the "Last Year"
soil_data %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(value)) -> last_missing


# TN SUM =================================================================================

# calculate average plot site for each site to JOIN with DATA
site_metadata %>% 
  mutate(Ave_Plot_Size = as.numeric(`Plot Size (ha)`)) %>% 
  mutate(Ave_Plot_Size = ifelse(UniqueID == "HICKS.B", round(40.8 / 6, 1), Ave_Plot_Size)) %>%
  mutate(Ave_Plot_Size = ifelse(UniqueID == "HICKS.G", round(20.2 / 12, 1), Ave_Plot_Size)) %>%
  mutate(Ave_Plot_Size = ifelse(UniqueID == "SERF", round(17 / 24, 1), Ave_Plot_Size)) %>%
  select(UniqueID, Latitude, Longitude, State, Ave_Plot_Size) -> metadata

# filter all data so only first and last year values remain
first_last_year_PLOT %>%
  mutate(`Diff Year` = NULL) %>%
  ungroup() %>%
  gather(key = FLY, value = year, First_Year:Last_Year) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> tempo

# calculate total TN data
soil_data %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, value) %>%
  filter(!is.na(value)) %>%                                
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>%
  mutate(value = value) %>%                               
  mutate(layer = ifelse(bot_depth < 30, "top", ifelse(bot_depth > 50, "bottom", "middle"))) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>% 
  left_join(plots_data[- which(names(plots_data) %in% c("plotid"))], by = c("id" = "id")) %>%
  select(site, plotid, year, depth, layer, value, crot, crot_name, rotation, tillage, drainage, nitrogen) %>%
  left_join(metadata, by = c("site" = "UniqueID")) %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>%
  ungroup() -> DATA



# calculate total soil value in the soil profile 0-60 cm
DATA %>%
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>% 
  mutate(value = value*(bot_depth-top_depth)/60) %>%        
  group_by(site, plotid, year, fly,
           crot, crot_name, rotation, tillage, drainage, nitrogen,
           Latitude, Longitude, State, Ave_Plot_Size) %>%
  summarize_at(vars(value), sum) %>%
  ungroup() -> DATA_plots


# calculate total soil data in the top (0-20cm) and bottom (40-60cm) layers
DATA %>% 
  filter(layer != "middle") %>%
  group_by(site, plotid, year, layer, fly, 
           crot, crot_name, rotation, tillage, drainage, nitrogen,
           Latitude, Longitude, State, Ave_Plot_Size) %>%
  summarise_at(vars(value), mean, na.rm = TRUE) %>%
  ungroup() -> DATA_layers




# PLOT TIME ========================================================================================
new_dir <- paste0(getwd(), "/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Soil_Figs/", Sys.Date())
dir.create(new_dir)
setwd(new_dir)
shell.exec(new_dir)    #opens new directory as window

# number of plots per site and crop rotation 
DATA %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n_distinct(plotid)) %>%
  spread(key = crot, value = total.plots) -> PLOTS_summary
PLOTS_summary$Total_Plots <- rowSums(PLOTS_summary[2:9], na.rm = TRUE)
PLOTS_summary <- rbind(PLOTS_summary, c("TOTAL", colSums(PLOTS_summary[2:10], na.rm = TRUE)))
# to print 
PLOTS_summary[is.na(PLOTS_summary)] <- "-"
PLOTS_summary

#
xtitle <- "TN (%)" 

# aggregated TN by sites --------------------
DATA_plots %>%
  select(site, value, Latitude, Longitude, State, Ave_Plot_Size) %>%
  group_by(site) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = xtitle,
                     #limits = c(0, 0.2),
                     labels = comma) +
  coord_flip() +
  theme_light() -> PLOT1

# PLOT 01.1
PLOT1 + 
  ggtitle("Average TN by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_01.1.png", width = 10, height = 6, dpi = 300)

# PLOT 01.2
PLOT1 + 
  facet_grid(~ State) + 
  ggtitle("Average TN") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_01.2.png", width = 10, height = 6, dpi = 300)

# PLOT 01.3
DATA_plots %>%
  select(site, value, Latitude, Longitude, State, Ave_Plot_Size) %>%
  group_by(site) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = xtitle,
                     #limits = c(0, 0.2),
                     labels = comma) + 
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_01.3.png", width = 10, height = 6, dpi = 300)



# aggregated TN by sites and first-last year (fly) -------------------
DATA_plots %>%
  select(site, plotid, fly, value, Latitude, Longitude, State) %>%
  spread(fly, value, fill = NA) %>%
  mutate(value_diff = last - first) %>% 
  group_by(site) %>%
  select(site, value_diff, first, last, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = xtitle,
                     #limits = c(-1.2, 0.5),
                     labels = comma)+
  coord_flip() +
  theme_light() -> PLOT2

# PLOT 02.1
PLOT2 + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_02.1.png", width = 10, height = 6, dpi = 300)


# PLOT 02.2
PLOT2 + 
  facet_grid(~ State) + 
  ggtitle("Average TN Difference between the First and the Last Data Years") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_02.2.png", width = 10, height = 6, dpi = 300)


# PLOT 02.3
DATA_plots %>%
  select(site, plotid, fly, value, Latitude, Longitude, State) %>%
  spread(fly, value, fill = NA) %>%
  mutate(value_diff = last - first) %>% 
  group_by(site) %>%
  select(site, value_diff, first, last, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value_diff, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = xtitle,
                     #limits = c(-1.2, 0.5),
                     labels = comma)+
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_02.3.png", width = 10, height = 6, dpi = 300)



# aggregated CS by State --------------------
# PLOT 03
DATA_plots %>%
  group_by(State) %>%
  select(value, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) %>%
  ggplot(aes(x=State, y=value)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = xtitle,
                     #limits = c(0, 3),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_03.png", width = 10, height = 6, dpi = 300)


# aggregated CS by State and first-last year (fly) -------------------
DATA_plots %>%
  select(site, plotid, value, Latitude, Longitude, State, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  group_by(State) %>%
  select(State, value_diff, first, last, Latitude, Longitude) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) %>%
  ggplot(aes(x=State, y=value_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = xtitle,
                     #limits = c(-0.6, 0.3),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_04.png", width = 10, height = 6, dpi = 300)


# aggregated TN by Rotation --------------------
# PLOT 05
DATA_plots %>%
  group_by(crot) %>%
  select(crot, crot_name, value) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot_name, y=value, fill=I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle,
                     #limits = c(0, 1.50),
                     labels = comma) +
  theme_bw() + 
  ggtitle("Average TN by Crop Rotation") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_TN_05.png", width = 10, height = 6, dpi = 300)



# aggregated TN by Rotation and first-last year (fly) -------------------
# PLOT O6
DATA_plots %>%
  select(site, plotid, crot, crot_name, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  group_by(crot) %>%
  select(crot, crot_name, value_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot_name, y=value_diff, fill = I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotaion", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle,
                     #limits = c(-0.12, 0.08),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_TN_06.png", width = 10, height = 6, dpi = 300)



DATA_plots %>%
  select(site, plotid, crot, crot_name, value, fly, State) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(color = ifelse(State == "WI", " WI", "Other States")) %>%
  mutate(color = as.factor(color)) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(crot_name, value_diff, fill = color)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Crop Rotaion", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))


DATA_plots %>%
  select(site, plotid, crot, crot_name, value, fly, State) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(color = ifelse(State == "WI", "WI", "Other States")) %>%
  mutate(color = as.factor(color)) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(crot_name, value_diff, fill = color)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Crop Rotaion", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_TN_06a.png", width = 10, height = 6, dpi = 300)



# aggregated CS by Rotation and others --------------------
# PLOT 07
DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  select(crot, crot_name, tillage, drainage, value, State) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  ggplot(aes(x=crot_name, y=value, fill=I(color))) + 
  #stat_summary(fun.y=mean, geom="bar") + 
  geom_bar(stat = "summary", fun.y = "mean") +
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  theme_bw() + 
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_TN_07.png", width = 10, height = 6, dpi = 300)



DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  select(crot, crot_name, tillage, drainage, value, State) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  #mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  mutate(color = ifelse(State == "WI", " WI", "Other States")) %>%
  ggplot(aes(x=crot_name, y=value, fill=color)) + 
  #stat_summary(fun.y=mean, geom="bar") + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  theme_bw() + 
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot_TN_07a.png", width = 10, height = 6, dpi = 300)




# aggregated CS by Rotation and other in first-last year (fly) -------------------
# PLOT 08
DATA_plots %>%
  select(site, plotid, State, crot, crot_name, tillage, drainage, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  mutate(State = as.factor(State)) %>% 
  ungroup() %>%
  ggplot(aes(x=crot_name, y=value_diff, fill=I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion", labels = function(x) str_wrap(x, width = 16)) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "plot_TN_08.png", width = 10, height = 6, dpi = 300)



DATA_plots %>%
  select(site, plotid, State, crot, crot_name, tillage, drainage, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(State == "WI", " WI", "Other States")) %>%
  mutate(color = as.factor(color)) %>%
  mutate(State = as.factor(State)) %>% 
  ungroup() %>%
  ggplot(aes(x=crot_name, y=value_diff, fill=color)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion", labels = function(x) str_wrap(x, width = 16)) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "plot_TN_08a.png", width = 10, height = 6, dpi = 300)

#



# BOX PLOTS ===================================
setwd(new_dir)
dev.off(dev.list()["RStudioGD"])
xtitle <- "TN (%)" 

# aggregated TN by sites --------------------
DATA_plots %>%
  select(site, value, Latitude, Longitude, State) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude)]))) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() -> PLOT1

# PLOT 01.1
PLOT1 + 
  ggtitle("Average TN by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_01.1.png", width = 10, height = 6, dpi = 300)

# PLOT 01.2
PLOT1 + 
  facet_grid(~ State) + 
  ggtitle("Average TN") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_01.2.png", width = 10, height = 6, dpi = 300)

# PLOT 01.3
DATA_plots %>%
  select(site, value, Latitude, Longitude, State) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude)]))) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value, fill = State)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() +
  ggtitle("Average TN by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_01.3.png", width = 10, height = 6, dpi = 300)



# aggregated TN by sites and first-last year (fly) -------------------
DATA_plots %>%
  select(site, plotid, fly, value, Latitude, Longitude, State) %>%
  spread(fly, value, fill = NA) %>%
  mutate(value_diff = last - first) %>% 
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value_diff)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() -> PLOT2

# PLOT 02.1
PLOT2 + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_02.1.png", width = 10, height = 6, dpi = 300)


# PLOT 02.2
PLOT2 + 
  facet_grid(~ State) + 
  ggtitle("Average TN Difference between the First and the Last Data Years") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_02.2.png", width = 10, height = 6, dpi = 300)


# PLOT 02.3
DATA_plots %>%
  select(site, plotid, fly, value, Latitude, Longitude, State) %>%
  spread(fly, value, fill = NA) %>%
  mutate(value_diff = last - first) %>% 
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) %>%
  ggplot(aes(x=site, y=value_diff, fill = State)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_02.3.png", width = 10, height = 6, dpi = 300)



# aggregated CS by State --------------------
# PLOT 03
DATA_plots %>%
  select(value, Latitude, Longitude, State) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) %>%
  ggplot(aes(x=State, y=value)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() +
  ggtitle("Average TN by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_03.png", width = 10, height = 6, dpi = 300)


# aggregated CS by State and first-last year (fly) -------------------
DATA_plots %>%
  select(site, plotid, value, Latitude, Longitude, State, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  select(State, value_diff, first, last, Latitude, Longitude) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) %>%
  ggplot(aes(x=State, y=value_diff)) + 
  geom_boxplot() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = xtitle,
                     labels = comma) +
  coord_flip() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_TN_04.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation --------------------
# PLOT 05
DATA_plots %>%
  select(crot, crot_name, value) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=value, fill=I(color))) + 
  geom_boxplot() +
  scale_x_discrete(name = "Crop Rotation ID", 
                   labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  ggtitle("Average TN by Crop Rotation") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_TN_05.png", width = 10, height = 6, dpi = 300)



# aggregated CS by Rotation and first-last year (fly) -------------------
# PLOT O6
DATA_plots %>%
  select(site, plotid, crot, crot_name, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(crot = as.factor(crot)) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=value_diff, fill = I(color))) + 
  geom_boxplot() +
  scale_x_discrete(name = "Crop Rotaion ID", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  coord_flip() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_TN_06.png", width = 10, height = 6, dpi = 300)


DATA_plots %>%
  select(site, plotid, crot, crot_name, value, fly, State) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(color = ifelse(State == "WI", "WI", "Other States")) %>%
  mutate(color = as.factor(color)) %>%
  mutate(State = as.factor(State)) %>%
  ggplot(aes(crot, value_diff, fill = color)) + 
  geom_boxplot() +
  scale_x_discrete(name = "Crop Rotaion ID", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle) +
  coord_flip() + 
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_TN_06a.png", width = 10, height = 6, dpi = 300)



# aggregated CS by Rotation and others --------------------
# PLOT 07
DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=value, fill=I(color))) + 
  geom_boxplot() +
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Crop Rotation ID", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_TN_07.png", width = 10, height = 6, dpi = 300)



DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(State == "WI", " WI", "Other States")) %>%
  ggplot(aes(x=crot, y=value, fill=color)) + 
  geom_boxplot() + 
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Crop Rotation ID", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "boxplot_TN_07a.png", width = 10, height = 6, dpi = 300)




# aggregated CS by Rotation and other in first-last year (fly) -------------------
# PLOT 08
DATA_plots %>%
  select(site, plotid, State, crot, crot_name, tillage, drainage, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  mutate(State = as.factor(State)) %>% 
  ungroup() %>%
  ggplot(aes(x=crot, y=value_diff, fill=I(color))) + 
  geom_boxplot() +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion ID", labels = function(x) str_wrap(x, width = 16)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  coord_flip() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "boxplot_TN_08.png", width = 10, height = 6, dpi = 300)



DATA_plots %>%
  select(site, plotid, State, crot, crot_name, tillage, drainage, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(crot_name = as.factor(crot_name)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) %>%
  mutate(color = ifelse(State == "WI", " WI", "Other States")) %>%
  mutate(color = as.factor(color)) %>%
  mutate(State = as.factor(State)) %>% 
  ungroup() %>%
  ggplot(aes(x=crot, y=value_diff, fill=color)) + 
  geom_boxplot() +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion ID", labels = function(x) str_wrap(x, width = 16)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  coord_flip() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "boxplot_TN_08a.png", width = 10, height = 6, dpi = 300)

#

















# NEW ADDITION ===========
#+++++++++++++++++++++++++++++++++++++++++++++  NEW   +++++++++++++++++++++++++++++++++++++++++++++++++


# PLOT SOIL13 and SOIL1 for WI sites ARL, LAN, MAR -----------
ARL <- soil_all_data %>% filter(site == "ARL")
LAN <- soil_all_data %>% filter(site == "LAN")
MAR <- soil_all_data %>% filter(site == "MAR")

#SOC ...............................................
ARL %>% 
  ggplot(aes(x=depth, y=SOIL13)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in ARL\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "ARL_SOC.png", width = 12, dpi = 300)

LAN %>% 
  ggplot(aes(x=depth, y=SOIL13)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in LAN\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "LAN_SOC.png", width = 12, dpi = 300)

MAR %>% 
  ggplot(aes(x=depth, y=SOIL13)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in MAR\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "MAR_SOC.png", width = 12, dpi = 300)

#BD ...............................................
ARL %>% 
  ggplot(aes(x=depth, y=SOIL1)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "Bulk Density (g/cm3)") +
  ggtitle("Soil Bulk Density in ARL\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "ARL_BD.png", width = 12, dpi = 300)

LAN %>% 
  ggplot(aes(x=depth, y=SOIL1)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "Bulk Density (g/cm3)") +
  ggtitle("Soil Bulk Density in LAN\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "LAN_BD.png", width = 12, dpi = 300)

MAR %>% 
  ggplot(aes(x=depth, y=SOIL1)) + 
  geom_point(aes(color=plotid), size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "Bulk Density (g/cm3)") +
  ggtitle("Soil Bulk Density in MAR\n(plot averages)") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
ggsave(filename = "MAR_BD.png", width = 12, dpi = 300)








# aggregated CS by COVER CROP ROTATIONS ============================================
# PLOT 05
DATA_plots %>%
  mutate(cover_crop = ifelse(crot %in% c("CR01", "CR02", "CR05", "CR07", "CR08"), "no cover crop", "with cover crop")) %>%
  ggplot(aes(x=cover_crop, y=value)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = xtitle, labels = comma) +
  facet_grid( ~ fly) +
  theme_bw() + 
  ggtitle("Average TN by Crop Rotations with and without Cover Crops\n(comparison of the first and the last data years)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_CC_05.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and first-last year (fly) -------------------
# PLOT O6
DATA_plots %>%
  mutate(cover_crop = ifelse(crot %in% c("CR01", "CR02", "CR05", "CR07", "CR08"), "no cover crop", "with cover crop")) %>%
  select(site, plotid, cover_crop, crot_name, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  ggplot(aes(x=cover_crop, y=value_diff)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_x_discrete(name = "Crop Rotaion") +
  scale_y_continuous(name = xtitle) +
  #coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation with and without Cover Crops") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_CC_06.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and others --------------------
# PLOT 07
DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(cover_crop = ifelse(crot %in% c("CR01", "CR02", "CR05", "CR07", "CR08"), "no cover crop", "with cover crop")) %>%
  select(cover_crop, crot_name, tillage, drainage, value, State, fly) %>%
  ggplot(aes(x=cover_crop, y=value, fill = fly)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Crop Rotation") +
  scale_y_continuous(name = xtitle) +
  theme_bw() + 
  ggtitle("Average TN by Crop Rotation with and without Cover Crops\ngrouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_CC_07.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and other in first-last year (fly) -------------------
# PLOT 08
DATA_plots %>%
  mutate(cover_crop = ifelse(crot %in% c("CR01", "CR02", "CR05", "CR07", "CR08"), "no cover crop", "with cover crop")) %>%
  select(site, plotid, cover_crop, crot_name, value, fly, drainage, tillage) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  ungroup() %>%
  ggplot(aes(x=cover_crop, y=value_diff)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion") +
  scale_y_continuous(name = xtitle) +
  #  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation with and without Cover Crops grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_CC_08.png", width = 10, height = 6, dpi = 300)

#





# aggregated CS by DIVERSE CROP ROTATIONS ============================================
# 3 or more crops in the system are considered as diverse
# 1 crop (such as cont C or cont S) is excluded
# PLOT 05
DATA_plots %>%
  mutate(diverse_crop = ifelse(crot %in% c("CR01", "CR02"), "mono-cropping", 
                               ifelse(crot %in% c("CR03", "CR04", "CR05"), 
                                      "bi-cropping", "multi-cropping"))) %>%
  mutate(diverse_crop = factor(diverse_crop, 
                               levels = c("mono-cropping","bi-cropping", "multi-cropping"))) %>%
  ggplot(aes(x=diverse_crop, y=value)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_x_discrete(name = "Cropping System") +
  scale_y_continuous(name = xtitle, labels = comma) +
  facet_grid( ~ fly) +
  theme_bw() + 
  ggtitle("Average TN by Diversity of Cropping System\n(comparison of the first and the last data years)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_DC_05.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and first-last year (fly) -------------------
# PLOT O6
DATA_plots %>%
  mutate(diverse_crop = ifelse(crot %in% c("CR01", "CR02"), "mono-cropping", 
                               ifelse(crot %in% c("CR03", "CR04", "CR05"), 
                                      "bi-cropping", "multi-cropping"))) %>%
  mutate(diverse_crop = factor(diverse_crop, 
                               levels = c("mono-cropping","bi-cropping", "multi-cropping"))) %>%
  select(site, plotid, diverse_crop, crot_name, value, fly) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  ggplot(aes(x=diverse_crop, y=value_diff)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  scale_x_discrete(name = "Cropping System") +
  scale_y_continuous(name = xtitle) +
  #coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Diversity of Cropping System") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_DC_06.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and others --------------------
# PLOT 07
DATA_plots %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  mutate(diverse_crop = ifelse(crot %in% c("CR01", "CR02"), "mono-cropping", 
                               ifelse(crot %in% c("CR03", "CR04", "CR05"), 
                                      "bi-cropping", "multi-cropping"))) %>%
  mutate(diverse_crop = factor(diverse_crop, 
                               levels = c("mono-cropping","bi-cropping", "multi-cropping"))) %>%
  select(diverse_crop, crot_name, tillage, drainage, value, State, fly) %>%
  ggplot(aes(x=diverse_crop, y=value, fill = fly)) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  facet_grid(tillage ~ drainage) + 
  scale_x_discrete(name = "Cropping System") +
  scale_y_continuous(name = xtitle) +
  theme_bw() + 
  ggtitle("Average TN by Diversity of Cropping System\ngrouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_DC_07.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation and other in first-last year (fly) -------------------
# PLOT 08
DATA_plots %>%
  mutate(diverse_crop = ifelse(crot %in% c("CR01", "CR02"), "mono-cropping", 
                               ifelse(crot %in% c("CR03", "CR04", "CR05"), 
                                      "bi-cropping", "multi-cropping"))) %>%
  mutate(diverse_crop = factor(diverse_crop, 
                               levels = c("mono-cropping","bi-cropping", "multi-cropping"))) %>%
  select(site, plotid, diverse_crop, crot_name, value, fly, drainage, tillage) %>%
  spread(fly, value) %>%
  mutate(value_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  ungroup() %>%
  ggplot(aes(x=diverse_crop, y=value_diff)) + 
  geom_bar(stat = "summary", fun.y = "mean") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Cropping System") +
  scale_y_continuous(name = xtitle) +
  #  coord_flip() +
  theme_light() + 
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Diversity of Cropping System grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_TN_DC_08.png", width = 10, height = 6, dpi = 300)

#
