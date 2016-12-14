# C:N ratio
# this code calculates Carbon to Nitrogen Ratio for all plots  

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


# select SOC (SOIL13) and TN (SOIL14)
soil %>% 
  filter(varname %in% c("SOIL13", "SOIL14")) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  select(site, plotid, depth, year, subsample, varname, value) %>%
  spread(key = varname, value = value, fill = NA) %>%
  mutate(value = SOIL13/SOIL14) %>%
  filter(!is.na(value)) -> soil_all


## get rid off  CC plots with continous corn rotation (ROT1) in SERF
soil_all %>% filter(!(site == "SERF" & grepl("C", plotid))) -> soil_all

soil_all %>% 
  mutate(site = as.factor(site),
         depth = as.factor(depth)) -> soil_all

# Plot
soil_all %>%
  ggplot(aes(x=site, y=value)) +
  geom_boxplot() + 
  coord_flip() +
  scale_y_continuous(name = "C:N") +
  ggtitle("Carbon to Nitrogen Ratio\n(actual data)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "CN.png", width = 12, height = 8, dpi = 300)






















# IDs ---------------------------------------------------------------
# set "id"to be used for sorting of "soil_all"
soil_all %>%
  mutate(id = paste(site, year, sep = "_")) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id3 = paste(site, plotid, year, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> soil_all




# SOC total  =================================================================================

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

# calculate total SOC data
soil_data %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, value) %>%
  filter(!is.na(value)) %>%                                # SEE COMMENTS ABOUT "NOT YET DONE"
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
xtitle <- "SOC (%)" 

# aggregated SOC by sites --------------------
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
                     limits = c(0, 3),
                     labels = comma) +
  coord_flip() +
  theme_light() -> PLOT1

# PLOT 01.1
PLOT1 + 
  ggtitle("Average SOC by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_01.1.png", width = 10, height = 6, dpi = 300)

# PLOT 01.2
PLOT1 + 
  facet_grid(~ State) + 
  ggtitle("Average SOC") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_01.2.png", width = 10, height = 6, dpi = 300)

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
                     limits = c(0, 3),
                     labels = comma) + 
  coord_flip() +
  theme_light() +
  ggtitle("Average SOC by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_01.3.png", width = 10, height = 6, dpi = 300)



# aggregated SOC by sites and first-last year (fly) -------------------
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
                     limits = c(-1.2, 0.5),
                     labels = comma)+
  coord_flip() +
  theme_light() -> PLOT2

# PLOT 02.1
PLOT2 + 
  ggtitle("Average SOC Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_02.1.png", width = 10, height = 6, dpi = 300)


# PLOT 02.2
PLOT2 + 
  facet_grid(~ State) + 
  ggtitle("Average SOC Difference between the First and the Last Data Years") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_02.2.png", width = 10, height = 6, dpi = 300)

  
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
                     limits = c(-1.2, 0.5),
                     labels = comma)+
  coord_flip() +
  theme_light() +
  ggtitle("Average SOC Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_02.3.png", width = 10, height = 6, dpi = 300)



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
                     limits = c(0, 3),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average SOC by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_03.png", width = 10, height = 6, dpi = 300)


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
                     limits = c(-0.6, 0.3),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average SOC Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "plot_SOC_04.png", width = 10, height = 6, dpi = 300)


# aggregated CS by Rotation --------------------
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
                     limits = c(0, 1.50),
                     labels = comma) +
  theme_bw() + 
  ggtitle("Average SOC by Crop Rotation") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_SOC_05.png", width = 10, height = 6, dpi = 300)



# aggregated CS by Rotation and first-last year (fly) -------------------
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
                     limits = c(-0.12, 0.08),
                     labels = comma) +
  coord_flip() +
  theme_light() + 
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_SOC_06.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation") +
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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_SOC_06a.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "plot_SOC_07.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "plot_SOC_07a.png", width = 10, height = 6, dpi = 300)




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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "plot_SOC_08.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "plot_SOC_08a.png", width = 10, height = 6, dpi = 300)

#


# BOX PLOTS ===================================
setwd(new_dir)

# aggregated SOC by sites --------------------
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
  ggtitle("Average SOC by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_01.1.png", width = 10, height = 6, dpi = 300)

# PLOT 01.2
PLOT1 + 
  facet_grid(~ State) + 
  ggtitle("Average SOC") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_01.2.png", width = 10, height = 6, dpi = 300)

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
  ggtitle("Average SOC by Site") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_01.3.png", width = 10, height = 6, dpi = 300)



# aggregated SOC by sites and first-last year (fly) -------------------
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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_02.1.png", width = 10, height = 6, dpi = 300)


# PLOT 02.2
PLOT2 + 
  facet_grid(~ State) + 
  ggtitle("Average SOC Difference between the First and the Last Data Years") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_02.2.png", width = 10, height = 6, dpi = 300)


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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_02.3.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_03.png", width = 10, height = 6, dpi = 300)


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
  ggtitle("Average SOC Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "boxplot_SOC_04.png", width = 10, height = 6, dpi = 300)


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
  ggtitle("Average SOC by Crop Rotation") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_SOC_05.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_SOC_06.png", width = 10, height = 6, dpi = 300)


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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_SOC_06a.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(filename = "boxplot_SOC_07.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave(filename = "boxplot_SOC_07a.png", width = 10, height = 6, dpi = 300)




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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "boxplot_SOC_08.png", width = 10, height = 6, dpi = 300)



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
  ggtitle("Average SOC Difference between the First and the Last Data Years by Crop Rotation\n grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.y = element_text(size = 8))
ggsave(filename = "boxplot_SOC_08a.png", width = 10, height = 6, dpi = 300)

#


























# NEW ADDITION ===========
#+++++++++++++++++++++++++++++++++++++++++++++  NEW   +++++++++++++++++++++++++++++++++++++++++++++++++

#functions to find outliers or out-outliers 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


is_toomuch <- function(x) {
  return(x < quantile(x, 0.25) - 4 * IQR(x) | x > quantile(x, 0.75) + 4 * IQR(x))
}





# Plot SOC for sites FREEMAN, WATERMAN, STJOHNS
# adding outlier function ..............................................


# FREEMAN 
soil %>% 
  filter(site == "FREEMAN", varname == "SOIL13") %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%  
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) %>% 
  filter(!is.na(value)) %>%
  group_by(depth) %>%
  mutate(outlier = ifelse(is_toomuch(value), plotid, as.character(NA))) %>%
  ungroup() %>% 
  #filter(!is.na(outlier))
  #head()
  
  ggplot(aes(x=depth, y=value)) + 
  geom_point(size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in FREEMAN") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
  geom_point(data = . %>% filter(!is.na(outlier)), aes(x=depth, y=value), color = "orange", size = 3)
ggsave(filename = "FREEMAN.png", width = 12, height = 8, dpi = 300)


# WATERMAN 
soil %>% 
  filter(site == "WATERMAN", varname == "SOIL13") %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%  
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) %>% 
  filter(!is.na(value)) %>%
  group_by(depth) %>%
  mutate(outlier = ifelse(is_toomuch(value), plotid, as.character(NA))) %>%
  ungroup() %>% 
  #filter(!is.na(outlier))
  #head()
  
  ggplot(aes(x=depth, y=value)) + 
  geom_point(size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in WATERMAN") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
  geom_point(data = . %>% filter(!is.na(outlier)), aes(x=depth, y=value), color = "orange", size = 3)
ggsave(filename = "WATERMAN.png", width = 12, height = 8, dpi = 300)


# STJOHNS 
soil %>% 
  filter(site == "STJOHNS", varname == "SOIL13") %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%  
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) %>% 
  filter(!is.na(value)) %>%
  group_by(depth) %>%
  mutate(outlier = ifelse(is_toomuch(value), plotid, as.character(NA))) %>%
  ungroup() %>% 
  #filter(!is.na(outlier))
  #head()
  
  ggplot(aes(x=depth, y=value)) + 
  geom_point(size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "SOC (%)") +
  ggtitle("Soil Organic Carbon in STJOHNS") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
  geom_point(data = . %>% filter(!is.na(outlier)), aes(x=depth, y=value), color = "orange", size = 3)
ggsave(filename = "STJOHNS.png", width = 12, height = 8, dpi = 300)




# Plot TN for sites FREEMAN, WATERMAN, STJOHNS
# adding outlier function ..............................................


# FREEMAN 
soil %>% 
  filter(site == "FREEMAN", varname == "SOIL14") %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%  
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) %>% 
  filter(!is.na(value)) %>%
  group_by(depth) %>%
  mutate(outlier = ifelse(is_toomuch(value), plotid, as.character(NA))) %>%
  ungroup() %>% 
  #filter(!is.na(outlier))
  #head()
  
  ggplot(aes(x=depth, y=value)) + 
  geom_point(size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "TN (%)") +
  ggtitle("Soil Organic Carbon in FREEMAN") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
  geom_point(data = . %>% filter(!is.na(outlier)), aes(x=depth, y=value), color = "orange", size = 3)
ggsave(filename = "FREEMAN_TN.png", width = 12, height = 8, dpi = 300)


# WATERMAN 
soil %>% 
  filter(site == "WATERMAN", varname == "SOIL14") %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%  
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) %>% 
  filter(!is.na(value)) %>%
  group_by(depth) %>%
  mutate(outlier = ifelse(is_toomuch(value), plotid, as.character(NA))) %>%
  ungroup() %>% 
  #filter(!is.na(outlier))
  #head()
  
  ggplot(aes(x=depth, y=value)) + 
  geom_point(size = 2) + 
  facet_grid(~ as.factor(year)) + 
  scale_x_discrete(name = "Depth (cm)") +
  scale_y_continuous(name = "TN (%)") +
  ggtitle("Soil Organic Carbon in WATERMAN") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
  geom_point(data = . %>% filter(!is.na(outlier)), aes(x=depth, y=value), color = "orange", size = 3)
ggsave(filename = "WATERMAN_TN.png", width = 12, height = 8, dpi = 300)


  
  
  
  
  
