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
  filter(varname %in% c("SOIL1", "SOIL13", "SOIL14")) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  select(site, plotid, depth, year, subsample, varname, value) %>%
  spread(key = varname, value = value, fill = NA) %>%
  mutate(SOILCN = SOIL13/SOIL14) -> soil_all


## get rid off  CC plots with continous corn rotation (ROT1) in SERF
soil_all %>% filter(!(site == "SERF" & grepl("C", plotid))) -> soil_all

soil_all %>% 
  mutate(site = as.factor(site),
         depth = as.factor(depth)) -> soil_all



# Plot
soil_all %>%
  filter(!is.na(SOIL14)) %>%
  ggplot(aes(x=site, y=SOILCN)) +
  geom_boxplot() + 
  coord_flip() +
  scale_y_continuous(name = "C:N") +
  ggtitle("Carbon to Nitrogen Ratio\n(actual data)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))
ggsave(filename = "CN.png", width = 12, height = 8, dpi = 300)




# FUNCTIONS to find outliers or out-outliers 
# Outlier
is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | 
           x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
}
# Out-of-Outlier
is_toomuch <- function(x) {
  return(x < quantile(x, 0.25, na.rm = T) - 4 * IQR(x, na.rm = T) | 
           x > quantile(x, 0.75, na.rm = T) + 4 * IQR(x, na.rm = T))
}


# add colums for out-of-outliers
soil_all %>% 
  group_by(site, depth) %>%
  mutate(outlier13 = ifelse(is_toomuch(SOIL13), plotid, as.character(NA)),
         outlier14 = ifelse(is_toomuch(SOIL14), plotid, as.character(NA)),
         outlierCN = ifelse(is_toomuch(SOILCN), plotid, as.character(NA))) %>%
  ungroup()-> soil_data


# PLOT TIME 
new_dir <- paste0(getwd(), "/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Soil_Figs/", Sys.Date())
dir.create(new_dir)
setwd(new_dir)



# PLOT soil bulk density (BD)
var <- c("SOIL1", "Soil Bulkd Density (gm/cm3)")
site_list <- as.character(unique(soil_data$site))

for (i in 1:28) {
  site_name <- site_list[i]
  soil_data %>% 
    filter(!is.na(SOIL1)) %>% 
    filter(site == site_name) %>%
    
    ggplot(aes(x=depth, y=get(var[1]))) +
    geom_boxplot(aes(color = I("lightblue2")), outlier.colour = "orange", outlier.size = 3) +
    geom_point(size = 2) + 
    facet_grid(~ as.factor(year)) + 
    scale_x_discrete(name = "Depth (cm)") +
    scale_y_continuous(name = var[2]) +
    ggtitle(paste("Soil Bulkd Density in", site_name)) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    
  ggsave(filename = paste0(site_name, "_", var[1], ".png"), width = 12, height = 8, dpi = 300)
}




# PLOT soil organic carbon (SOC)
var <- c("SOIL13", "SOC (%)")
site_list <- as.character(unique(soil_data$site))

for (i in 1:28) {
  site_name <- site_list[i]
  soil_data %>% 
    filter(!is.na(SOIL13)) %>% 
    filter(site == site_name) %>%
    
    ggplot(aes(x=depth, y=get(var[1]))) + 
    geom_boxplot(aes(color = I("lightblue2")), outlier.colour = "orange", outlier.size = 3) +
    geom_point(size = 2) + 
    facet_grid(~ as.factor(year)) + 
    scale_x_discrete(name = "Depth (cm)") +
    scale_y_continuous(name = var[2]) +
    ggtitle(paste("Soil Organic Carbon in", site_name)) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    #geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
    geom_point(data = . %>% filter(!is.na(outlier13)), 
               aes(x=depth, y=get(var[1])), color = "orange", size = 3)
  ggsave(filename = paste0(site_name, "_", var[1], ".png"), width = 12, height = 8, dpi = 300)
}





# PLOT soil total nitrogen (TN)
var <- c("SOIL14", "TN (%)")
site_list <- as.character(unique(soil_data$site))

for (i in 1:28) {
  site_name <- site_list[i]
  soil_data %>% 
    filter(!is.na(SOIL14)) %>% 
    filter(site == site_name) %>%
    
    ggplot(aes(x=depth, y=get(var[1]))) + 
    geom_boxplot(aes(color = I("lightblue2")), outlier.colour = "orange", outlier.size = 3) +
    geom_point(size = 2) + 
    facet_grid(~ as.factor(year)) + 
    scale_x_discrete(name = "Depth (cm)") +
    scale_y_continuous(name = var[2]) +
    ggtitle(paste("Soil Total Nitrogen in", site_name)) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    #geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
    geom_point(data = . %>% filter(!is.na(outlier14)), 
               aes(x=depth, y=get(var[1])), color = "orange", size = 3)
  ggsave(filename = paste0(site_name, "_", var[1], ".png"), width = 12, height = 8, dpi = 300)
}




# PLOT C:N ratio
var <- c("SOILCN", "CN")
site_list <- as.character(unique(soil_data$site))

for (i in 1:28) {
  site_name <- site_list[i]
  soil_data %>% 
    filter(!is.na(SOILCN)) %>% 
    filter(site == site_name) %>%
    
    ggplot(aes(x=depth, y=get(var[1]))) + 
    geom_boxplot(aes(color = I("lightblue2"))) +
    geom_point(size = 2) + 
    facet_grid(~ as.factor(year)) + 
    scale_x_discrete(name = "Depth (cm)") +
    scale_y_continuous(name = var[2]) +
    ggtitle(paste("C:N Ratio in", site_name)) +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    #geom_text(aes(label = outlier), na.rm = TRUE, vjust = 1.5, hjust = 0.5) +
    geom_point(data = . %>% filter(!is.na(outlierCN)), 
               aes(x=depth, y=get(var[1])), color = "orange", size = 3)
  ggsave(filename = paste0(site_name, "_", var[1], ".png"), width = 12, height = 8, dpi = 300)
}


















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

