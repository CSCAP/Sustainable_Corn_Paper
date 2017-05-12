# PREPARING SOC FOR SASHA
# 



library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
library(dplyr)



setwd("C:/Users/Gio/Documents")

load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/soil.RData")
load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/plots.RData")
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


# BOX + POINT plots of SOIL data =============================
new_dir <- paste0(getwd(), "/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Soil_Figs/", Sys.Date())
dir.create(new_dir)
setwd(new_dir)

data_frame(code = c("SOIL1", "SOIL13", "SOIL14"),
           name = c("Soil Bulk Density",
                    "Soil Organic Carbon",
                    "Soil Total Nitrogen"),
           units = c("(gm/cm3)",
                     "(%)",
                     "(%)")) -> plot_vars
unique(soil$site) -> plot_sites

for (i in 1:3) {
  for (j in 1:length(plot_sites)) {
    soil %>%
      filter(varname == plot_vars$code[i], site == plot_sites[j]) %>%
      filter(!is.na(value)) %>% 
      ggplot(aes(x = depth, y = value)) +
      geom_boxplot(colour = "lightblue") +
      geom_point() +
      facet_grid(~ year) +
      ggtitle(paste(plot_vars$name[i], "in", plot_sites[j])) +
      labs(x = "Sampling Depth (cm)",
           y = paste(plot_vars$name[i], plot_vars$units[i])) +
      theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5), 
            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 11, angle = 90),
            axis.text.y = element_text(size = 11),
            strip.text = element_text(size = 14, face = "bold")) +
      ggsave(filename = paste0(plot_sites[j], "_", plot_vars$code[i], ".png"), 
             width = 12, height = 8, dpi = 300)
  }
}

  

# CLEANING of the DATES  ================================================
# removing questionable data

# remove 2015 data at 20-60 cm in ARL
soil %>% filter(depth != "20 to 60") -> soil

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



# =============
plots %>%
  select(uniqueid, plotid, tillage, nitrogen, drainage, rotation, crot, crot_name) %>%
  rename(site = uniqueid) %>%
  arrange(site, plotid) %>%
  filter(!nitrogen %in% c("NIT1", "NIT3", "NIT4")) %>%
  mutate(tillage = ifelse(tillage == "TIL2", "YES", "NO")) %>%
  mutate(cover = ifelse(grepl("RYE cover", crot_name), "YES", "NO")) %>%
  mutate(extended_rot = ifelse(grepl("WHEAT", crot_name), "YES", "NO")) %>%
  select(site, plotid, tillage, cover, extended_rot, crot_name, rotation) %>%
  rename(extended_rot_name = crot_name, rot_id = rotation) -> plot_id

soil %>%
  filter(varname != "SOIL14") %>%
  left_join(plot_id, by = c("site", "plotid")) %>%
  mutate(varname = ifelse(varname == "SOIL1", "BD", "SOC")) %>%
  mutate(sampling_time = ifelse(grepl("BRADFORD", site), "fall", "spring")) %>%
  #mutate(sampling_time = ifelse(site == "STJOHNS" & year == 2013, "June-19", sampling_time)) %>%
  mutate(tillage = ifelse(site == "SEPAC" & year == 2011, "YES", tillage),
         tillage = ifelse(site == "STJOHNS" & year == 2011, "NO", tillage)) %>%
  write.csv(file = "~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/SASHA_SOC.csv", 
            row.names = FALSE, 
            na = ".")
