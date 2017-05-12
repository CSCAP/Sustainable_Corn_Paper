# MANAGE DATABASE
# Analysing NO3-N Load 
library(readxl)
library(tidyverse)
library(stringr)

ag_load <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/MANAGE DB/manage2016/ag load Apr 2016.xlsx")
drain_load <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/MANAGE DB/manage2016/drain load Oct 2015.xlsx")


drain_load %>%
  select(RefID, Year, `Watershed ID`, `State/Province`, 
         `Drainage type`, `Drain spacing (m)`, `Drain depth (m)`, #`Min Drainage Area (ha)`, `Max Drainage Area (ha)`, 
         `Land Use`, Tillage2, `Dominant Soil Type`,
         `Avg N Applied (kg/ha)`, `Avg P Applied (kg/ha)`,
         `Crop 1`, `Avg Crop 1 Yield (Mg/ha)`, `Crop 2`, `Avg Crop 2 Yield (Mg/ha)`,
         `Avg Precipitation (mm)`, `Drainage Discharge (mm)`, 
         `Avg Soil Loss (kg/ha)`, 
         `Avg Dissolved N (kg/ha)`, `Form Dissolved N`,
         `Avg Dissolved P (kg/ha)`, `Form Dissolved P`,
         `Sample Collection Timing`, `Full year`, Comments) -> drain

drain %>%
  filter(!is.na(`Avg Dissolved N (kg/ha)`)) %>%                # select entries with NO3-N load data
  filter(grepl("corn", `Land Use`, ignore.case = TRUE)) %>%    # select rotations with corn
  filter(!RefID %in% c(52, 80, 95)) %>%                        # remove papers with repetitive NO3-N load data
  mutate(ID = as.factor(paste(RefID, `Watershed ID`, sep = "_")),
         state = as.factor(`State/Province`),
         siteID = as.factor(RefID),
         plotID = as.factor(`Watershed ID`),
         rotation = `Land Use`,
         tillage = Tillage2,
         soil_type = `Dominant Soil Type`,
         drainage_type = as.factor(`Drainage type`),
         drainage_spacing = as.numeric(`Drain spacing (m)`),
         drainage_depth = as.numeric(`Drain depth (m)`),
         N_applied = as.numeric(`Avg N Applied (kg/ha)`),
         P_applied = as.numeric(`Avg P Applied (kg/ha)`),
         crop_1 = as.factor(str_to_lower(`Crop 1`)),
         crop_yield_1 = as.numeric(`Avg Crop 1 Yield (Mg/ha)`),
         crop_2 = as.factor(str_to_lower(`Crop 2`)),
         crop_yield_2 = as.numeric(`Avg Crop 2 Yield (Mg/ha)`),
         precip_mm = as.double(`Avg Precipitation (mm)`),
         drainage_mm = as.double(`Drainage Discharge (mm)`),
         soil_loss = as.double(`Avg Soil Loss (kg/ha)`),
         N_load = as.double(`Avg Dissolved N (kg/ha)`),
         N_form = `Form Dissolved N`,
         P_load = as.double(`Avg Dissolved P (kg/ha)`),
         P_form = `Form Dissolved P`,
         sample_timing = `Sample Collection Timing`,
         comments = Comments) %>%
  select(-c(1:26)) -> drain



drain %>%
  ggplot(aes(x=drainage_type, y=N_load)) +
  geom_boxplot(na.rm = TRUE) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, color = "blue", shape = 3, stroke = 1.5) +
  scale_x_discrete(name = "Drainage Type", labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(name = "Nitrate-N Load, kg/ha", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))





