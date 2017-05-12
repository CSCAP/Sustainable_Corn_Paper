library(readxl)
library(tidyverse)
library(stringr)
library(scales)


# MANAGE db =====================================================
manage_db <-
  read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/MANAGE DB/manage2016/drain load Oct 2015.xlsx")

manage_db %>%
  select(ID, RefID, Year, `Watershed ID`, `State/Province`,
         `Drainage type`, `Drain depth (m)`, `Drain spacing (m)`,
         `Land Use`, Tillage2, `Dominant Soil Type`,
         `Crop 1`, `Avg Crop 1 Yield (Mg/ha)`, `Crop 2`, `Avg Crop 2 Yield (Mg/ha)`,
         `Avg Precipitation (mm)`, `Drainage Discharge (mm)`,
         `Avg Soil Loss (kg/ha)`, `Avg Dissolved N (kg/ha)`, 
         `Avg Dissolved P (kg/ha)`, `Avg Total P (kg/ha)`,
         Comments) %>%
  filter(!is.na(`Avg Dissolved N (kg/ha)`)) %>%                # select entries with NO3-N load data
  filter(`State/Province` %in% c("IA", "MO", "MN", "IN", "OH", "IL")) %>% 
  filter(`Drainage type` == "Subsurface (no inlets specified)") %>%
  filter(grepl("corn", `Land Use`, ignore.case = TRUE)) %>%    # select rotations with corn
  mutate(id = ID, 
         uniqueID = as.factor(paste(RefID, `Watershed ID`, sep = "_")),
         state = as.factor(`State/Province`),
         siteID = as.factor(RefID),
         plotID = as.factor(`Watershed ID`),
         year = as.numeric(Year),
         rotation = `Land Use`,
         tillage = Tillage2,
         soil_type = `Dominant Soil Type`,
         drainage_type = as.factor(`Drainage type`),
         drainage_spacing = as.numeric(`Drain spacing (m)`),
         drainage_depth = as.numeric(`Drain depth (m)`),
         crop_1 = as.factor(str_to_lower(`Crop 1`)),
         crop_yield_1 = as.numeric(`Avg Crop 1 Yield (Mg/ha)`),
         crop_2 = as.factor(str_to_lower(`Crop 2`)),
         crop_yield_2 = as.numeric(`Avg Crop 2 Yield (Mg/ha)`),
         precip_mm = as.double(`Avg Precipitation (mm)`),
         drainage_mm = as.double(`Drainage Discharge (mm)`),
         soil_loss = as.double(`Avg Soil Loss (kg/ha)`),
         N_load = as.double(`Avg Dissolved N (kg/ha)`),
         P_load = as.double(`Avg Dissolved P (kg/ha)`),
         comments = Comments) %>%
  select(id:comments) -> manage_drainage

manage_drainage %>%
  mutate(STATE = state, 
         SITE = siteID,
         PLOT = plotID,
         DRAINAGE = "free",
         YEAR = year,
         LOAD = N_load) %>%
  select(STATE:LOAD) -> manage_load



# CSCAP Data =============================================================================
read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/NO3-N Loss.xlsx", sheet = "CSCAP", 
           col_types = c("text", "text", "text", "text", 
                         "numeric", "numeric",  "numeric", "numeric", "text")) -> N_load

N_load %>%
  rename(LOAD = `NO3-N Loss (kg/ha)`, DRAINAGE = Treatment) %>%
  filter(Project == "CSCAP") %>%
  filter(!SITE %in% c("BRADFORD.A", "WATERMAN")) %>%
  filter(DRAINAGE != "shallow drainage") %>%
  mutate(DRAINAGE = ifelse(DRAINAGE == "cont. drainage", "controlled", "free")) -> cscap_load


# MERGE TWO DATA ==============================================================

N_load %>%
  filter(Project == "MANAGE") %>%
  group_by(STATE, SITE, PLOT) %>%
  summarise(Latitude = first(Latitude), Longitude = first(Longitude)) -> manage_coordinates

manage_load %>%
  left_join(manage_coordinates, by = c("STATE", "SITE", "PLOT")) %>%
  mutate(Project = "MANAGE") %>% 
  bind_rows(cscap_load %>% filter(YEAR > 2010)) -> nload

save(nload, file = "~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/nload.Rda")


# PLOT TIME ==============================================================
# This generated plots based on PLOT-YEAR
#load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/nload.Rda")  

setwd("~/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Water_Figs")

# get Cumulative Frequency Distribution function of CSCAP Free Drainage
nload %>%
  filter(DRAINAGE == "free",
         Project == "CSCAP") %>%
  group_by(SITE, PLOT, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  ungroup() %>%
  select(LOAD) %>% 
  collect() %>%
  .[["LOAD"]] %>%
  ecdf() -> cscap_free_ecdf

data.frame(regions = c("Iowa", "Indiana", "Minnesota"),
           max_loads = c(17, NA, 7), 
           y_text = rep(0.99, 3)) %>%
  mutate(CF_free = cscap_free_ecdf(max_loads)) %>%
  mutate(CF_free = round(CF_free, digits = 3))-> vlines


# FIG.1 - Cummulative frequency of N loads 
nload %>%
  mutate(YEAR = ifelse(is.na(YEAR), 2020, YEAR)) %>%
  filter(DRAINAGE == "free") %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, PLOT, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  #group_by(Project) %>% count()
  ggplot(aes(x=LOAD, y=CF, colour=Project, linetype=Project)) +
  geom_line(size = 1.5) +
  #geom_point(size = 3) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_color_manual(values = c("#1f78b4", "#ffd19d", "#d95f02","#4D4325")) +  
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +

  geom_vline(xintercept = vlines$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  geom_hline(yintercept = vlines$CF_free, colour = "gray40", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines, mapping = aes(x=0, y=CF_free, label = paste(CF_free*100, "%")),
            size = 4, color = "gray40", vjust = -0.25, hjust = 0.25) +
  
  #scale_y_continuous(breaks = sort(c(seq(0, 1, 0.25), vlines$CF_free)), labels = percent) +
  
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.8, 0.25),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  # geom_segment(data = vlines %>% filter(!is.na(max_loads)), 
  #              aes(x = 0, y = CF_free, xend = max_loads, yend = CF_free),
  #              inherit.aes = F, 
  #              colour = "gray40",
  #              linetype = "dashed") +
  
  geom_segment(inherit.aes = FALSE, 
               data = vlines %>% filter(!is.na(max_loads)),
               aes(x = (max_loads+ 0.5), y = CF_free, xend = 140, yend = CF_free), 
               colour = "white") +
  
  coord_cartesian(xlim = c(0, 100)) +
  
  ggsave(filename = "fig1.png", width = 6, height = 4, dpi = 300)
  


# FIG.2 - Boxplots of N load 
nload %>%
  filter(DRAINAGE == "free", !is.na(YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, PLOT, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  ggplot(aes(x=YEAR, y=LOAD, group=interaction(YEAR, Project), fill=Project)) +
  geom_boxplot() +
  geom_hline(yintercept = vlines$max_loads, 
             size = 1, colour = "darkred", alpha = 0.85, linetype = "dashed") +
  scale_fill_manual(values = c("#1f78b4", "#ffd19d","#4D4325")) + 
#  scale_fill_manual(values = c("#1f78b4", "#d95f02","#4D4325")) + 
  scale_y_continuous(breaks = sort(c(seq(0, 100, 50), vlines$max_loads))) +
  labs(x = "Year", 
       y = "Nitrate-N Load (kg/ha)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
       panel.grid.major = element_blank(),
       axis.title = element_text(size = 18),
       axis.text = element_text(size = 16),
       axis.ticks = element_line(size = 1),
       legend.position = c(0.75, 0.85),
       legend.key.size = unit(1.5, 'lines'),
       legend.title = element_blank(),
       legend.text = element_text(size = 16, color = "gray20"),
       legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  ggsave(filename = "fig2.png", width = 6, height = 4, dpi = 300)
  




# FIG.3 - Cummulative frequency of N loads 
nload %>%
  mutate(YEAR = ifelse(is.na(YEAR), 2020, YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, PLOT, DRAINAGE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project, DRAINAGE) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  #group_by(Project) %>% count()
  ggplot(aes(x=LOAD, y=CF, 
             colour=interaction(Project, DRAINAGE), 
             linetype=interaction(Project, DRAINAGE))) +
  geom_line(size = 1.5) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted"),
                        labels = c("Controlled Drainage - CSCAP", 
                                   "Free Drainage - CSCAP", 
                                   "Free Drainage - MANAGE")) +
  scale_color_manual(values = c("#ACCD42", "#1f78b4", "#ffd19d", "#d95f02","#4D4325"),
                     labels = c("Controlled Drainage - CSCAP", 
                                "Free Drainage - CSCAP", 
                                "Free Drainage - MANAGE")) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +
  
  geom_vline(xintercept = vlines$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.65, 0.2),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  coord_cartesian(xlim = c(0, 100)) +
  
  ggsave(filename = "fig3.png", width = 6, height = 4, dpi = 300)

#






# PLOT TIME 2 ==============================================================
# This generated plots based on SITE-YEAR
#load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/nload.Rda")  

setwd("~/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Water_Figs")

# calculate number for the draft
# MANAGE db Site Years
nload %>%
  filter(Project == "MANAGE", DRAINAGE == "free") %>%
  group_by(SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD)) %>% 
  dim() %>% .[1]
# MANAGE db # of Site
nload %>%
  filter(Project == "MANAGE", DRAINAGE == "free") %>%
  group_by(SITE) %>%
  count() %>%
  dim() %>% .[1]
# CSCAP db Site Years
nload %>%
  filter(Project == "CSCAP", DRAINAGE == "free") %>%
  group_by(SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD)) %>% 
  dim() %>% .[1]
# CSCAP db # of Site
nload %>%
  filter(Project == "CSCAP", DRAINAGE == "free") %>%
  group_by(SITE) %>%
  count() %>%
  dim() %>% .[1]
# Median of CSCAP and MANAGE
nload %>%
  filter(DRAINAGE == "free") %>%
  group_by(Project) %>%
  summarise(MEDIAN = median(LOAD, na.rm = TRUE))
# Alternative Median
nload %>%
  filter(DRAINAGE == "free") %>%
  group_by(SITE, YEAR, Project) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project) %>%
  summarise(MEDIAN = median(LOAD, na.rm = TRUE))  
# MANAGE db Site Years
nload %>%
  filter(Project == "MANAGE", DRAINAGE == "free") %>%
  group_by(SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD)) %>% 
  filter(LOAD > 60) 
# MANAGE db Site Years
nload %>%
  filter(!is.na(YEAR), Project == "MANAGE") %>%
  filter(YEAR %in% c(max(YEAR), min(YEAR)))
  


# get Cumulative Frequency Distribution function of CSCAP Free Drainage
nload %>%
  filter(DRAINAGE == "free",
         Project == "CSCAP") %>%
  group_by(SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  ungroup() %>%
  select(LOAD) %>% 
  collect() %>%
  .[["LOAD"]] %>%
  ecdf() -> cscap_free_ecdf_2

data.frame(regions = c("Iowa", "Indiana", "Minnesota"),
           max_loads = c(17, NA, 7), 
           y_text = rep(0.99, 3)) %>%
  mutate(CF_free = cscap_free_ecdf_2(max_loads)) %>%
  mutate(CF_free = round(CF_free, digits = 3))-> vlines_2

vlines_2 %>% filter(!is.na(max_loads)) -> vlines_2


# FIG.1 - Cummulative frequency of N loads 
nload %>%
  mutate(YEAR = ifelse(is.na(YEAR), 2020, YEAR)) %>%
  filter(DRAINAGE == "free") %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  #group_by(Project) %>% count()
  ggplot(aes(x=LOAD, y=CF, colour=Project, linetype=Project)) +
  geom_line(size = 1.5) +
  #geom_point(size = 3) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_color_manual(values = c("#1f78b4", "#ffd19d", "#d95f02","#4D4325")) +  
  #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines_2$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +
  
  geom_vline(xintercept = vlines_2$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines_2, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  geom_hline(yintercept = vlines_2$CF_free, colour = "gray40", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines_2, mapping = aes(x=0, y=CF_free, label = paste(CF_free*100, "%")),
            size = 4, color = "gray40", vjust = -0.25, hjust = 0.25) +
  
  #scale_y_continuous(breaks = sort(c(seq(0, 1, 0.25), vlines_2$CF_free)), labels = percent) +
  
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.8, 0.25),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  # geom_segment(data = vlines_2 %>% filter(!is.na(max_loads)), 
  #              aes(x = 0, y = CF_free, xend = max_loads, yend = CF_free),
  #              inherit.aes = F, 
  #              colour = "gray40",
  #              linetype = "dashed") +
  
  geom_segment(inherit.aes = FALSE, 
               data = vlines_2 %>% filter(!is.na(max_loads)),
               aes(x = (max_loads+ 0.5), y = CF_free, xend = 140, yend = CF_free), 
               colour = "white") +
  
  coord_cartesian(xlim = c(0, 100)) +
  
  ggsave(filename = "figure1.png", width = 6, height = 4, dpi = 300)



# FIG.2 - Boxplots of N load 
nload %>%
  filter(DRAINAGE == "free", !is.na(YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  ggplot(aes(x=YEAR, y=LOAD, group=interaction(YEAR, Project), fill=Project)) +
  geom_boxplot() +
  geom_hline(yintercept = vlines_2$max_loads, 
             size = 1, colour = "darkred", alpha = 0.85, linetype = "dashed") +
  scale_fill_manual(values = c("#1f78b4", "#ffd19d","#4D4325")) + 
  #  scale_fill_manual(values = c("#1f78b4", "#d95f02","#4D4325")) + 
  scale_y_continuous(breaks = sort(c(seq(0, 100, 50), vlines_2$max_loads))) +
  labs(x = "Year", 
       y = "Nitrate-N Load (kg/ha)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.75, 0.85),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  ggsave(filename = "figure2.png", width = 6, height = 4, dpi = 300)





# FIG.3 - Cummulative frequency of N loads 
nload %>%
  mutate(YEAR = ifelse(is.na(YEAR), 2020, YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, DRAINAGE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project, DRAINAGE) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  #group_by(Project) %>% count()
  ggplot(aes(x=LOAD, y=CF, 
             colour=interaction(Project, DRAINAGE), 
             linetype=interaction(Project, DRAINAGE))) +
  geom_line(size = 1.5) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted"),
                        labels = c("Controlled Drainage - CSCAP", 
                                   "Free Drainage - CSCAP", 
                                   "Free Drainage - MANAGE")) +
  scale_color_manual(values = c("#ACCD42", "#1f78b4", "#ffd19d", "#d95f02","#4D4325"),
                     labels = c("Controlled Drainage - CSCAP", 
                                "Free Drainage - CSCAP", 
                                "Free Drainage - MANAGE")) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines_2$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +
  
  geom_vline(xintercept = vlines_2$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines_2, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.65, 0.2),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  coord_cartesian(xlim = c(0, 100)) +

  ggsave(filename = "figure3.png", width = 6, height = 4, dpi = 300)


# FIG.3a - Cummulative frequency of N loads (max = 50 kg/ha)
nload %>%
  mutate(YEAR = ifelse(is.na(YEAR), 2020, YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  #filter(SITE != "GILMORE") %>%
  group_by(Project, SITE, DRAINAGE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(Project, DRAINAGE) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  #group_by(Project) %>% count()
  ggplot(aes(x=LOAD, y=CF, 
             colour=interaction(Project, DRAINAGE), 
             linetype=interaction(Project, DRAINAGE))) +
  geom_line(size = 1.5) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted"),
                        labels = c("Controlled Drainage - CSCAP", 
                                   "Free Drainage - CSCAP", 
                                   "Free Drainage - MANAGE")) +
  scale_color_manual(values = c("#ACCD42", "#1f78b4", "#ffd19d", "#d95f02","#4D4325"),
                     labels = c("Controlled Drainage - CSCAP", 
                                "Free Drainage - CSCAP", 
                                "Free Drainage - MANAGE")) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines_2$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +
  
  geom_vline(xintercept = vlines_2$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines_2, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  theme_light() + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.7, 0.2),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  coord_cartesian(xlim = c(1, 50)) +
  
  ggsave(filename = "figure3a.png", width = 8, height = 5, dpi = 300)


# VERSION 2 - 2017-05-12 ==================================================
# encorporating Jane's requests sent in 2017-05-11

# FIG.2 - Boxplots of N load 
# Jane's request: (a) show only 10 years (2006-2015) 
# Gio's idea:     (b) combine CAP and MANAGE data
nload %>%
  filter(DRAINAGE == "free", !is.na(YEAR)) %>%
  filter(!is.na(LOAD)) %>%
  filter(between(YEAR, 2006, 2015)) %>%
  mutate(YEAR = factor(YEAR, levels = sort(unique(YEAR)))) %>%
  group_by(Project, SITE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  # ggplot(aes(x=YEAR, y=LOAD, group=interaction(YEAR, Project), fill=Project)) +
  # geom_boxplot() +
  ggplot(aes(x=YEAR, y=LOAD, group=YEAR, fill = "#1f78b4")) +
  geom_boxplot(show.legend = FALSE) +
  geom_hline(yintercept = vlines_2$max_loads,
             size = 1, colour = "darkred", alpha = 0.85, linetype = "dashed") +
  scale_fill_manual(values = c("#1f78b4", "#ffd19d","#4D4325")) + 
  scale_y_continuous(breaks = sort(c(seq(0, 100, 50), vlines_2$max_loads))) +
  labs(x = "Year", 
       y = "Nitrate-N Load (kg/ha)") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.ticks = element_line(size = 1),
        legend.position = c(0.75, 0.85),
        legend.key.size = unit(1.5, 'lines'),
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +

  # ggsave(filename = "figure2.png", width = 6, height = 4, dpi = 300)  
  ggsave(filename = "figure2b.png", width = 6, height = 4, dpi = 300)




# FIG.3a - Cummulative frequency of N loads (max = 50 kg/ha)
# Jane's request: (a) remove MANAGE, (b) show years with symbols
nload %>%
  filter(Project == "CSCAP") %>%
  filter(!is.na(LOAD)) %>%
  group_by(SITE, DRAINAGE, YEAR) %>%
  summarise(LOAD = mean(LOAD, na.rm = TRUE)) %>%
  group_by(DRAINAGE) %>%
  mutate(CF = ecdf(LOAD)(LOAD)) %>% 
  ungroup() %>% 
  mutate(YEAR = factor(YEAR, ordered = is.ordered(YEAR))) %>%
  ggplot(aes(x=LOAD, y=CF, colour=DRAINAGE)) +
  geom_point(aes(shape = YEAR), size = 4) +
  geom_line(size = 1.5) +
  scale_color_manual("Drainage", 
                     values = c("#ACCD42", "#1f78b4", "#ffd19d", "#d95f02","#4D4325"),
                     labels = c("Controlled", "Free")) +
  scale_shape_manual("Year", 
                     values = c(15, 0, 19, 1, 17)) +
  scale_x_continuous(breaks =sort(c(seq(0, 100, 25), vlines_2$max_loads))) +
  scale_y_continuous(labels = percent) +
  labs(x = "Nitrate-N Load (kg/ha)",
       y = "Cumulative Frequency (%)") +
  
  geom_vline(xintercept = vlines_2$max_loads, size = 1, colour = "gray20", alpha = 0.5, 
             linetype = "dashed") +
  geom_text(inherit.aes = F, data=vlines_2, mapping = aes(x=max_loads, y=y_text, label = regions),
            size = 6, fontface = "bold", color = "gray20", angle = 90, hjust = 1, vjust = -0.4) +
  
  theme_light() + 
  guides(color = guide_legend(order = 0),
         shape = guide_legend(order = 1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.ticks = element_line(size = 1),
        # # legend.position = "bottom", 
        # # legend.direction = "horizontal",
        # # legend.box = "vertical",
        # legend.position = "right",
        legend.position = c(0.95, 0.35),
        legend.justification = c("right"),
        legend.box.just = "left",
        legend.key.size = unit(1.3, 'lines'),
        legend.title = element_text(size = 16, color = "gray20"),
        legend.text = element_text(size = 14, color = "gray20"),
        legend.background = element_rect(fill = "transparent", colour = "transparent")) +
  
  coord_cartesian(xlim = c(1, 50)) +
  
  #ggsave(filename = "figure3a.png", width = 6, height = 4, dpi = 300)
  ggsave(filename = "figure3a.png", width = 8, height = 5, dpi = 300)

  
#


