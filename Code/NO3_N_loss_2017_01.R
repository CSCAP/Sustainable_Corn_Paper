library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

setwd("C:/Users/Gio/Documents")


load("C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow.RData")


# # STJOHNS NO3-N Loss ========================================================================
# 
# STJ_NO3_N_conc <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/STJOHNS/StJohns_Concentrations_2011-2015.xlsx", 
#                              col_types = c("date", "text", "text", "numeric"))
# 
# names(STJ_NO3_N_conc) <- c("date", "plotid", "grab", "nitrate_conc")
# STJ_NO3_N_conc %>% 
#   filter(!is.na(nitrate_conc)) %>%
#   group_by(plotid, date) %>%
#   summarize(nitrate_conc = mean(nitrate_conc), grab = first(grab)) %>%
#   mutate(date = force_tz(ymd_hms(paste(date, "12:00:00")), tzone = "America/New_York")) -> STJ_NO3_N_conc
# 
# 
# #OlsonNames()
# 
# 
# flow %>% 
#   filter(uniqueid == "STJOHNS") %>% 
#   arrange(plotid, valid) %>% 
#   select(uniqueid, plotid, valid, discharge_mm_qc) %>%
#   mutate(time = with_tz(valid, tzone = "America/New_York")) %>% 
#   mutate(year = year(time)) %>% 
#   left_join(STJ_NO3_N_conc, by = c("plotid" = "plotid", "valid" = "date")) -> stj
# 
# # replace frist na with the first available nitrate concentration value
# stj[stj$plotid=="WN", "nitrate_conc"][1] <- stj[stj$plotid=="WN" & !is.na(stj$nitrate_conc), "nitrate_conc"][1]
# stj[stj$plotid=="WS", "nitrate_conc"][1] <- stj[stj$plotid=="WS" & !is.na(stj$nitrate_conc), "nitrate_conc"][1]
# 
# stj <- stj[ , c("uniqueid", "plotid", "year", "time", "discharge_mm_qc", "nitrate_conc", "grab", "valid")]
# 
# 
# stj_wn <- stj[stj$plotid == "WN", ]
# stj_ws <- stj[stj$plotid == "WS", ]
#   
#   
# zoo(stj_wn$nitrate_conc, stj_wn$valid) %>%
#   na.approx(na.rm = FALSE) %>%
#   na.locf() %>%
#   as.data.frame() -> no3wn
# no3wn$valid <- ymd_hms(rownames(no3wn))
# no3wn$plotid <- "WN"
# rownames(no3wn) <- 1:dim(no3wn)[1]
# 
# zoo(stj_ws$nitrate_conc, stj_ws$valid) %>%
#   na.approx(na.rm = FALSE) %>% 
#   na.locf() %>% 
#   as.data.frame() -> no3ws
# no3ws$valid <- ymd_hms(rownames(no3ws))
# no3ws$plotid <- "WS"
# rownames(no3ws) <- 1:dim(no3ws)[1]
# 
# rbind(no3wn, no3ws) -> no3
# names(no3)[1] <- "no3_conc"
# 
# stj %>%
#   left_join(no3, by = c("plotid" = "plotid", "valid" = "valid")) -> stj
# 
# #calculate NO3-N loss in kg/ha
# stj %>% 
#   mutate(area = ifelse(plotid == "WN", 7.16, 7.28)) %>%
#   mutate(no3_loss = discharge_mm_qc * 10000 * no3_conc / 10^6) %>%            #to convert to gallons (* 0.264172 * area)
#   group_by(plotid, year) %>%
#   summarise(loss = sum(no3_loss, na.rm = TRUE)) -> stj_loss
#  
# 
# 
# # DPAC NO3-N Loss ===========================================================
# DPAC_NO3_N_conc <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/DPAC/DPAC Water Quality Data 2017.xlsx", 
#                              col_types = c("text", "date", "text", "numeric", "numeric", "numeric"))
# 
# names(DPAC_NO3_N_conc) <- c ("plotid", "date", "grab", "nitrate_conc", "ortho_P", "total_P")
# DPAC_NO3_N_conc %>% 
#   select(1:4) %>%
#   filter(!is.na(nitrate_conc)) %>%
#   group_by(plotid, date) %>%
#   summarize(nitrate_conc = mean(nitrate_conc), grab = first(grab)) %>%
#   mutate(date = force_tz(ymd_hms(paste(date, "12:00:00")), tzone = "America/New_York")) -> DPAC_NO3_N_conc
# 
# 
# #OlsonNames()
# 
# 
# flow %>% 
#   filter(uniqueid == "DPAC") %>% 
#   arrange(plotid, valid) %>%
#   select(uniqueid, plotid, valid, discharge_mm_qc) %>%
#   mutate(time = with_tz(valid, tzone = "America/New_York")) %>%
#   mutate(year = year(time)) %>% 
#   left_join(DPAC_NO3_N_conc, by = c("plotid" = "plotid", "valid" = "date")) -> dpac
# 
# 
# 
# # extract no3-n conc data that was measured before flow measurement start 
# # (i.e. the last 2011 no3-n data to be used for interpolation from 2012)
# DPAC_NO3_N_conc %>% 
#   mutate(year = year(date)) %>%
#   filter(year == 2011) %>%
#   group_by(plotid) %>%
#   filter(date == max(date)) -> dpac_no3_n_2011
# names(dpac_no3_n_2011)[which(names(dpac_no3_n_2011) == "date")] <- "time"
#  
# dpac %>%
#   bind_rows(dpac_no3_n_2011) %>%
#   arrange(plotid, time) -> dpac
# 
# 
# dpac <- dpac[ , c("uniqueid", "plotid", "year", "time", "discharge_mm_qc", "nitrate_conc", "grab", "valid")]
# 
# dpac %>%
#   arrange(plotid, desc(time)) %>%
#   group_by(plotid) %>%
#   mutate(new_grab = na.locf(grab, na.rm = F)) %>%
#   mutate(no3_conc = ifelse(new_grab == "C", na.locf(nitrate_conc, na.rm = FALSE), nitrate_conc)) %>%
#   arrange(plotid, time) %>%
#   mutate(no3_conc = na.approx(no3_conc, na.rm = FALSE)) %>%
#   mutate(no3_conc = na.locf(no3_conc, na.rm = FALSE)) -> dpac
#   
# 
# #calculate NO3-N loss in kg/ha
# dpac %>% 
#   mutate(no3_loss = discharge_mm_qc * 10000 * no3_conc / 10^6) %>%            #to convert to gallons (* 0.264172 * area)
#   group_by(plotid, year) %>%
#   summarise(loss = sum(no3_loss, na.rm = TRUE)) %>%
#   filter(year != 2011) -> dpac_loss
# 
# 
# 
# 

# MANAGE db =====================================================
manage_drain <-
  read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/MANAGE DB/manage2016/drain_load_short_10.2015.xlsx")

manage_drain %>%
  select(RefID, Year, `Watershed ID`, State,
         `Drainage type`, `Drain depth (m)`, `Drain spacing (m)`,
         `Land Use`, Tillage, `Dominant Soil Type`,
         `Crop 1`, `Avg Crop 1 Yield (Mg/ha)`, `Crop 2`, `Avg Crop 2 Yield (Mg/ha)`,
         `Avg Precipitation (mm)`, `Drainage Discharge (mm)`,
         `Avg Soil Loss (kg/ha)`, `Avg Dissolved N (kg/ha)`, `Avg Dissolved P (kg/ha)`, `Avg Total P (kg/ha)`,
         Comments) %>%
  filter(!is.na(`Avg Dissolved N (kg/ha)`)) %>%                # select entries with NO3-N load data
  filter(grepl("corn", `Land Use`, ignore.case = TRUE)) %>%    # select rotations with corn
  filter(!RefID %in% c(52, 80, 95)) %>%                        # remove papers with repetitive NO3-N load data
  mutate(ID = as.factor(paste(RefID, `Watershed ID`, sep = "_")),
         state = as.factor(State),
         siteID = as.factor(RefID),
         plotID = as.factor(`Watershed ID`),
         year = as.numeric(Year),
         rotation = `Land Use`,
         tillage = Tillage,
         soil_type = `Dominant Soil Type`,
         drainage_type = as.factor(`Drainage type`),
         drainage_spacing = as.numeric(`Drain spacing (m)`),
         drainage_depth = as.numeric(`Drain depth (m)`),
         #N_applied = as.numeric(`Avg N Applied (kg/ha)`),
         #P_applied = as.numeric(`Avg P Applied (kg/ha)`),
         crop_1 = as.factor(str_to_lower(`Crop 1`)),
         crop_yield_1 = as.numeric(`Avg Crop 1 Yield (Mg/ha)`),
         crop_2 = as.factor(str_to_lower(`Crop 2`)),
         crop_yield_2 = as.numeric(`Avg Crop 2 Yield (Mg/ha)`),
         precip_mm = as.double(`Avg Precipitation (mm)`),
         drainage_mm = as.double(`Drainage Discharge (mm)`),
         soil_loss = as.double(`Avg Soil Loss (kg/ha)`),
         N_load = as.double(`Avg Dissolved N (kg/ha)`),
         #N_form = `Form Dissolved N`,
         P_load = as.double(`Avg Dissolved P (kg/ha)`),
         #P_form = `Form Dissolved P`,
         #sample_timing = `Sample Collection Timing`,
         comments = Comments) %>%
  select(ID:comments) -> manage_NO3_N



manage_NO3_N %>%
  ggplot(aes(x=drainage_type, y=N_load)) +
  geom_boxplot(na.rm = TRUE) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, color = "blue", shape = 3, stroke = 1.5) +
  scale_x_discrete(name = "Drainage Type", labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(name = "Nitrate-N Load, kg/ha", labels = comma) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))



# ANALYZE THIS =============================================================================
library(readxl)

setwd("~/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Water_Figs")

NO3_N_Loss <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/NO3-N Loss.xlsx", sheet = "Sheet1", 
                         col_types = c("text", "text", "text", "text", "numeric", "numeric"))


NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  mutate(outlier = ifelse(Treatment == "cont. drainage" & `NO3-N Loss (kg/ha)` > 25 |
                          Treatment == "free drainage"  & `NO3-N Loss (kg/ha)` > 100, 
                          paste(SITE, PLOT, YEAR), NA)) %>%
  group_by(Treatment) %>%
  mutate(number = n()) %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`)) +
  geom_boxplot() + 
  #geom_text(aes(y = 135, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  geom_text(aes(label = outlier, colour = I("grey50")), vjust = 0.5, hjust = -0.1) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Nitrate-N Loss") +
  theme(#panel.grid.minor = element_blank(),
        #panel.grid.major.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text = element_text(size = 12, vjust = 0.5))
ggsave(filename = "no3n_01.png", width = 12, height = 8, dpi = 300)


NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  #filter(YEAR > 2010) %>%
  filter(Treatment != "shallow drainage") %>%
  group_by(Treatment) %>%
  mutate(number = n()) %>%
  mutate(se = sd(`NO3-N Loss (kg/ha)`)/sqrt(number)) %>%
  mutate(mean = mean(`NO3-N Loss (kg/ha)`)) %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  geom_text(aes(y = 35, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Average Nitrate-N Loss") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text = element_text(size = 12, vjust = 0.5)) +
  coord_cartesian(ylim = c(0, 35)) +
  scale_fill_brewer()
ggsave(filename = "no3n_02.png", width = 12, height = 8, dpi = 300)


NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(YEAR > 2010) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  group_by(Treatment, YEAR) %>%
  mutate(number = n()) %>%
  mutate(se = sd(`NO3-N Loss (kg/ha)`)/sqrt(number)) %>%
  mutate(mean = mean(`NO3-N Loss (kg/ha)`)) %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  facet_grid( ~ YEAR) +
  #geom_text(aes(y = 50, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 50))
ggsave(filename = "no3n_03_5years.png", width = 12, height = 8, dpi = 300)



NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(YEAR > 2010) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_boxplot() + 
  facet_grid( ~ YEAR) +
  #geom_text(aes(y = 50, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer()
ggsave(filename = "no3n_04_5years.png", width = 12, height = 8, dpi = 300)




NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(YEAR > 2010) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  facet_grid(SITE ~ YEAR) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss by Sites") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "no3n_05_5years.png", width = 12, height = 8, dpi = 300)

NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  group_by(Treatment, YEAR) %>%
  mutate(number = n()) %>%
  mutate(se = sd(`NO3-N Loss (kg/ha)`)/sqrt(number)) %>%
  mutate(mean = mean(`NO3-N Loss (kg/ha)`)) %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  facet_grid( ~ YEAR) +
  #geom_text(aes(y = 50, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 50))
ggsave(filename = "no3n_03_l.png", width = 12, height = 8, dpi = 300)



NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_boxplot() + 
  facet_grid( ~ YEAR) +
  #geom_text(aes(y = 50, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer()
ggsave(filename = "no3n_04_l.png", width = 12, height = 8, dpi = 300)




NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  facet_grid(SITE ~ YEAR) +
  scale_x_discrete(name ="") +
  theme_light() +
  ggtitle("Annual Nitrate-N Loss by Sites") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer(palette = "Blues")
ggsave(filename = "no3n_05_l.png", width = 12, height = 8, dpi = 300)




NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(YEAR > 2010) %>%
  filter(SITE != "WATERMAN") %>%
  filter(Treatment != "shallow drainage") %>%
  group_by(Treatment, SITE) %>%
  mutate(number = n()) %>%
  mutate(se = sd(`NO3-N Loss (kg/ha)`)/sqrt(number)) %>%
  mutate(mean = mean(`NO3-N Loss (kg/ha)`)) %>%
  ggplot(aes(x = SITE, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1, position = position_dodge(.9)) +
  geom_text(aes(y = 45, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  theme_light() +
  xlab("") +
  ggtitle("Annual Nitrate-N Load by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(size = 12, vjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 12, vjust = 0.5)) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 45))
ggsave(filename = "no3n_06_5years.png", width = 12, height = 8, dpi = 300)



NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(YEAR > 2010) %>%
  filter(SITE != "WATERMAN") %>%
  filter(SITE != "GILMORE") %>%
  filter(Treatment != "shallow drainage") %>%
  group_by(Treatment, YEAR) %>%
  mutate(number = n()) %>%
  mutate(se = sd(`NO3-N Loss (kg/ha)`)/sqrt(number)) %>%
  mutate(mean = mean(`NO3-N Loss (kg/ha)`)) %>%
  ggplot(aes(x = Treatment, y = `NO3-N Loss (kg/ha)`, fill = Treatment)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  facet_grid( ~ YEAR) +
  #geom_text(aes(y = 50, label = number, colour = I("grey60")), vjust = 1, size = 7) +
  scale_x_discrete(name ="") +
  theme_light() +
  labs(title = "Annual Nitrate-N Loss", subtitle = "without GILMORE data") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.y = element_text(size = 12, vjust = 0.5),
        axis.text.x = element_blank()) +
  scale_fill_brewer() +
  coord_cartesian(ylim = c(0, 50))
ggsave(filename = "no3n_07_5years.png", width = 12, height = 8, dpi = 300)



# MERGE TWO DATA ==============================================================
manage_NO3_N %>%
  select(state, siteID, plotID, 
         drainage_type, year, N_load) %>%
  mutate(project = "MANAGE") -> manage

NO3_N_Loss %>%
  filter(complete.cases(.)) %>%
  filter(SITE != "WATERMAN") %>%
  mutate(project = "CSCAP") -> cscap

names(manage) -> names(cscap)

bind_rows(cscap, manage) %>%
  mutate(state = as.factor(state),
         siteID = as.factor(siteID),
         drainage_type = as.factor(drainage_type),
         project = as.factor(project)) -> mcap

#save(mcap, file = "~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/cap_manage.Rda")
#load("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/cap_manage.Rda")  

mcap %>%
  filter(state %in% c("IA", "MO", "MN", "IN", "OH", "IL")) %>%
  filter(drainage_type %in% c("free drainage", "cont. drainage", "Subsurface (no inlets specified)")) %>%
  ggplot(aes(x=drainage_type, y=N_load, fill = project)) +
  geom_boxplot(na.rm = TRUE) +
  stat_summary(fun.y = "mean", geom = "point", shape = 3, stroke = 1.5, color = "black", size = 3) +
  #stat_summary(fun.y = "mean", geom = "text", label = "------", color = "black", size = 15) +
  #scale_color_brewer(palette = "Set1", direction = -1) +
  scale_fill_brewer() +
  scale_x_discrete(name = "Drainage Type", labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(name = "Nitrate-N Load, kg/ha", labels = comma) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12)) +
  ggtitle("Annual Nitrate-N Loads")
ggsave(filename = "no3n_mcap_1.png", width = 12, height = 8, dpi = 300)


mcap %>%
  group_by(drainage_type, year) %>%
  mutate(number = n()) %>%
  mutate(mean = mean(N_load),
         se = sd(N_load)/sqrt(number)) %>%
  filter(state %in% c("IA", "MO", "MN", "IN", "OH", "IL")) %>%
  filter(drainage_type %in% c("free drainage", "cont. drainage", "Subsurface (no inlets specified)")) %>%
  filter((project == "CSCAP" & year > 2010) | (project == "MANAGE")) %>%
  ggplot(aes(x=year, y=N_load, fill = project)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack", na.rm = T) + 
  facet_grid(drainage_type ~ ., labeller = label_wrap_gen(width = 20)) +
  theme_light() +
  ggtitle("Average Annual NO3-N Load") +
  ylab("Nitrate-N Load, kg/ha") +
  xlab("") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text = element_text(size = 12, vjust = 0.5),
        strip.text = element_text(size = 14, face ="bold")) +
  scale_fill_brewer(palette = "Accent") +
  coord_cartesian(ylim = c(0, 70))
ggsave(filename = "no3n_mcap_2.png", width = 12, height = 8, dpi = 300)


mcap %>% 
  filter(state %in% c("IA", "MO", "MN", "IN", "OH", "IL")) %>%
  filter(drainage_type %in% c("free drainage", "cont. drainage", "Subsurface (no inlets specified)")) %>%
  ggplot(aes(x=N_load, 
             group=interaction(project,drainage_type), 
             color = interaction(project,drainage_type))) + 
  stat_ecdf(na.rm = T, lwd = 1, geom = "line") +
  theme_light() +
  scale_color_brewer(palette = "Accent", labels = c("Controlled Drainage - CSCAP", 
                                                    "Free Drainage - CSCAP", 
                                                    "Free Drainage - MANAGE")) +
  xlab("Nitrate-N Load, kg/ha") +
  ylab("Cummulative Density") +
  labs(color = "", title = "Nitrate-N Load from Ag Fields in Corn Belt Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = c(0.8, 0.2))

ggsave(filename = "no3n_mcap_3.png", width = 10, height = 7, dpi = 300)


library(splines)
library(dplyr)

mcap %>% 
  filter(state %in% c("IA", "MO", "MN", "IN", "OH", "IL")) %>%
  filter(drainage_type %in% c("free drainage", "cont. drainage", "Subsurface (no inlets specified)")) %>%
  filter((project == "CSCAP" & year > 2010) | (project == "MANAGE")) %>%
  filter((project == "CSCAP" & siteID != "BRADFORD.A") | (project == "MANAGE")) %>%   #remove BRADFORD.A
  group_by(interaction(project,drainage_type)) %>%
  mutate(ecd = ecdf(N_load)(N_load)) %>%
  ungroup() %>%
  ggplot(aes(x = N_load, y = ecd, group = drainage_type, colour = drainage_type)) +
  #geom_smooth(se = FALSE, formula = y ~ ns(x, 15), method = "lm", lwd = 1.5) +       #smooth with BRADFORD.A
  geom_line(lwd = 1.5) +
  theme_light() +
  scale_color_brewer(palette = "Accent", labels = c("Controlled Drainage - CSCAP", 
                                                    "Free Drainage - CSCAP", 
                                                    "Free Drainage - MANAGE")) +
  xlab("Nitrate-N Load, kg/ha") +
  ylab("Cummulative Density") +
  labs(color = "", title = "Nitrate-N Load from Ag Fields in Corn Belt Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = c(0.8, 0.2)) -> smooth_mcap
smooth_mcap
#ggsave(filename = "no3n_mcap_3_smooth.png", width = 10, height = 7, dpi = 300)    #figure with BRADFORD.A
ggsave(filename = "no3n_mcap_3_wo_BRADFORD.png", width = 10, height = 7, dpi = 300)    #figure w/o BRADFORD.A

vlines <- data.frame(regions = c("Iowa", "Indiana", "Minnesota"),
                     max_loads = c(17, NA, 7), 
                     y_value = rep(0, 3))

smooth_mcap +
  scale_x_continuous(breaks = sort(c(0, 50, 100, 150, vlines$max_loads))) +
  geom_vline(xintercept = vlines$max_loads, size = 1, colour = "gray40", alpha = 0.5, 
             linetype = "longdash") +
  theme(panel.grid.minor.x = element_blank()) +
  geom_text(inherit.aes = F, data=vlines, mapping = aes(x=max_loads, y=y_value, label = regions),
            size = 4, fontface = "bold", color = "gray40", angle = 90, hjust = 0, vjust = -0.2)
 
#ggsave(filename = "no3n_mcap_3_smooth_vline.png", width = 10, height = 7, dpi = 300)  #figure with BRADFORD.A
ggsave(filename = "no3n_mcap_3_wo_BRADFORD_vline.png", width = 10, height = 7, dpi = 300)  #figure w/o BRADFORD.A


