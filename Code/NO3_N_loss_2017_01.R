library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

setwd("C:/Users/Gio/Documents")


load("C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow.RData")


# STJOHNS NO3-N Loss ========================================================================

STJ_NO3_N_conc <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/STJOHNS/StJohns_Concentrations_2011-2015.xlsx", 
                             col_types = c("date", "text", "text", "numeric"))

names(STJ_NO3_N_conc) <- c("date", "plotid", "grab", "nitrate_conc")
STJ_NO3_N_conc %>% 
  filter(!is.na(nitrate_conc)) %>%
  group_by(plotid, date) %>%
  summarize(nitrate_conc = mean(nitrate_conc), grab = first(grab)) %>%
  mutate(date = force_tz(ymd_hms(paste(date, "12:00:00")), tzone = "America/New_York")) -> STJ_NO3_N_conc


#OlsonNames()


flow %>% 
  filter(uniqueid == "STJOHNS") %>% 
  arrange(plotid, valid) %>% 
  select(uniqueid, plotid, valid, discharge_mm_qc) %>%
  mutate(time = with_tz(valid, tzone = "America/New_York")) %>% 
  mutate(year = year(time)) %>% 
  left_join(STJ_NO3_N_conc, by = c("plotid" = "plotid", "valid" = "date")) -> stj

# replace frist na with the first available nitrate concentration value
stj[stj$plotid=="WN", "nitrate_conc"][1] <- stj[stj$plotid=="WN" & !is.na(stj$nitrate_conc), "nitrate_conc"][1]
stj[stj$plotid=="WS", "nitrate_conc"][1] <- stj[stj$plotid=="WS" & !is.na(stj$nitrate_conc), "nitrate_conc"][1]

stj <- stj[ , c("uniqueid", "plotid", "year", "time", "discharge_mm_qc", "nitrate_conc", "grab", "valid")]


stj_wn <- stj[stj$plotid == "WN", ]
stj_ws <- stj[stj$plotid == "WS", ]
  
  
zoo(stj_wn$nitrate_conc, stj_wn$valid) %>%
  na.approx(na.rm = FALSE) %>%
  na.locf() %>%
  as.data.frame() -> no3wn
no3wn$valid <- ymd_hms(rownames(no3wn))
no3wn$plotid <- "WN"
rownames(no3wn) <- 1:dim(no3wn)[1]

zoo(stj_ws$nitrate_conc, stj_ws$valid) %>%
  na.approx(na.rm = FALSE) %>% 
  na.locf() %>% 
  as.data.frame() -> no3ws
no3ws$valid <- ymd_hms(rownames(no3ws))
no3ws$plotid <- "WS"
rownames(no3ws) <- 1:dim(no3ws)[1]

rbind(no3wn, no3ws) -> no3
names(no3)[1] <- "no3_conc"

stj %>%
  left_join(no3, by = c("plotid" = "plotid", "valid" = "valid")) -> stj

#calculate NO3-N loss in kg/ha
stj %>% 
  mutate(area = ifelse(plotid == "WN", 7.16, 7.28)) %>%
  mutate(no3_loss = discharge_mm_qc * 10000 * no3_conc / 10^6) %>%            #to convert to gallons (* 0.264172 * area)
  group_by(plotid, year) %>%
  summarise(loss = sum(no3_loss, na.rm = TRUE)) -> stj_loss
 


# DPAC NO3-N Loss ===========================================================
DPAC_NO3_N_conc <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/DPAC/DPAC Water Quality Data 2017.xlsx", 
                             col_types = c("text", "date", "text", "numeric", "numeric", "numeric"))

names(DPAC_NO3_N_conc) <- c ("plotid", "date", "grab", "nitrate_conc", "ortho_P", "total_P")
DPAC_NO3_N_conc %>% 
  select(1:4) %>%
  filter(!is.na(nitrate_conc)) %>%
  group_by(plotid, date) %>%
  summarize(nitrate_conc = mean(nitrate_conc), grab = first(grab)) %>%
  mutate(date = force_tz(ymd_hms(paste(date, "12:00:00")), tzone = "America/New_York")) -> DPAC_NO3_N_conc


#OlsonNames()


flow %>% 
  filter(uniqueid == "DPAC") %>% 
  arrange(plotid, valid) %>%
  select(uniqueid, plotid, valid, discharge_mm_qc) %>%
  mutate(time = with_tz(valid, tzone = "America/New_York")) %>%
  mutate(year = year(time)) %>% 
  left_join(DPAC_NO3_N_conc, by = c("plotid" = "plotid", "valid" = "date")) -> dpac



# extract no3-n conc data that was measured before flow measurement start 
# (i.e. the last 2011 no3-n data to be used for interpolation from 2012)
DPAC_NO3_N_conc %>% 
  mutate(year = year(date)) %>%
  filter(year == 2011) %>%
  group_by(plotid) %>%
  filter(date == max(date)) -> dpac_no3_n_2011
names(dpac_no3_n_2011)[which(names(dpac_no3_n_2011) == "date")] <- "time"
 
dpac %>%
  bind_rows(dpac_no3_n_2011) %>%
  arrange(plotid, time) -> dpac


dpac <- dpac[ , c("uniqueid", "plotid", "year", "time", "discharge_mm_qc", "nitrate_conc", "grab", "valid")]

dpac %>%
  arrange(plotid, desc(time)) %>%
  group_by(plotid) %>%
  mutate(new_grab = na.locf(grab, na.rm = F)) %>%
  mutate(no3_conc = ifelse(new_grab == "C", na.locf(nitrate_conc, na.rm = FALSE), nitrate_conc)) %>%
  arrange(plotid, time) %>%
  mutate(no3_conc = na.approx(no3_conc, na.rm = FALSE)) %>%
  mutate(no3_conc = na.locf(no3_conc, na.rm = FALSE)) -> dpac
  

#calculate NO3-N loss in kg/ha
dpac %>% 
  mutate(no3_loss = discharge_mm_qc * 10000 * no3_conc / 10^6) %>%            #to convert to gallons (* 0.264172 * area)
  group_by(plotid, year) %>%
  summarise(loss = sum(no3_loss, na.rm = TRUE)) %>%
  filter(year != 2011) -> dpac_loss




# ANALYZE THIS =============================================================================
library(readxl)

setwd("~/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Water_Figs")

NO3_N_Loss <- read_excel("~/GitHub/CSCAP/Sustainable_Corn_Paper/Data/flow/NO3-N Loss.xlsx", sheet = "Data", 
                         col_types = c("text", "text", "text", "numeric", "numeric", "blank", "blank"))


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




















