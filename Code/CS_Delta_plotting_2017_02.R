library(tidyverse)
library(lubridate)
library(scales)
library(stringr)
library(ggthemes)

load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/CS40.RData")

new_dir <- paste0(getwd(), "/GitHub/CSCAP/Sustainable_Corn_Paper/Figs/Soil_Figs/", Sys.Date())
dir.create(new_dir)
setwd(new_dir)



# COVER vs NO COVER
CS40 %>%
  filter(crot != "CR07") %>%
  group_by(site, fly, crot_name, crot) %>%
  summarise(CS = mean(CS)) %>%
  mutate(crop = ifelse(crot %in% c("CR01", "CR03"), "C_CORN", "CORN_SOY")) %>%
  mutate(cover = ifelse(crot %in% c("CR01", "CR05"), "NO_COVER", "WITH_COVER")) %>%
  ungroup() %>%
  select(-c(crot, crot_name)) %>%
  spread(key = cover, value = CS) %>%
  filter(!is.na(WITH_COVER)) -> CScover
CScover %>%
  gather(key = COVER, value = SOC, 4:5) %>%
  ggplot(aes(x=fly, y=SOC, group = COVER, fill = COVER)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average Carbon Sotcks by Crop Rotation\nw/ and w/o Cover Crop") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CScover01.png", width = 10, height = 8)
CScover %>%
  mutate(DIFF = WITH_COVER - NO_COVER) %>%
  ggplot(aes(x=fly, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() + 
  ggtitle("Carbon Stocks Difference Between Crop Rotation\nw/ and w/o Cover Crop") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CScover02.png", width = 10, height = 8)
CScover %>%
  gather(key = COVER, value = SOC, 4:5) %>%
  spread(key = fly, value = SOC) %>%
  mutate(DIFF = last - first) %>%
  ggplot(aes(x=COVER, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Cover Crop") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Carbon Stocks Change \nBetween First and Last Years") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CScover03.png", width = 10, height = 8)
CScover %>%
  gather(key = COVER, value = SOC, 4:5) %>%
  ggplot(aes(x=fly, y=SOC, group = COVER)) +
  geom_boxplot() +
  geom_text(aes(label = site)) +
  theme_light()





# TILL vs NO TILL
CS40 %>%
  group_by(site, fly, tillage) %>%
  summarize(CS = mean(CS)) %>%
  ungroup() %>%
  spread(key = tillage, value = CS) %>%
  filter(!is.na(CT)) %>%
  filter(!is.na(NT)) -> CStill
CStill %>%
  gather(key = TILL, value = SOC, 3:4) %>%
  ggplot(aes(x=fly, y=SOC, group = TILL, fill = TILL)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average Carbon Sotcks by Tillage Systems\nNo Till and Conventional Till") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CStill01.png", width = 10, height = 8)
CStill %>% 
  mutate(DIFF = NT - CT) %>%
  ggplot(aes(x=fly, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() + 
  ggtitle("Carbon Stocks Difference Between Tillage Systems\nNo Till and Conventional Till") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CStill02.png", width = 10, height = 8)
CStill %>% 
  gather(key = TILL, value = SOC, 3:4) %>%
  spread(key = fly, value = SOC) %>% 
  mutate(DIFF = last - first) %>%
  ggplot(aes(x=TILL, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Tillage System") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() + 
  ggtitle("Carbon Stocks Change \nBetween First and Last Years") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CStill03.png", width = 10, height = 8)
CStill %>%
  gather(key = TILL, value = SOC, 3:4) %>%
  ggplot(aes(x=fly, y=SOC, group = TILL)) +
  geom_boxplot() +
  theme_light()

  


# FREE DRAINGE vs CONTROLED
CS40 %>%
  filter(!is.na(drainage)) %>%
  group_by(site, fly, drainage) %>%
  summarize(CS = mean(CS)) %>%
  ungroup() %>%
  filter(drainage %in% c("DWM2", "DWM3")) %>%
  mutate(DWM = ifelse(drainage == "DWM2", "free", "controlled")) %>%
  select(-drainage) %>%
  spread(key = DWM, value = CS) %>%
  filter(site != "BRADFORD.A") -> CSdrain
CSdrain %>%
  gather(key = DWM, value = SOC, 3:4) %>%
  ggplot(aes(x=fly, y=SOC, fill = DWM)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average Carbon Sotcks by Drainage Systems\nFree Drainage and Controlled Drainage") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSdrain01.png", width = 10, height = 8) 
CSdrain %>% 
  mutate(DIFF = controlled - free) %>%
  ggplot(aes(x=fly, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Carbon Sotcks Difference Between Drainage Systems\nFree Drainage and Controlled Drainage") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSdrain02.png", width = 10, height = 8) 
CSdrain %>% 
  gather(key = DWM, value = SOC, 3:4) %>%
  spread(key = fly, value = SOC) %>% 
  mutate(DIFF = last - first) %>%
  ggplot(aes(x=DWM, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Drainage Type") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Carbon Stocks Change \nBetween First and Last Years") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSdrain03.png", width = 10, height = 8) 
CSdrain %>%
  gather(key = DWM, value = SOC, 3:4) %>%
  ggplot(aes(x=fly, y=SOC, group = DWM)) +
  geom_boxplot() +
  #geom_jitter(width = 0.1) +
  theme_light()
  
  
# ROTATIONS
CS40 %>%
  filter(crot %in% c("CR01", "CR05", "CR07")) %>%
  group_by(site, fly, crot_name) %>%
  summarise(CS = mean(CS)) %>%
  group_by(site) %>%
  mutate(count = sum(!is.na(CS))/2) %>%
  filter(count > 1) %>%
  ungroup() %>%
  select(-count) -> CSrot
CSrot %>%
  ggplot(aes(x=fly, y=CS, fill = crot_name)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average Carbon Sotcks by Crop Rotations") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSrot01.png", width = 10, height = 8) 
CSrot %>%
  spread(key = crot_name, value = CS) %>%
  mutate(`CC-CS` = `continuous CORN` - `CORN-SOY`) %>%
  mutate(`CSW-CS` = `CORN-SOY-WHEAT` - `CORN-SOY`) %>%
  select(site, fly, `CC-CS`, `CSW-CS`) %>%
  gather(key = `Difference between`, value = CS, 3:4) %>%
  filter(!is.na(CS)) %>%
  ggplot(aes(x=fly, y=CS, fill = `Difference between`)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Data Year") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Carbon Sotcks Difference\nBetween Crop Rotations") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSrot02.png", width = 10, height = 8)
CSrot %>%
  spread(key = fly, value = CS) %>%
  mutate(DIFF = last - first) %>%
  ggplot(aes(x=crot_name, y=DIFF)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge") +
  scale_x_discrete(name = "Crop Rotations") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Carbon Stocks Change Between First and Last Years\nBy Crop Rotations") +
  scale_fill_brewer(palette = "Accent") +
  theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14)) +
  ggsave(filename = "CSrot03.png", width = 10, height = 8)


  