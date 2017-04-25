library(tidyverse)
library(scales)
library(stringr)



# load Carbn Stock data for seleted plots
load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/CS20.RData")
load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/CS40.RData")
load(file = "C:/Users/Gio/Documents/GitHub/CSCAP/Sustainable_Corn_Paper/Data/CS60.RData")


# TILLAGE effect on CS change ---------------
# CS20 %>%
#   select(-c(Latitude, Longitude, County, `Ave Plot Size (ha)`, rotation, layer)) %>%
#   filter(!is.na(tillage)) %>%
#   mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) -> cs

CS40 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  filter(!is.na(tillage)) -> cs

CS60 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  filter(!is.na(tillage)) %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) -> cs


cs %>%
  select(site, tillage, CS) %>%
  group_by(site, tillage) %>%
  summarise(CS = mean(CS)) %>%
  summarize(count = n()) %>%
  filter(count > 1) %>%
  select(site) %>%
  as.data.frame() -> my_sites

  
# PLOT1
cs %>%
  filter(site %in% my_sites$site) %>%    # paired sites only
  ggplot(aes(x = CS, group = tillage, colour = tillage)) +
  stat_ecdf(na.rm = TRUE, lwd = 1.5, geom = "line") +
  theme_light()



cs %>%
  filter(site %in% my_sites$site) %>%    # paired sites only
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first"),
         year_diff = max(year)-min(year)) %>%
  ungroup() %>%
  select(-c(year)) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first,
         CS_diff_yearly = CS_diff/year_diff) %>%
  group_by(tillage) %>%
  mutate(ecd = ecdf(CS_diff)(CS_diff),
         ecd_yearly = ecdf(CS_diff_yearly)(CS_diff_yearly)) %>%
  ungroup() %>% 
  # select(site, plotid, crot_name, tillage, CS_diff, ecd) %>%   
  # filter(ecd > 0.7 & CS_diff < 5000) -> temp
  ggplot(aes(x = CS_diff, group = tillage, colour = tillage)) +
  stat_ecdf(na.rm = TRUE, lwd = 1.5, geom = "line") +
  theme_light() +
  geom_vline(xintercept = 0)


# cs %>%
#   #filter(site %in% my_sites$site) %>%   # paired sites only
#   group_by(site, plotid) %>%
#   mutate(fly = ifelse(year == max(year), "last", "first")) %>%
#   ungroup() %>%
#   select(-c(year, drainage, nitrogen)) %>%
#   spread(fly, CS) %>%
#   mutate(CS_diff = last - first) %>%
#   group_by(site, tillage) %>%
#   summarise(CS_diff = mean(CS_diff)) %>%
#   group_by(tillage) %>%
#   mutate(ecd = ecdf(CS_diff)(CS_diff)) %>%
#   ungroup() %>%
#   #filter(ecd > 0.3 & CS_diff < 2000) %>% arrange(tillage)
#   ggplot(aes(x = CS_diff, group = tillage, colour = tillage)) +
#   stat_ecdf(na.rm = TRUE, lwd = 1.5, geom = "line") +
#   theme_light() +
#   geom_vline(xintercept = 0)




# DRAINAGE effect on CS change ---------------
# CS20 %>%
#   select(-c(Latitude, Longitude, County, `Ave Plot Size (ha)`, rotation, layer)) %>%
#   filter(drainage %in% c("DWM2", "DWM3")) -> cs

CS40 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  filter(drainage %in% c("DWM2", "DWM3")) -> cs

CS60 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  filter(drainage %in% c("DWM2", "DWM3")) -> cs



cs %>%
  filter(!site %in% c("GILMORE", "BRADFORD.A")) %>%   # paired sites only
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first"),
         year_diff = max(year)-min(year)) %>%
  ungroup() %>%
  select(-c(year)) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first,
         CS_diff_yearly = CS_diff/year_diff) %>%
  group_by(drainage) %>%
  mutate(ecd = ecdf(CS_diff)(CS_diff),
         ecd_yearly = ecdf(CS_diff_yearly)(CS_diff_yearly)) %>%
  ungroup() %>% 
  # select(site, plotid, crot_name, drainage, CS_diff, CS_diff_yearly, ecd) %>%   
  # filter(CS_diff < -10000) -> temp
  ggplot(aes(x = CS_diff_yearly, group = drainage, colour = drainage)) +
  stat_ecdf(na.rm = TRUE, lwd = 1.5, geom = "line") +
  theme_light() +
  geom_vline(xintercept = 0)





# COVER CROP effect on CS change ---------------
# CS20 %>%
#   select(-c(Latitude, Longitude, County, `Ave Plot Size (ha)`, rotation, layer)) %>%
#   mutate(crot = ifelse(str_detect(crot_name, "cover"), "COVER", "NO COVER")) -> cs

CS40 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  mutate(crot = ifelse(str_detect(crot_name, "cover"), "COVER", "NO COVER")) -> cs

CS60 %>%
  select(site, plotid, year, CS, crot, crot_name, tillage, drainage, nitrogen, State) %>%
  mutate(crot = ifelse(str_detect(crot_name, "cover"), "COVER", "NO COVER")) -> cs


cs %>%
  select(site, crot, CS) %>%
  group_by(site, crot) %>%
  summarise(CS = mean(CS)) %>%
  group_by(site) %>%
  summarise(n=n()) %>%
  filter(n > 1) %>% 
  select(site) %>%
  collect(site) -> my_sites
  

cs %>%
  filter(site %in% my_sites$site) %>%   # paired sites only
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first"),
         year_diff = max(year)-min(year)) %>%
  ungroup() %>%
  select(-c(year)) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first,
         CS_diff_yearly = CS_diff/year_diff) %>%
  group_by(crot) %>%
  mutate(ecd = ecdf(CS_diff)(CS_diff),
         ecd_yearly = ecdf(CS_diff_yearly)(CS_diff_yearly)) %>%
  ungroup() %>%
  ggplot(aes(x = CS_diff_yearly, group = crot, colour = crot)) +
  stat_ecdf(na.rm = TRUE, lwd = 1.5, geom = "line") +
  theme_light() +
  geom_vline(xintercept = 0)

