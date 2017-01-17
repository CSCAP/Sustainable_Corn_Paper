# PLOT TIME - CS -----------------------------------------
CS %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> CS_summary
# to print 
CS_summary[is.na(CS_summary)] <- "-"
CS_summary



# PLOT 1 -------------------- CS average by site
CS %>%
  select(site, CS, Latitude, Longitude, State, County, `Ave Plot Size (ha)`) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  
  ggplot(aes(x = site, y = CS, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot01.png", width = 12, height = 8, dpi = 300)


# PLOT 2 -------------------- CS difference by site
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(site, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  
  ggplot(aes(x = site, y = CS_diff, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot02.png", width = 12, height = 8, dpi = 300)



# PLOT 3 -------------------- CS average by State
CS %>%
  select(CS, Latitude, Longitude, State) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  
  ggplot(aes(x = State, y = CS)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot03.png", width = 12, height = 8, dpi = 300)



# PLOT 4 -------------------- CS difference by State
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(State, CS_diff, first, last, Latitude, Longitude) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  
  ggplot(aes(x = State, y = CS_diff)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot04.png", width = 12, height = 8, dpi = 300)



# PLOT5 --------------------- CS by rotation
CS %>%
  select(crot, crot_name, CS) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  
  ggplot(aes(x = crot_name, y = CS, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot05.png", width = 12, height = 8, dpi = 300)



# PLOT6 --------------------- CS difference by rotation
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  select(crot, crot_name, CS_diff, first, last) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  
  ggplot(aes(x = crot_name, y = CS_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years\nby Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "plot06.png", width = 12, height = 8, dpi = 300)



# PLOT7 --------------------- CS by rotation and Tillage
CS %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, CS) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  group_by(crot, tillage) %>%
  mutate(count = n()) %>%  
  mutate(mean = mean(CS)) %>%
  mutate(sd = sd(CS)) %>%
  mutate(se = sd/sqrt(count)) %>%
  
  ggplot(aes(x = crot_name, y = CS, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  geom_text(aes(y = 150000, label = count, colour = I("grey60")), vjust = 1.5, size = 7) +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average CS by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 150000))
ggsave(filename = "plot07.png", width = 12, height = 8, dpi = 300)



# PLOT8 --------------------- CS difference by rotation and tillage
CS %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  ungroup() %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, CS_diff, first, last) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  group_by(crot, tillage) %>%
  mutate(count = n()) %>%  
  mutate(mean = mean(CS_diff)) %>%
  mutate(sd = sd(CS_diff)) %>%
  mutate(se = sd/sqrt(count)) %>% 
  
  ggplot(aes(x = crot_name, y = CS_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  geom_text(aes(y = 14000, label = count, colour = I("grey60")), vjust = 1.5, size = 7) +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  #coord_flip() +
  theme_light() +
  ggtitle("Average CS Difference between the First and the Last Data Years\nby Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 14)) +
  coord_cartesian(ylim = c(-15000, 15000))
ggsave(filename = "plot08.png", width = 12, height = 8, dpi = 300)


