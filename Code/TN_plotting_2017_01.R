# PLOT TIME - TN -----------------------------------------
TN %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> TN_summary
# to print 
TN_summary[is.na(TN_summary)] <- "-"
TN_summary



# PLOT 1 -------------------- TN average by site
TN %>%
  select(site, TN, Latitude, Longitude, State, County, `Ave Plot Size (ha)`) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = TN, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot01.png", width = 12, height = 8, dpi = 300)


# PLOT 2 -------------------- TN difference by site
TN %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, TN, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(site, TN_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`) %>%
  mutate(State = as.factor(State)) %>%
  mutate(site = factor(site, levels = unique(site[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = site, y = TN_diff, fill = State)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Site ID") +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years by Site") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot02.png", width = 12, height = 8, dpi = 300)



# PLOT 3 -------------------- TN average by State
TN %>%
  select(TN, Latitude, Longitude, State) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = TN)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot03.png", width = 12, height = 8, dpi = 300)



# PLOT 4 -------------------- TN difference by State
TN %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, TN, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(State, TN_diff, first, last, Latitude, Longitude) %>%
  mutate(State = factor(State, levels = unique(State[order(Latitude, decreasing = FALSE)]))) %>%
  ggplot(aes(x = State, y = TN_diff)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "US State") +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years by State") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot04.png", width = 12, height = 8, dpi = 300)



# PLOT5 --------------------- TN by rotation
TN %>%
  select(crot, crot_name, TN) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN by Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot05.png", width = 12, height = 8, dpi = 300)



# PLOT6 --------------------- TN difference by rotation
TN %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, TN, fly) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  select(crot, crot_name, TN_diff, first, last) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot06.png", width = 12, height = 8, dpi = 300)



# PLOT7 --------------------- TN by rotation and Tillage
TN %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, TN) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  ggplot(aes(x = crot_name, y = TN, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  theme_light() +
  ggtitle("Average TN by Crop Rotation grouped by Drainage and Tillage") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12))
ggsave(filename = "TN_plot07.png", width = 12, height = 8, dpi = 300)



# PLOT8 --------------------- TN difference by rotation and tillage
TN %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, TN, fly) %>%
  spread(fly, TN) %>%
  mutate(TN_diff = last - first) %>% 
  ungroup() %>%
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  select(crot, crot_name, tillage, TN_diff, first, last) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT", "orange", "grey40")) %>%
  select(crot, crot_name, TN_diff, first, last, tillage) %>%
  mutate(crot_name = factor(crot_name, levels = unique(crot_name[order(crot, decreasing = FALSE)]))) %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>% 
  ggplot(aes(x = crot_name, y = TN_diff, fill = I(color))) + 
  geom_bar(stat = "summary", fun.y = "mean", position = "stack") +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Total Nitrogen (kg N/ha)", labels = comma) +
  coord_flip() +
  theme_light() +
  ggtitle("Average TN Difference between the First and the Last Data Years\nby Crop Rotation grouped by Drainage and Tillage") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text  = element_text(size = 12))
ggsave(filename = "TN_plot08.png", width = 12, height = 8, dpi = 300)
