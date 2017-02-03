CS %>%
  filter(site == "BRADFORD.C") %>%
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
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.3, size = 1) +
  #geom_text(aes(y = 5000, label = count, colour = I("grey60")), vjust = 1.5, size = 7) +
  facet_grid( ~ tillage) +
  scale_x_discrete(name = "Crop Rotation", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(name = "Carbon Stock (kg C/ha)", labels = comma) +
  #coord_flip() +
  theme_light() +
  ggtitle("BRADFORD.C\nCS Difference between the First and the Last Data Years") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold"),
        axis.title = element_text(face = "bold", colour = "grey30", size = 16),
        axis.text.x = element_text(angle=90, vjust=0.5, size=12),
        axis.text.y = element_text(size = 12), 
        strip.text = element_text(size = 14)) 
ggsave(filename = "BRADFORD.C.png", width = 12, height = 8, dpi = 300)





# Coefficient of Variance of CS by depth
CS2 %>%
  group_by(site, layer) %>%
  summarize(ave = mean(CS), dev = sd(CS)) %>%
  mutate(CV = dev/ave) %>%
  mutate(layer = factor(layer, levels=c("top", "middle", "bottom"))) %>%
  #mutate(CV = percent(CV)) %>%
  ggplot(aes(layer, CV)) + 
  geom_boxplot(outlier.shape = NA, lwd = 1) + 
  geom_jitter(alpha = 0.3, width = 0.15)



# CS delta of deltas
CS %>%
  select(-c(Latitude, Longitude, County, `Ave Plot Size (ha)`)) %>%
  mutate(till = ifelse(tillage == "TIL2", "CT", "NT")) %>%
  group_by(site, year, crot_name, till) %>%
  summarise(ave_CS = mean(CS)) %>%
  filter(!is.na(till)) %>%
  spread(key=till, value = ave_CS) %>%
  filter(!is.na(CT), !is.na(NT)) %>%
  mutate(delta = NT - CT) %>%
  group_by(site) %>%
  mutate(fly = ifelse(year > mean(year), "last", "first")) %>%
  mutate(cover = ifelse(grepl("cover", crot_name), "cover", "no cover")) %>%
  select(-c(year, CT, NT)) %>%
  spread(key=fly, value = delta) %>%
  mutate(delta_delta = last - first) %>%
  ggplot(aes(x=cover, y = delta_delta)) + 
  geom_boxplot()

ggplot(aes(x=cover, y=delta)) + geom_boxplot(outlier.alpha = 0) + geom_jitter(width=0.1, size=2)

ggplot(aes(x=crot_name, y= delta, color = year)) + 
  geom_boxplot(outlier.shape = NA, size = 1) + 
  geom_jitter(width = 0.1, size = 2)
