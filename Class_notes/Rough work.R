##### Some graph codes:


ggplot(mydata, aes(months, values)) +
  geom_bar(stat = "identity")

ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  #geom_line(aes(group = Chick)) +
  geom_smooth(method = "lm")

ggplot(data = Chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Diet, ncol = 2) +
  labs(x = "Days", y = "Mass (g)")

ggplot(data = Chicks, aes(x = Time, y = weight)) +
  geom_point(aes(colour = Diet)) +
  geom_line(aes(group = Chick), colour = "lightyellow2", alpha = 0.9) +
  geom_smooth(method = "lm", aes(colour = Diet))

plot3 <- ggplot(nandos, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x ="Final Mass (g)", y = "Count")

new <- ggarrange(plot1, plot2, plot3, plot4,
                 ncol = 2, nrow = 2, # Set number of rows and columns
                 labels = c("A", "B", "C", "D"), # Label each figure
                 common.legend = TRUE)

ggplot(dat1, aes(x = site, y = mn.fr.len)) +
  geom_col(aes(fill = site)) +
  scale_fill_brewer(palette = "Spectral")

dat1 <- lam %>%
  group_by(site) %>% 
  summarise(mn.fr.len = mean(blade_length),
            sd.fr.len = sd(blade_length))

ggplot(dat1, aes(x = site, y = mn.fr.len)) +
  geom_col(aes(fill = site)) +
  geom_errorbar(aes(ymin = mn.fr.len - sd.fr.len,
                    ymax  = mn.fr.len + sd.fr.len)) +
  scale_fill_brewer(palette = "Spectral")

plot5 <- ggplot(dat1, aes(x = Petal.Length)) +
  geom_histogram(aes(fill = Species)) 
plot5
##################################################################################################3

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumalitive sum",
       x = NULL, y = "Count") +
  theme_minimal()


# Making a pie chart

ggplot(data = sa_count, aes(x = "", y = n, fill= time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  scale_fill_brewer() #### Using a different palette

# Histogram
ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats,  size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))

ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

ggplot(data = chicks_tukey, aes(x = pairs, y = diff)) +
  geom_segment(aes(x = lwr, xend = upr, y = pairs, yend = pairs)) +
  geom_vline(xintercept = 0,linetype="dotted", 
             color = "blue", size=1.5) 

################# Transforming the data #####################
dat2 <- data %>% 
  mutate(log10 = log10(Steps)) %>% 
  mutate(log = log(Steps)) %>%
  mutate(cuberoot = (Steps)) %>% 
  mutate(sqrt = sqrt(Steps))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))


sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

# Plot these
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats,  size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))









