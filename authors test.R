# Amieroh stuffs ----------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(corrplot)

Ecology_1970s <- read_csv("Desktop/Ecology_1970s.csv")
Biogeography_1970s<- read_csv("Desktop/1970_Biogeography.csv")
Ecography_1970s <- read_csv("Desktop/1970_Ecography.csv")

plot1 <- ggplot(Biogeography_1970s, aes(x = year, y = No_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Biogeography 1970's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plot1

plot2 <- ggplot(Ecology_1970s, aes(x = year, y = No_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecology 1970's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plot2

plot3 <- ggplot(Ecography_1970s, aes(x = year, y = No_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecography 1970's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plot3

combined_1970s <- ggarrange(plot1, plot2, plot3,
                            ncol = 2, nrow = 2)
combined_1970s

JD_1980 <- read_csv("Desktop/JD_1980.csv")

JD_biogeography <- JD_1980 %>% 
  filter(journal == "biogeography")

graph1 <- ggplot(JD_biogeography, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Biogeography 1980's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
graph1

JD_ecology <- JD_1980 %>% 
  filter(journal == "ecology")

graph2 <- ggplot(JD_ecology, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecology 1980's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
graph2

JD_ecography <- JD_1980 %>% 
  filter(journal == "ecography")

graph3 <- ggplot(JD_ecography, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecography 1980's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
graph3

combined_1980s <- ggarrange(graph1, graph2, graph3,
                            ncol = 2, nrow = 2)
combined_1980s

JD_1990 <- read_csv("Desktop/JD_1990.csv")

JD_bio1990 <- JD_1990 %>% 
  filter(journal == "Biogeography")

gr1 <- ggplot(JD_bio1990, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Biogeography 1990's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
gr1

JD_eco1990 <- JD_1990 %>% 
  filter(journal == "Ecology")

gr2 <- ggplot(JD_eco1990, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecology 1990's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
gr2

JD_ecog1990 <- JD_1990 %>% 
  filter(journal == "Ecography")

gr3 <- ggplot(JD_ecog1990, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecography 1990's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
gr3

combined_1990s <- ggarrange(gr1, gr2, gr3,
                            ncol = 2, nrow = 2)
combined_1990s

JD_2000 <- read_csv("Desktop/JD_2000.csv")

JD_bio <- JD_2000 %>% 
  filter(journal == "Biogeography")

g1 <- ggplot(JD_bio, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Biogeography 2000's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
g1

JD_eco <- JD_2000 %>% 
  filter(journal == "Ecology")

g2 <- ggplot(JD_eco, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecology 2000's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
g2

JD_ecog <- JD_2000 %>% 
  filter(journal == "Ecography")

g3 <- ggplot(JD_ecography, aes(x = year, y = no_of_authors)) +
  geom_col(aes(fill = study_size)) +
  labs(x = "Year", y = "No of authors", colour = "study area") + 
  theme(legend.position = "bottom") +
  ggtitle("Journal of Ecography 2000's") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
g3

combined_2000s <- ggarrange(g1, g2, g3,
                            ncol = 2, nrow = 2)
combined_2000s

Combined_authors <- read_csv("Desktop/Combined_authors.csv")

normality_test <- Combined_authors %>% 
  group_by(year) %>% 
  summarise(norm_authors = as.numeric(shapiro.test(no_of_authors)[2]))

homo <- Combined_authors %>% 
  group_by(year) %>% 
  summarise(auth_var = var(no_of_authors))

compare_means(no_of_authors ~ year, data = Combined_authors, method = "t.test")

compare_means(no_of_authors ~ year , data = Combined_authors, method = "t.test", alternative = "two.sided")

# Carlin stuffs -----------------------------------------------------------

# cant do a ttest because there are more than two factors or groups 

# so, what we can do is an ANOVA looking at number of authors and year...
# 

# H0: number of authors does not increase as years go by
# H1: number of authors increases as years go by
auth.aov <- aov(no_of_authors ~ year, data = Combined_authors)
summary(auth.aov)
  # results show p<0.05...

# if data is not normal... (similar end result though for non-parametric test)
compare_means(no_of_authors ~ year, data = Combined_authors, method = "kruskal.test")
  # results show p<0.05 therefore, 
    # number of authors increase as years go by

# but there are a load of dates, maybe we can condense dates into just 70s, 80s, 90s and 00s?
# then we'll only have four groups to compare instead of all the dates individually?? i think... 

# TukeyHSD(auth.aov)
  # ^ this gives an error :(... 

# we can also do an ANOVA for number of authors by journal
# however. not sure if this has any significance to us. but I'll do it anyway
jour.aov <- aov(no_of_authors ~ journal, data = Combined_authors)
summary(jour.aov)
  # results show p = 0.111 so there is no significant difference in number of authors by journal

# same result for non-parametric test
compare_means(no_of_authors ~ journal, data = Combined_authors, method = "kruskal.test")

TukeyHSD(jour.aov)
plot(TukeyHSD(jour.aov))
# TBH im deeply confused and i dont know what this means. i am a fool.

# maybe we can do a correlation to see if theres a relationship between number of authors and
# study size (more authors = larger study size?)
  # cor.test(Combined_authors$no_of_authors, Combined_authors$study_size, method = "")
    # error for this too... SAD. Do you think there will be a difference if we change study size to
    # classes 1-5?

# correlation and regression for year and number of authors
cor.test(Combined_authors$year, Combined_authors$no_of_authors, method = "spearman")
  # 36.8% of variance explained (not very strong relationship but positive)
summary(lm(no_of_authors ~ year, data = Combined_authors))
  # 15.3% of variance explained (also not very strong, positive though)

# random EDA stuff
summary(Combined_authors)

ggplot(data = Combined_authors, aes(x = as.factor(year), y = no_of_authors)) +
  geom_col() +
  facet_wrap(~ journal)

# to try:
  # condense dates into their decades... 70s, 80s etc
    # rename study sizes as class numbers (1-5)... this might make the correlation thing work
      # working with mean number of authors per year or decade may also make things easier 
# in the normality test more of the values are less than 0.05. so i dont think the data is normally distributed...