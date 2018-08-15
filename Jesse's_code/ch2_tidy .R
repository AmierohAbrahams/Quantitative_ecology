# Ch 2 in tidy R 
# July 2018
# Jesse Smith


# load libraries and data -------------------------------------------------


# Load required packages
library(tidyverse)
#library(dplyr)
library(vegan)
#library(labdsv)
library(ggpubr)
library(corrplot)

# Load additionnal functions
# (files must be in the working directory)
source("panelutils.R")

# Import the data from CSV files

# Species (community) data frame (fish abundances)
spe <- read.csv("DoubsSpe.csv", row.names=1)
# Environmental data frame
env <- read.csv("DoubsEnv.csv", row.names=1)
# Spatial data frame – cartesian coordinates
spa <- read.csv("DoubsSpa.csv", row.names=1)


# fig 2.1  ----------------------------------------------------------------

# do this to see what the data is about

head(spe)     # Display only the first 6 lines
tail(spe)     # displays the last 6 lines 
nrow(spe)			# Number of rows (sites)
ncol(spe)			# Number of columns (species)
dim(spe)			# Dimensions of the data frame (rows, columns)
colnames(spe)		# Column labels (descriptors = species)
rownames(spe)		# Row labels (objects = sites)
summary(spe)		# Descriptive statistics for columns

range(spe)

df <- table(unlist(spe))
df
#barplot(df)

#barplot(df, xlab = "Abundance class", ylab = "Frequency")
#df <- data.frame(unlist(spe))

df_r <- data.frame(integers = df)
df_r$integers.Var1 <- as.numeric(df_r$integers.Var1)
df_r$integers.Freq <- as.numeric(df_r$integers.Freq)

ggplot(data = df_r, aes(x = integers.Var1, y = integers.Freq, fill = integers.Var1)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Abundance class", y = "Frequency") +
  theme_minimal()

# use histogram rather than bar graph 

# site location maps (fig 2.2) ------------------------------------------------------

mut_spa <- spa %>% 
  mutate(N = 1:30)

ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_text(aes(label = N), colour = "blue4") +
  annotate("text", label = "Upstream", x = 68, y = 20, colour = "red") +
  annotate("text", label = "Downstream", x = 10, y = 50, colour = "red") +
  labs(title = "Site Location", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal()

# map of fish species (fig 2.3) ----------------------------------------------------


plot1 <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_point(aes(cex=spe$Satr), shape = 1, colour = "blue4", show.legend = FALSE) +
  labs(title = "Brown trout", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal()

plot2 <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_point(aes(cex=spe$Thth), shape = 1, colour = "blue4", show.legend = FALSE) +
  labs(title = "Grayling", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal() 
  
plot3 <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_point(aes(cex=spe$Baba), shape = 1, colour = "blue4", show.legend = FALSE) +
  labs(title = "Barbel", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal()

plot4 <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_point(aes(cex=spe$Abbr), shape = 1, colour = "blue4", show.legend = FALSE) +
  labs(title = "Common bream", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal()

ggarrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

# non-random distribution of species 
# distribution due to environmental factors? 
# what env factors varies over a spatial scale? - ecological questions 
# biological questions: maybe dist. is due to predators in the area

# multivariate analysis (to include all species)

# Compare species: number of occurrences (fig 2.4)----------------------------------

# To sum by columns, the second argument of apply(), MARGIN, is set to 2
spe.pres <- data.frame(apply(spe > 0, 2, sum))
# Sort the results in increasing order
sort_pres <- sort(spe.pres$apply.spe...0..2..sum.)
sort_pres_df <- data.frame(species = sort_pres)
sort_pres_df$species <- as.numeric(sort_pres_df$species)

# ggplot(sort_pres_df, aes(x = species)) +
#   geom_histogram()

# Compute percentage frequencies
spe.relf <- 100*spe.pres/nrow(spe)
# Round the sorted output to 1 digit
sort_relf <- round(sort(spe.relf$apply.spe...0..2..sum.), 1)
sort_relf_df <- data.frame(spe_freq = sort_relf)
sort_relf_df$spe_freq <- as.numeric(sort_relf_df$spe_freq)

# ggplot(sort_relf_df, aes(x = spe_freq)) +
#   geom_histogram()

df_bind <- cbind(sort_pres_df, sort_relf_df) 

fig1 <- ggplot(df_bind, aes(x = species)) +
  geom_histogram(breaks = seq(0,30,by=5), colour = "grey55")

fig2 <- ggplot(df_bind, aes(x = spe_freq)) +
  geom_histogram(breaks = seq(0,100,by=10), colour = "grey55")

ggarrange(fig1, fig2)

# include figure labels 

# species richness (fig 2.5) ----------------------------------------------

# Compute the number of species at each site
# To sum by rows, the second argument of apply(), MARGIN, is set to 1
sit.pres <- apply(spe > 0, 1, sum)

site_df <- data.frame(sites = sit.pres)

# Sort the results in increasing order
# sort_sit.pres <- sort(sit.pres$apply.spe...0..1..sum.)
sit.pres_df <- data.frame(spe_rich = sit.pres)
# sit.pres_df$spe_rich <- as.numeric(sit.pres_df$spe_rich)

spe_richness <- sit.pres_df %>% 
  mutate(N = 1:30)
 
fig3 <- ggplot(spe_richness, aes(x = N, y = spe_rich)) +
  geom_step() +
  geom_text(aes(label = N), colour = "red") +
  labs( x = "Positions of sites along the river", y = "Species Richness", 
        title = "Species Richness vs. Upstream-Downstream Gradient") +
  theme_minimal()

fig4 <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_path(colour = "deeppink4") +
  geom_point(aes(cex=spe_richness$spe_rich), shape = 1, colour = "blue4", show.legend = FALSE) +
  labs(title = "Map of Species Richness", x = "x coordinate (km)", y = "y coordinate (km)") +
  theme_minimal()

ggarrange(fig3, fig4)

# species richness is the number of species present per site or area

# mosts species present around sites 17-20, 29 has most species 

N0 <- rowSums(spe > 0)         # Species richness
H <- diversity(spe)            # Shannon entropy
N1 <- exp(H)                   # Shannon diversity (number of abundant species)
N2 <- diversity(spe, "inv")    # Simpson diversity (number of dominant species)
J <- H/log(N0)                 # Pielou evenness
E10 <- N1/N0                   # Shannon evenness (Hill's ratio)
E20 <- N2/N0                   # Simpson evenness (Hill's ratio)
(div <- data.frame(N0, H, N1, N2, E10, E20, J))


# Transformation and standardization of the species data (fig 2.6) ----------------------------------------------------------------

#see transforming data from bio stats 

# Partial view of the raw data (abundance codes)
spe[1:5, 2:4]
# Transform abundances to presence-absence (1-0)
spe.pa <- decostand(spe, method="pa")
spe.pa[1:5, 2:4]

# 1 species in a table 
# mutate 3 columns (sqrt, log and raw data) 
# gather (from wide to long data)
# plot 

# fig 2.7 ---------------------------------------------------------------

# environmental factors 

altitude <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_point(cex = 5*env$alt/max(env$alt), colour = "red") +
  geom_path(colour = "lightblue") +
  labs(title = "Altitude", x = "X", y = "Y")

dishcharge <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_point(cex = 5*env$flo/max(env$flo), colour = "blue") +
  geom_path(colour = "lightblue") +
  labs(title = "Flow Rate", x = "X", y = "Y")

oxygen <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_point(cex = 5*env$oxy/max(env$oxy), colour = "green") +
  geom_path(colour = "lightblue") +
  labs(title = "Oxygen", x = "X", y = "Y")

nitrate <- ggplot(mut_spa, aes(x = as.numeric(X), y = as.numeric(Y))) +
  geom_point(cex = 5*env$nit/max(env$nit), colour = "yellow") +
  geom_path(colour = "lightblue") +
  labs(title = "Nitrate", x = "X", y = "Y")

ggarrange(altitude, dishcharge, oxygen, nitrate, nrow = 2, ncol = 2)

# increasingly polluted along the length of the river (Nitrate) - cumulative effect
# colineararity  - closely related, difficult to pinpoint which is the cause 
# exclude altitude (fish cant feel Altitude)

# oxy vs flow rate - which is the cause? together? 
# most important: replicate experiments with various flow rates and oxygen concentrations 
# can replicate by rivers in a region with diff oxy and flow rate regimes 

# fig 2.8 -----------------------------------------------------------------

alt <- ggplot(env, aes(x = dfs, y = alt)) +
  geom_line() +
  labs(title = "Altitude", x = "Distance from source (km)", y = "Altitude (m)") +
  theme_minimal()

flo <- ggplot(env, aes(x = dfs, y = flo)) +
  geom_line() +
  labs(title = "Flow Rate", x = "Distance from source (km)", y = "Flow Rate (m3/s)") +
  theme_minimal()

oxy <- ggplot(env, aes(x = dfs, y = oxy)) +
  geom_line() +
  labs(title = "Oxygen", x = "Distance from source (km)", y = "Oxygen (mg/L") +
  theme_minimal()

nit <- ggplot(env, aes(x = dfs, y = nit)) +
  geom_line() +
  labs(title = "Nitrate", x = "Distance from source (km)", y = "Nitrate (mg/L") +
  theme_minimal()

ggarrange(alt, flo, oxy, nit, nrow = 2, ncol = 2)

# oxy drop is not related to flow rate but maybe to a heavy loading of nitrate
# point source pollution effect 

# fig 2.9 -----------------------------------------------------------------

# corrplot?

pairs = env[, c("alt", "slo", "flo", "pH", "har", "pho", "nit", "amm", "oxy", "bod")]
pairs(env, labels = colnames(env), main = "Bivariate Plots with Histograms and Smooth Curves", pch = 21,
      bg = c("red", "green3", "blue", "yellow")[unclass(env$alt)], panel=panel.smooth, diag.panel=panel.hist) 

# dont really plot in a tidy way 
# never publish 

# fig 2.10 ----------------------------------------------------------------

mut_env <- env %>%
  mutate(log_slo = log(slo))
a <- ggplot(mut_env, aes(x = as.numeric(slo))) +
  geom_histogram(binwidth = 20, breaks = seq(0,50,by=10), colour = "grey55") +
  theme_minimal()

 b <- ggplot(mut_env, aes(x = log_slo)) +
   geom_histogram(binwidth = 2, breaks = seq(-2,4,by=1), colour = "grey55") +
   theme_minimal()

 c <- ggplot(mut_env, aes(x = "", y = slo)) +
   geom_boxplot(colour = "salmon") +
   theme_minimal()

 d <- ggplot(mut_env, aes(x = "", y = log_slo)) +
   geom_boxplot(colour = "salmon") +
   theme_minimal()

 ggarrange(a,b,c,d, nrow = 2, ncol = 2)  
 
 
 # which properties of the env, can explain the fish diversity 