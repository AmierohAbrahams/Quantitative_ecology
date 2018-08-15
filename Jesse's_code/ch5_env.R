# CA using env data 

# Load the required packages
# (vegan must be loaded after ade4 to avoid some conflicts)
library(ade4)
library(vegan)
library(gclus)
library(ape)
library(FactoMineR)

# Load additionnal functions
# (files must be in the working directory)
source("evplot.R")
source("cleanplot.pca.R")
source("PCA.newr.R")
source("CA.newr.R")

# Import the data from CSV files
# (files must be in the working directory)

# Doubs fish data
spe <- read.csv("DoubsSpe.csv", row.names=1)
env <- read.csv("DoubsEnv.csv", row.names=1)
spa <- read.csv("DoubsSpa.csv", row.names=1)
# Remove empty site 8
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]



# Compute CA
(env.ca <- cca(env))
summary(env.ca)		# default scaling 2
summary(env.ca, scaling=1)

# Plot eigenvalues and % of variance for each axis
(ev2 <- env.ca$CA$eig)
# dev.new(title="CA eigenvalues")
evplot(ev2)

# CA biplots
# dev.new(title="CA biplots", width=14, height=7)
par(mfrow=c(1,2))
# Scaling 1: sites are centroids of species
plot(env.ca, scaling=1, main="CA env variables - biplot scaling 1")
# Scaling 2 (default): species are centroids of sites
plot(env.ca, main="CA env variables - biplot scaling 2")

# A surface plot for the CA
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(env, tmp <- ordisurf(env.ca ~ alt, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Alt"))
abline(h = 0, v = 0, lty = 3)
with(env, tmp <- ordisurf(env.ca ~ flo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Flo"))
abline(h = 0, v = 0, lty = 3)
with(env, tmp <- ordisurf(env.ca ~ bod, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Bod"))
abline(h = 0, v = 0, lty = 3)
with(env, tmp <- ordisurf(env.ca ~ amm, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Amm"))
abline(h = 0, v = 0, lty = 3)

# A posteriori projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active
(env.ca.env <- envfit(env.ca, env, scaling = 2)) # Scaling 2 is default
plot(env.ca.env)
# Plot significant variables with a different colour
plot(env.ca.env, p.max = 0.05, col = "red")

