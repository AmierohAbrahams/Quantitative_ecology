# 2 Aug 2018
# Jesse Smith

# Diversity ------------------------

# alpha: number of spp present within the smallest sampling unit (plot, transect or quardrat) - local scale 
# Bray-Curtis, Sorensen, Jaccard 
# compares similarity of one site to another site - 1 value 

# beta: how spp community similarity changes from one site to another 
# species turnover 
# Jaccard index: 
# measures how quickly things become dissimilar 

# gamma: total number of species in the sampling region 
# diversity of all samples combined 

# which diversity measures used depends on the ecological question being asked
# depends on spatial scale and nature of the landscape 
# main focus is alpha 

# Ecologial Similarity and Distance -----------------------

# distance matrix: how different is one transect from another, or how similar are they? 
# similarity and dissimilarity are inversely related to each other 
# dependent on env differences that influences spp composition 
# or unmeasured influences (residual)
# or noise 

# distance matrices: species table + environmental table 
# association/resemblance matrices 
# vegdist in vegan -- euclidean distance / bray / 
# results in pairwise differences in community structure(square matrix)
# square matrix: pairwise 

# euclidian distance only to be applied to env data 
# uses pythagorean theorem 
# results in cartesian units (x,y // x,y,z)
# 	d[jk] = sqrt(sum(x[ij]-x[ik])^2 

# want to compare distance diagonally (shortest/ direct distance) - hypotenuse of pythagorean triangle 

# ALL INFO OF SPP ID OR ENV ARE LOST!!!

# Spatial data frame – cartesian coordinates
spa <- read.csv("DoubsSpa.csv", row.names=1)
head(spa)

spa_euc <- round(vegdist(spa, method = "euclidian"), 2)
spa_euc

dim(as.matrix(spa_euc))

# Environmental data frame
env <- read.csv("DoubsEnv.csv", row.names=1)

# remove influence of units 
env_std <- decostand(env, method = "standardize")
env_std

# simmilarity / dissimilarity index based on common env variables 
env_euc <- round(vegdist(env_std, method = "euclidian"), 2)
env_euc



# exercise 1  -------------------------------------------------------------

# look at Doubs species data 

# Species (community) data frame (fish abundances)
spe <- read.csv("DoubsSpe.csv", row.names=1)

# bray-curtis for abundance data 
# Jaccard fro presence/ absence data (binary = true)

# Bray or Jaccard? = Bray ONLY FOR SPP

spe_bray <- round(vegdist(spe, method = "bray"), 2)
as.matrix(spe_bray)[, 1:4]

#general trends 
# sites close together (sequential sites), initially have some similarity (< 1), further u move away are very dissimilar (= 1)

# spe_jacc <- round(vegdist(spe, method = "jaccard"), 2)
# spe_jacc

# plot graph 
# ggplot(data.frame(spe_bray), aes(x = "", y = ""))


# create a distance matrix with altitude data,
# euclidian == env data 

alt_num <- data.frame(as.numeric(env$alt))

alt_euc <- vegdist(alt_num, method = "euclidian")
as.matrix(alt_euc)[, 1:4]

plot.dat <- tibble(x = seq (1:30), 
                   h = as.matrix(alt_euc)[, 1])


ggplot(plot.dat, aes(x = x, y = h)) +
  geom_point()








