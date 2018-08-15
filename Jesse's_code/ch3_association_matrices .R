# 6 august 2018 
# Jesse Smith 

# 1 - similar
# 0 - dissimilar 

#association matrices: how closely are sites associated with one another based on how closely the spp are related to each other 
# species: comparing sites with sites 
# transposed: comparing spp with spp, based on how present spp are within sites 
# jaccard coefficient: presence absence relativity (binary = true)
# level of dissimilarity: if one spp is present, the other is x times less likely to occur 

# = correlation matrix for env data 
# pearson correlation coefficient (+/-)
# pairwise correlation plot 
# q(patterns btwn variables) mode and r(corr) mode analysis 

# dont apply euclidian distances to spp data!
# transforming data 
# log transformations: importance of high values

# CH4: cluster analysis: for discrete differences  -------------------------

# packages: vegan(standardization and transformations), cluster and gclus
# applied to spp or env data 
# clustering and ordination 



# ch 5: unconstrained ordination  --------------------------------------------

# spp patterns speaks for itself in the absence of env data 
# strong gradients 
# ordinations highlight gradients 
# suited to multivariate data: with space (spatial) with many sites, with many variables (spp or env)
# patterns in communities 

# subset: orthogonal axes (linearly independent, uncorrelated)
# unconstrained ordination is purely descriptional not statistical 
# patterns inherent to biological data 
# constrained ordination adds statistical testing 

# why ordination? 

# 1) It is impossible to visualize multiple dimensions simultaneously. While physicists grumble if space exceeds four dimensions, ecologists typically grapple with dozens of dimensions (species and/or samples).

# 2) A single multivariate analysis saves time, in contrast to a separate univariate analysis for each species.

# 3) Ideally and typically, dimensions of this ‘low dimensional space’ will represent important and interpretable environmental gradients.

# 4) If statistical tests are desired, problems of multiple comparisons are diminished when species composition is studied in its entirety

# 5) Statistical power is enhanced when species are considered in aggregate, because of redundancy
 
# 6) By focusing on ‘important dimensions’, we avoid interpreting (and misinterpreting) noise. Thus, ordination is a ‘noise reduction technique’ (Gauch 1982).
 
# 7) We can determine the relative importance of different gradients; this is virtually impossible with univariate techniques.
 
# 8) Community patterns may differ from population patterns.
 
# 9) Some techniques provide a measure of beta diversity

# 10) The graphical results from most techniques often lead to ready and intuitive interpretations of species-environment relationships.

# Principle Component Analysis (PCA)----------------------------

# 1st axis has the most explanatory power and axes decrease in power accordingly 
# order of axes is important, related to relative influence of env drivers 
# visual patterns 


library(vegan)
library(tidyverse)

# Load additionnal functions
# (files must be in the working directory)
source("evplot.R")
source("cleanplot.pca.R")
source("PCA.newr.R")
source("CA.newr.R")

# Import the data from CSV files
# (files must be in the working directory)

# Doubs fish data
spe <- as.tibble(read.csv("DoubsSpe.csv", row.names=1))
env <- as.tibble(read.csv("DoubsEnv.csv", row.names=1))
spa <- as.tibble(read.csv("DoubsSpa.csv", row.names=1))
# Remove empty site 8
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]

summary(env)

# scaling = 1 vs scaling = 2


#  task 1 -----------------------------------------------------------------

# pca on spp data
# jaccard, binary = true 
# rda,  scaling = true

spe.jacc <-  vegdist(spe, method = "bray", binary = TRUE)

spe.pca <- rda(spa.jacc, scale = )









