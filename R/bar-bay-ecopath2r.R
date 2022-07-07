long.diet.mat <- read.csv('Data/long_diet_mat.csv')
head(long.diet.mat)
n.grps <- length(unique(long.diet.mat$PreyID))
diet.mat <- matrix(long.diet.mat$Diet, nrow = n.grps, ncol = n.grps, byrow = TRUE)

ecopath.group.info <- read.csv('Data/ecopath_GroupInfo.csv')

