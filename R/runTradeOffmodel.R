require(dplyr)
library(here)
source(here('R/ecopath_inputs.R'))
print('Ecopath has run.')
source(here('R/calc_consumption.R'))
source(here('R/calc_jacobian.R'))
source(here('R/combine_groups.R'))

n.living.groups <- sum(test$type <= 1)
n.boot <- 1000
Jr.ls <- list()
grps.of.interest <- c('adu.panaeids', 'juv.panaeids',
                      'adu.r.drum', 'juv.r.drum',
                      'adu.croaker.spot.perch', 'juv.croaker.spot.perch',
                      'adu.blue.crab', 'juv.blue.crab',
                      'adu.menhaden', 'juv.menhaden')


stanza.list <- split(group.info, group.info$stanza) %>%
  lapply(function(x) x$group)
combined.mod <- test
for(st in 1:length(stanza.list)) {
  combined.mod <- combine_groups(grp.names = stanza.list[[st]], 
                                 rpath.mod = combined.mod, 
                                 combined.name = names(stanza.list)[st])
}
n.living.combined <- sum(combined.mod$type <= 1)
Jr.ls.combined <- list()
grps.of.interest.combined <- c('panaeids',
                               'r.drum',
                               'croaker.spot.perch',
                               'blue.crab',
                               'menhaden')

  
set.seed(39804)
for(ii in 1:n.boot) {
  eps.boot <- rbeta(n.living.groups^2, 2, 2)
  gam.boot <- rbeta(n.living.groups, 3, 12)
  the.boot <- rbeta(n.living.groups^2, 12, 3)
  Jr.ls[[ii]] <- calc_jacobian(test, theta.use=the.boot, eps.use=eps.boot, 
                               gamma.use=gam.boot)
  Jr.ls.combined[[ii]] <- calc_jacobian(combined.mod, 
                                        theta.use=the.boot[1:n.living.combined^2], 
                                        eps.use=eps.boot[1:n.living.combined^2], 
                                        gamma.use=gam.boot[1:n.living.combined])
} 

# Calculate yield trade offs ----------------------------------------------
# Separate out detritus groups as a constant biomass level

F.living <- with(test, (rowSums(Landings + Discards)/Biomass)[type <= 1])

# Calculate Jacobian of r vector
effort <- 10
selectivity <- F.living/effort

Jr.inv <- lapply(Jr.ls, function(Jr.i) {
  Jr.inv.i <- matrix(NA, nrow = nrow(Jr.i), ncol = ncol(Jr.i))
  try(eval(parse(text = "Jr.inv.i=solve(Jr.i)")), silent=FALSE)
  })

# check for numerical stability. counts number of non-invertible jacobians.
temp <- sapply(Jr.inv, function(xx) xx[1,1]) %>% is.na() %>% sum()
print(paste(temp, 'non-invertible Jacobians'))

# Fishing
dx.dE <- sapply(Jr.inv, function(Jr.inv.i) Jr.inv.i %*% 
                  matrix(selectivity, ncol=1))
rownames(dx.dE) <- test$Group[test$type <= 1]

fishing.res <- dx.dE[grps.of.interest,] %>%
apply(2, function(xx) xx /
        test$Biomass[sapply(grps.of.interest, function(xx) which(test$Group==xx))])

# Predation      
Jr.inv.arr <- sapply(Jr.inv, identity, simplify = 'array') 
dimnames(Jr.inv.arr) <- list(biomass.i = test$Group[test$type <= 1],
                             r.j = test$Group[test$type <= 1],
                             NULL)

Jr.inv.std <- array(NA, dim=c(10, 3, n.boot), 
                    dimnames=list(biomass.i = grps.of.interest,
                                  r.j = c('dolphins', 'pelicans', 'diving.birds'),
                                  NULL))

for(prey in grps.of.interest) {
  for(pred in c('dolphins', 'pelicans', 'diving.birds')) {
    Jr.inv.std[prey, pred,] <- Jr.inv.arr[prey, pred,] *
      test$PB[test$Group==pred] / 
      test$Biomass[test$Group==prey]
  }
}

# Fishing combined groups
F.living.combined <- with(combined.mod, 
                          (rowSums(Landings + Discards)/Biomass)[type <= 1])
# Calculate Jacobian of r vector
selectivity.combined <- F.living.combined/effort

Jr.inv.combined <- lapply(Jr.ls.combined, function(Jr.i) {
  Jr.inv.i <- matrix(NA, nrow = nrow(Jr.i), ncol = ncol(Jr.i))
  try(eval(parse(text = "Jr.inv.i=solve(Jr.i)")), silent=FALSE)
})
# check for numerical stability. counts number of non-invertible jacobians.
temp <- sapply(Jr.inv.combined, function(xx) xx[1,1]) %>% is.na() %>% sum()
print(paste(temp, 'non-invertible Jacobians'))

dx.dE.combined <- sapply(Jr.inv.combined, function(Jr.inv.i) Jr.inv.i %*% 
                           matrix(selectivity.combined, ncol=1))
rownames(dx.dE.combined) <- combined.mod$Group[combined.mod$type <= 1]

fishing.res.combined <- dx.dE.combined[grps.of.interest.combined,] %>%
  apply(2, function(xx) xx /
          combined.mod$Biomass[sapply(grps.of.interest.combined, 
                                      function(xx) which(combined.mod$Group==xx))])
Jr.inv.arr.combined <- sapply(Jr.inv.combined, identity, simplify = 'array') 
dimnames(Jr.inv.arr.combined) <- list(biomass.i = combined.mod$Group[combined.mod$type <= 1],
                                      r.j = combined.mod$Group[combined.mod$type <= 1],
                                      NULL)

Jr.inv.std.combined <- array(NA, dim=c(5, 3, n.boot), 
                    dimnames=list(biomass.i = grps.of.interest.combined,
                                  r.j = c('dolphins', 'pelicans', 'diving.birds'),
                                  NULL))

for(prey in grps.of.interest.combined) {
  for(pred in c('dolphins', 'pelicans', 'diving.birds')) {
    Jr.inv.std.combined[prey, pred,] <- Jr.inv.arr.combined[prey, pred,] *
      combined.mod$PB[combined.mod$Group==pred] / 
      combined.mod$Biomass[combined.mod$Group==prey]
  }
}

# All predators at once, unaggregated groups
pred.pb <- data.frame(test[-(1:4)]) %>%
  dplyr::filter(type <= 1) %>%
  dplyr::mutate(pred.pb = ifelse(Group %in% c('dolphins', 'pelicans', 'diving.birds'), 
                                 PB, 0)) %>%
  with(pred.pb)

predation.decline <- pred.pb/effort

dx.dE.pred <- sapply(Jr.inv, function(Jr.inv.i) Jr.inv.i %*% 
                  matrix(predation.decline, ncol=1))
rownames(dx.dE.pred) <- test$Group[test$type <= 1]

pred.all.res <- dx.dE.pred[grps.of.interest,] %>%
  apply(2, function(xx) xx /
          test$Biomass[sapply(grps.of.interest, function(xx) which(test$Group==xx))])

# All predators at once, aggregated groups
pred.pb.combined <- data.frame(combined.mod[-(1:4)]) %>%
  dplyr::filter(type <= 1) %>%
  dplyr::mutate(pred.pb = ifelse(Group %in% c('dolphins', 'pelicans', 'diving.birds'), 
                                 PB, 0)) %>%
  with(pred.pb)

predation.decline.combined <- pred.pb.combined/effort

dx.dE.pred.combined <- sapply(Jr.inv.combined, function(Jr.inv.i) Jr.inv.i %*% 
                       matrix(predation.decline.combined, ncol=1))
rownames(dx.dE.pred.combined) <- combined.mod$Group[combined.mod$type <= 1]

pred.all.res.combined <- dx.dE.pred.combined[grps.of.interest.combined,] %>%
  apply(2, function(xx) xx /
          combined.mod$Biomass[sapply(grps.of.interest.combined, function(xx) which(combined.mod$Group==xx))])


## Direct mortality analysis
consump.ls <- calc_consumption(test)
Mpred.mat <- apply(consump.ls$consump, 2, function(xx) xx/test$Biomass[test$type != 3])
rownames(Mpred.mat) <- colnames(Mpred.mat) <- test$Group[test$type != 3]

names(F.living) <- test$Group[test$type <= 1]

mort.mat <- cbind(Mpred.mat[grps.of.interest, 
                            c('dolphins', 'pelicans', 'diving.birds')],
                  F.living[grps.of.interest]) /
  test$PB[sapply(grps.of.interest, function(xx) which(test$Group==xx))]



# print(test) %>%
#   filter(Group!='fishery' & Group!='detritus') %>%
#   summarize(mean(TL), mean(TL*Biomass))

