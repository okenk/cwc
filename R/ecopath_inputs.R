# devtools::install_github(repo = 'okenk/Rpath', ref = 'before_stanza_changes')
# To run, must install Rpath version before commit 8735a8383d888bced13247dcd60149bcc93d2185
# After that commit, Rpath can only handle leading stanzas that are last.
# The before_stanza_changes branch on okenk fork also includes better plotting functions

library(Rpath)
library(data.table)
library(magrittr)
library(here)

group.info <- read.csv(here('Data/GroupInfo_data_deleted.csv'), stringsAsFactors = FALSE, na.strings = '')
#group.info <- read.csv('Data/GroupInfo_biomass_quadrupled.csv', stringsAsFactors = FALSE, na.strings = '')
diet.df <- read.csv(here('Data/Diet.csv'))#, na.strings = '0')
stanzas <- read.csv(here('Data/stanzas.csv'), stringsAsFactors = FALSE)

#group.info$EE[grepl('insect', group.info$group)] <- 0.3
# tibble::tibble(group = group.info$group, dolphin.diet = diet.df$dolphins) %>% View
# group.info <- read.csv('Data/GroupInfoDM.csv', stringsAsFactors = FALSE, na.strings = '')
# diet.df <- read.csv('Data/DietDM.csv')#, na.strings = '0')
# stanzas <- read.csv('Data/stanzasDM.csv', stringsAsFactors = FALSE)
rpath.start <- create.rpath.params(group = group.info$group, 
                                  type = group.info$type,
                                  stgroup = group.info$stanza)


# DM.ecopath <- create.rpath.params(group = group.info$X,
#                                   type = group.info$Type,
#                                   stgroup = group.info$Stanza)




rpath.start$model[,Biomass := group.info$Biomass]
rpath.start$model[,PB := group.info$PB]
rpath.start$model[,QB := group.info$QB]
rpath.start$model[,BioAcc := c(rep(0, nrow(rpath.start$model)-1), NA)]
rpath.start$model[,Unassim := c(rep(.2, 45), rep(.4, 5), NA)]
rpath.start$model[,fishery := group.info$Landings]
rpath.start$model[,fishery.disc := group.info$Discards]
rpath.start$model[Group=='herb.insects', EE := .3]
rpath.start$model[Group=='carn.insects', EE := .3]
# rpath.start$model[Group=='dolphins', Biomass := .04]

# DM.ecopath$model[,Biomass := group.info$Biomass]
# DM.ecopath$model[,PB := group.info$PB]
# DM.ecopath$model[,QB := group.info$QB]
# DM.ecopath$model[,BioAcc := c(rep(0, nrow(DM.ecopath$model)-1), NA)]
# DM.ecopath$model[,Unassim := c(rep(.2, 66), rep(.4, 4), NA)]
# DM.ecopath$model[,Detritus := c(rep(1, 69), 0, 1)]
# DM.ecopath$model[,Fishery := group.info$Landings]
# DM.ecopath$model[,Fishery.disc := group.info$Discards]

ordered.stanzas <- stanzas[match(rpath.start$stanzas$stgroups$StanzaGroup, 
                                 stanzas$stanza),]
rpath.start$stanzas$stgroups[,VBGF_Ksp := ordered.stanzas$K]
rpath.start$stanzas$stgroups[,Wmat := rep(.09, rpath.start$stanzas$NStanzaGroups)]

# ordered.stanzas <-stanzas[match(DM.ecopath$stanzas$stgroups$StanzaGroup, stanzas$stanza),]
# DM.ecopath$stanzas$stgroups[,VBGF_Ksp := ordered.stanzas$K]
# DM.ecopath$stanzas$stgroups[,Wmat := rep(.09, DM.ecopath$stanzas$NStanzaGroups)]

stanza.info <- group.info[!is.na(group.info$stanza),]
rpath.start$stanzas$stindiv[,First := stanza.info$stanza_start]
rpath.start$stanzas$stindiv[,Last := stanza.info$stanza_end]
rpath.start$stanzas$stindiv[,Z := stanza.info$PB]
rpath.start$stanzas$stindiv[,Leading := stanza.info$is_lead]
rpath.stanzas.filled <- rpath.stanzas(rpath.start)


# stanza.info <- group.info[!is.na(group.info$Stanza),]
# DM.ecopath$stanzas$stindiv[,First := stanza.info$Stanza_start]
# DM.ecopath$stanzas$stindiv[,Last := stanza.info$Stanza_end]
# DM.ecopath$stanzas$stindiv[,Z := stanza.info$Z]
# DM.ecopath$stanzas$stindiv[,Leading := stanza.info$is_lead]
# DM.ecopath <- rpath.stanzas(DM.ecopath)

names(diet.df)[1] <- 'Group'
rpath.stanzas.filled$diet <- data.table(diet.df)
# names(diet.df) <- sapply(names(diet.df), gsub, pattern='\\.', replacement=' ')
# DM.ecopath$diet <- data.table(diet.df)
test <- rpath(rpath.stanzas.filled)
Rpath.params <- rpath.stanzas.filled
test

# test <- rpath(DM.ecopath)
bqb <- test$Biomass * test$QB
pred.consump <- matrix(0, nrow=nrow(test$DC), ncol=ncol(test$DC))
for(ii in 1:ncol(pred.consump)) {
  pred.consump[,ii] <- test$DC[,ii] * bqb[ii]
}
    
dimnames(pred.consump) <- list(diet.df$Group, names(diet.df[-1]))

production <- test$Biomass * test$PB
names(production) <- rownames(pred.consump)

# For troubleshooting unbalanced model:
# pred.consump[which(test$EE > 1 | test$Biomass < 0), 
#              c('gar', 'stingray', 'dolphins', 'marsh.birds', 'pelicans', 'diving.birds')] %>%
#   View()

