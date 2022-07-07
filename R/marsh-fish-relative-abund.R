marsh.data <- read.csv('Data/FishCompositionandAbundance_datasubmission_GRIIDC.csv')
require(dplyr)
require(tidyr)
require(ggplot2)

# functional groups:
# carnivorous marsh fish: Fundulus.pulvereus, Fundulus.grandis, Menidia.spp., Bathygobius.soporator, 
# Feather.Fin.Goby, Gobiosoma.bosci, Gobiosoma.spp, 

# Herbivorous marsh fish: Cyprinodon.spp.

# Omnivorous marsh fish: Adinia.xenica, Mugil.curema, Poecilia.spp.
marsh.data %>%
  mutate(fundulus = (Fundulus.pulvereus + Fundulus.grandis) /  
           ifelse(Sampling.Gear=='Leonard Trap' | Sampling.Gear=='Minnow Trap',
                                                                      No..of.Traps * Soak.Time..hrs., 1),
         other = (Menidia.spp. + Bathygobius.soporator + Feather.Fin.Goby + Gobiosoma.bosci + Gobiosoma.spp +
           Cyprinidon.spp. + Adinia.xenica + Mugil.curema + Poecilia.spp.) /
           ifelse(Sampling.Gear=='Leonard Trap' | Sampling.Gear=='Minnow Trap',
                  No..of.Traps * Soak.Time..hrs., 1)) %>%
  group_by(Sampling.Gear) %>%
  # filter(Sampling.Gear == 'Seine') %>%
  summarize(fundulus = mean(fundulus, na.rm = TRUE),
            other = mean(other, na.rm = TRUE))

rel.abund <- marsh.data %>%
  mutate(carn.marsh.fish = (Fundulus.pulvereus + Fundulus.grandis + Menidia.spp. + Bathygobius.soporator +
                               Feather.Fin.Goby + Gobiosoma.bosci + Gobiosoma.spp) / 
           ifelse(Sampling.Gear=='Leonard Trap' | Sampling.Gear=='Minnow Trap',
                  No..of.Traps * Soak.Time..hrs., 1),
         herb.marsh.fish = Cyprinidon.spp. /
           ifelse(Sampling.Gear=='Leonard Trap' | Sampling.Gear=='Minnow Trap',
                  No..of.Traps * Soak.Time..hrs., 1),
         omn.marsh.fish = (Adinia.xenica + Mugil.curema + Poecilia.spp.) /
           ifelse(Sampling.Gear=='Leonard Trap' | Sampling.Gear=='Minnow Trap',
                  No..of.Traps * Soak.Time..hrs., 1)) %>%
  group_by(Sampling.Gear) %>%
  # filter(Sampling.Gear == 'Seine') %>%
  summarize(carn.marsh.fish = mean(carn.marsh.fish, na.rm = TRUE),
            herb.marsh.fish = mean(herb.marsh.fish, na.rm = TRUE),
            omn.marsh.fish = mean(omn.marsh.fish, na.rm = TRUE)) %>%
  apply(1, function(x) as.numeric(x[2:4]) / sum(as.numeric(x[2:4]))) %>%

rownames(rel.abund) <- c('carn.marsh.fish', 'herb.marsh.fish', 'omn.marsh.fish')  
colnames(rel.abund) <- c('Dip Net', 'Leonard Trap', 'Minnow Trap', 'Seine', 'Tupper')