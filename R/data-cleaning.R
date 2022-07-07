require(plyr)
require(dplyr)
require(tidyr)

bulk.si <- read.csv('Data/bulk_SI.csv', na.strings = 'not measured')[,1:9]

sample.key <- read.csv('Data/sample_key.csv', na.strings = 'na')  
sample.key$Latitude <- gsub('°', '', sample.key$Latitude) %>% as.numeric()
sample.key$Longitude <- gsub('°', '', sample.key$Longitude) %>% as.numeric()
names(sample.key)[7] <- 'Habitat.Location'

# For old version of spreadsheet, did not want to lose:
fatty.acids <- readLines('Data/fatty_acids_old.csv')[-(2:4)] %>% textConnection() %>%
  read.csv(na.strings = 'not measured')

fatty.acids <- read.csv('Data/fatty_acids.csv', stringsAsFactors = FALSE) %>%
  filter(Name != '') %>%  # remove unnamed FA
  t() %>% data.frame(stringsAsFactors = FALSE)
codable.names <- 
names(fatty.acids) <- gsub(":", ".", fatty.acids[1,])

fatty.acids <- cbind(rownames(fatty.acids), fatty.acids)
rownames(fatty.acids) <- NULL

View(fatty.acids)

names(fatty.acids) <- fatty.acids[1,]
data.frame(fatty.acids) %>% slice(-1) %>% View()

names(fatty.acids)
names(fatty.acids) <- gsub('ap.', '', names(fatty.a
fa.names <- names(fatty.acids)[-(1:5)]
area.percent <- fa.names[grep('ap', fa.names)]  
raw.val <- fa.names[grep('X', fa.names)]

no.ap <- gsub('ap.', '', fa.names)
no.x <- gsub('X', '', raw.val)

repeated <- no.ap[no.ap %in% no.x]
no.x[no.x %in% repeated]

biomarkers <- select(sample.key, -Collection.Notes, -Other.ID.Number, -Latitude, -Longitude) %>% left_join(bulk.si) %>% 
  rename(si.lab = Laboratory) %>% left_join(fatty.acids) %>% rename(fa.lab = Laboratory) %>%
  mutate(d13C.corrected = ifelse(C.N<3.22, d13C, d13C - 0.00639*(3.22 - C.N)/C.N))
# Lipid correction is the same one Talia did in her dissertation (pg. 152). 
# Makes basically no difference.

require(ggplot2)
require(ggsidekick)

png('Figs/si-plot.png', width=5, height=4, units='in', res=300)
filter(biomarkers, grepl('killifish', Common.Name) | Common.Name=='Blue crab' |
         Node.Code == 'Carn. fish') %>%
  mutate(new.code = ifelse(grepl('killifish', Common.Name), 'Killifish', as.character(Node.Code))) %>%
  ggplot() + geom_point(aes(x=d13C, y=d15N, col=new.code), cex=3) + theme_sleek() +
  xlab(expression(paste(delta, '13C'))) + ylab(expression(paste(delta, '15N'))) +
  guides(col=guide_legend(title='Functional group'))
dev.off()

  filter(biomarkers, grepl('killifish', Common.Name)) %>%
  # group_by(Habitat.Location, Common.Name) %>% summarize(d13C=mean(d13C, na.rm=T), d15N = mean(d15N, na.rm=T)) %>%
  ggplot() + geom_point(aes(x=d13C, y=d15N, col=Habitat.Location), cex=3)

filter(biomarkers, Common.Name=='Blue crab') %>% 
  filter(!grepl('\\D', Carapace.width..mm.)) %>% 
  mutate(size = as.numeric(as.character(Carapace.width..mm.))) %>%
  ggplot() + geom_point(aes(x=d13C, y=d15N, col=size), cex=3)

filter(biomarkers, Node.Code=='Blue crabs') %>% nrow()
