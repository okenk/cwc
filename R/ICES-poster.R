require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggsidekick)

three.cols <- c('#648A0E', '#E5CB00', '#2E3AB8')

bulk.si <- read.csv('Data/bulk_SI.csv', na.strings = 'not measured')[,1:9]

sample.key <- read.csv('Data/sample_key.csv', na.strings = 'na')  
sample.key$Latitude <- gsub('°', '', sample.key$Latitude) %>% as.numeric()
sample.key$Longitude <- gsub('°', '', sample.key$Longitude) %>% as.numeric()
names(sample.key)[7] <- 'Habitat.Location'

fatty.acids <- readLines('Data/fatty_acids_old.csv')[-(2:4)] %>% textConnection() %>%
  read.csv(na.strings = 'not measured')

biomarkers <- select(sample.key, -Collection.Notes, -Other.ID.Number, -Latitude, -Longitude) %>% left_join(bulk.si) %>% 
  rename(si.lab = Laboratory) %>% left_join(fatty.acids) %>% rename(fa.lab = Laboratory)


require(stats)
for.pca <- filter(biomarkers, !is.na(X14.0), 
              grepl('killifish', Common.Name) | Common.Name=='Blue crab' | Node.Code == 'Carn. fish') %>%
  select(X14.0, X16.0, X17.0, X18.0, X16.1.7, X18.1.9c, X18.1.7, 
                  X18.2.6c, X20.3.6, X20.4.6, X20.5.3, X22.5.6, X22.5.3, X22.6.3) %>%
  as.matrix() %>% apply(c(1,2), function(x) ifelse(x==0, .001, x))/100
  
pca <- boot::logit(for.pca) %>% prcomp(center=TRUE, scale.=TRUE)

png('Figs/fa-plot.png', width=7, height=5, units='in', res=300)
filter(biomarkers, !is.na(X14.0), 
       grepl('killifish', Common.Name) | Common.Name=='Blue crab' | Node.Code == 'Carn. fish') %>%
  cbind(pca$x[,1:2]) %>%
  mutate(new.code = ifelse(grepl('killifish', Common.Name), 
                           'Killifish', as.character(Node.Code))) %>%
  ggplot() + geom_point(aes(x=PC1, y=PC2, col=new.code), cex=3) +
  theme_sleek() +
  guides(col=guide_legend(title='Functional group')) +
  scale_color_manual(values=three.cols) +
  theme(text=element_text(size=18)) +
  ggtitle('Fatty acids')
dev.off()


pca$x[,1:2]

# Diamond and Gulf killifish definitely overlap. Some evidence Bayou have lower 
# Carbon signature than the other 2.
png('Figs/si-plot.png', width=7, height=5, units='in', res=300)
filter(biomarkers, grepl('killifish', Common.Name) | Common.Name=='Blue crab' |
         Node.Code == 'Carn. fish') %>%
  mutate(new.code = ifelse(grepl('killifish', Common.Name), 
                           'Killifish', as.character(Node.Code))) %>%
  ggplot() + geom_point(aes(x=d13C.corrected, y=d15N, col=new.code), cex=3) + 
  theme_sleek() +
  xlab(expression(paste(delta, '13C'))) +
  ylab(expression(paste(delta, '15N'))) +
  ggtitle('Stable isotopes') +
  guides(col=guide_legend(title='Functional group')) +
  scale_color_manual(values=three.cols) +
  theme(text=element_text(size=18))
dev.off()
