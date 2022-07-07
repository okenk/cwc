Rpath.obj <- test
line.col <- 'grey'
library(ggplot2)
library(tibble)
library(dplyr)

pointmap <- data.table(GroupNum = 1:length(Rpath.obj$TL), 
                       Group    = Rpath.obj$Group, 
                       type     = Rpath.obj$type, 
                       TL       = Rpath.obj$TL, 
                       Biomass  = Rpath.obj$Biomass)

pointmap[TL < 2,               TLlevel := 1]
pointmap[TL >= 2.0 & TL < 3.0, TLlevel := 2]
pointmap[TL >= 3.0 & TL < 3.5, TLlevel := 3]
pointmap[TL >= 3.5 & TL < 4.0, TLlevel := 4]
pointmap[TL >= 4.0 & TL < 4.5, TLlevel := 5]
pointmap[TL >= 4.5 & TL < 5.0, TLlevel := 6]
pointmap[TL >= 5.0,            TLlevel := 7]

pointmap <- pointmap[type < 3, ]
nTL <- table(pointmap[, TLlevel])
pointmap[, n := nTL[which(names(nTL) == TLlevel)], by = TLlevel]
pointmap[, x.space  := 1 / n]
pointmap[, x.offset := x.space / 2]
x.count.all <- c()
for(i in 1:max(pointmap[, TLlevel])){
  x.count <- pointmap[TLlevel == i, list(Group)]
  if(length(x.count[, Group] > 0)){
    for(j in 1:nrow(x.count)){
      x.count[j, x.count := j] 
    }
    x.count.all <- rbind(x.count.all, x.count)
  }
}
pointmap <- merge(pointmap, x.count.all, by = 'Group', all.x = T)
pointmap[x.count == 1, x.pos := x.offset + rnorm(1, 0, 0.01)]
pointmap[x.count != 1, x.pos := x.offset + x.space * (x.count - 1) + rnorm(1, 0, 0.01)]
pointmap[, c('TLlevel', 'n', 'x.offset', 'x.space', 'x.count') := NULL]

tot.catch <- Rpath.obj$Landings + Rpath.obj$Discards
pred      <- pointmap[!type %in% 1:2, GroupNum] # pred is all the consumers

connection.mat <- matrix(NA, nrow = sum(Rpath.obj$DC != 0), ncol = 4,
                           dimnames = list(pair = NULL, 
                                           to.plot = c('pred.x', 'pred.y',
                                                       'prey.x', 'prey.y')))
row.num <- 0
for(i in pred){
  pred.x <- pointmap[GroupNum == i, x.pos] 
  pred.y <- pointmap[GroupNum == i, TL]
  if(pointmap[GroupNum == i, type] == 0){
    prey <- which(Rpath.obj$DC[, i] > 0)
  }
  if(pointmap[GroupNum == i, type] == 3){
    gear.num <- i - (Rpath.obj$NUM_GROUPS - Rpath.obj$NUM_GEARS)
    prey <- which(tot.catch[, gear.num] > 0)
  }
  nprey <- length(prey)
  prey.x <- pointmap[GroupNum %in% prey, x.pos]
  prey.y <- pointmap[GroupNum %in% prey, TL]
  
  connection.mat[row.num + 1:nprey, 'pred.x'] <- pred.x
  connection.mat[row.num + 1:nprey, 'pred.y'] <- pred.y
  connection.mat[row.num + 1:nprey, 'prey.x'] <- prey.x
  connection.mat[row.num + 1:nprey, 'prey.y'] <- prey.y
  
  row.num <- row.num + nprey
}

png('Figs/ggrepel_plot.png', width = 7, height = 7, units = 'in', res = 500)
as_tibble(pointmap) %>%
  mutate(Group = gsub('croaker.spot.perch', 'sm.sciaenids', Group)) %>%
  ggplot(aes(x = x.pos, y = TL)) +
  geom_segment(aes(x = pred.x, y = pred.y, xend = prey.x, yend = prey.y),
               col = 'grey', alpha = 0.5,
               data = as_tibble(connection.mat)) +
  geom_point(col = 'grey50') +
  ggrepel::geom_text_repel(aes(label = Group), max.overlaps = 15) +
  labs(x = '', y = 'Trophic Level') +
  theme_classic() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
dev.off()

# groups with juveniles higher TL than adults:
# pinfish, sm sciaenids, mullet (looks like major error?), menhaden (another big one)
