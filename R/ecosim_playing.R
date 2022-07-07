

test.sim <- rsim.scenario(test, DM.ecopath, years=50)

test.sim.fishery <- adjust.fishing(test.sim, 'EFFORT', gear='Fishery', year=20, value=0)
test.sim.preds <- adjust.fishing(test.sim, parameter = 'FRATE', gear='Fishery', year=20, value=.1,
                                 group = 'dolphins')
test.sim.preds <- adjust.fishing(test.sim.preds, parameter = 'FRATE', gear='Fishery', year=20, value=.5,
                                 group = 'sea birds')
test.sim.both <- adjust.fishing(test.sim.preds, 'EFFORT', gear='Fishery', year=20, value=0)

test.run.fishery <- rsim.run(test.sim.fishery, method='RK4', years=50)
test.run.preds <- rsim.run(test.sim.preds, method='RK4', years=50)
test.run.both <- rsim.run(test.sim.both, method='RK4', years=50)

png('Figs/ts-example.png', res=1000, height=7, width=7, units='in', bg = NA)
plot(test.run.both$out_BB[200:300,'adult gulf menhaden'], type='l', ann=F, axes=F, lwd=2,
     ylim=c(0, .2))
box(bty='l')
mtext('Time', 1, .5, cex=1.5)
mtext('Relative biomass', 2, .5, cex=1.5)
dev.off()

ecosim.list <- list(fish=test.run.fishery, pred=test.run.preds, both=test.run.both)
selected.spp <- lapply(ecosim.list, function(x) x$out_BB[, c('adult brown shrimp', 'adult white shrimp', 
                                                             'adult red drum',
                                                             'adult Atlantic croaker',
                                                             'adult blue crab',
                                                             'adult gulf menhaden')])
increases <- sapply(selected.spp, function(y) 
  apply(y, 2, function(x) max(x[(12*20):(12*23)])/mean(x[12*20])))


increases.adj <- increases
increases.adj['adult gulf menhaden', c(1,3)] <- 1.6
fishing.only <- increases.adj
fishing.only[,2:3] <- 1

three.cols <- c('#648A0E', '#E5CB00', '#2E3AB8')

png('Figs/fishing.png', res=1000, height=7, width=9, units='in')
par(mar=c(6,4,3,2)+.1)
barplot(100*(t(fishing.only)-1), names.arg = rep('', 6), 
        axes=FALSE, ylim=c(0,60), beside=T, col=three.cols)
mtext('% increase from pre-spill biomass', 2, 2.5, cex=1.5)
axis(2, at=c(0:5*10, 60), labels = c(0:5*10, 213), las=1, cex.axis=1.25)
text(c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5) + 1, rep(-1,6), srt=45,
     labels = c('Brown shrimp', 'White shrimp', 'Red drum', 
                'Atlantic croaker', 'Blue crab', 'Menhaden'), 
     xpd=NA, pos=2, cex=1.25)
dev.off()

png('Figs/fishing-pred.png', res=1000, height=7, width=9, units='in')
par(mar=c(6,4,3,2)+.1)
barplot(100*(t(increases.adj)-1), ylim=c(0,60), beside=T, col=three.cols)
mtext('% increase from pre-spill biomass', 2, 2.5, cex=1.5)
axis(2, at=c(0:5*10, 60), labels = c(0:5*10, 213), las=1, cex.axis=1.25)
text(c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5) + 1, rep(-1,6), srt=45,
     labels = c('Brown shrimp', 'White shrimp', 'Red drum', 
                'Atlantic croaker', 'Blue crab', 'Menhaden'), 
     xpd=NA, pos=2, cex=1.25)
dev.off()

temp <- filter(diet.df, Group=='adult sunfishes') %>% 
  as.vector() 
temp[which(temp>0)]

compare.run <- rsim.run(test.sim, method='RK4', years=10)
rsim.plot(compare.run, spname='adult menhaden')
