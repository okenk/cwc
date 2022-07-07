cex.amt <- 2

setwd("C:/Users/Kiva Oken/Dropbox/Presentations/UCDavis2019")
png('direct-mortality.png', res=1000, height=7, width=9, units='in')
par(mar=c(5,6,2,1)+.1)
barplot(t(mort.mat), beside=TRUE, col=c('grey60', '#1D8D2A', '#AA7601', 'darkblue'),
        names.arg = rep('', 10), axes=FALSE)
axis(2, las=2, cex.axis=cex.amt)
mtext('Fraction of total mortality from source', 2, 4, cex=cex.amt)
text(5*0:9 +3.5, rep(-.01,6), srt=45,
     labels = '',
     # c('A. Shrimp', 'J. Shrimp',
     #            'A. Red drum', 'J. Red drum',
     #            'A. sm. Sciaenids', 'J. sm. Sciaenids',
     #            'A. Blue crab', 'J. Blued crab',
     #            'A. Menhaden', 'J. Menhaden'), 
     xpd=NA, pos=2, cex=cex.amt)
dev.off()

png('indirect-preds.png', res=1000, height=7, width=9, units='in')
par(mar = c(1,7.1,2,1)+.1, oma = c(4,0,0,0))
apply(10*Jr.inv.std, c(1,2), median) %>%
  t() %>%
  barplot(beside=TRUE, col=c('grey60', '#1D8D2A', '#AA7601'), names.arg = rep('', 10), axes=FALSE,
          ylim=1.05*range(pred.quantiles))
for(pred in 0:2) {
  segments(1.5 + pred + 4*0:9, pred.quantiles[1,,pred+1],
           1.5 + pred + 4*0:9, pred.quantiles[2,,pred+1], lwd=2)
}
axis(2, las=2, cex.axis=cex.amt)
mtext('% change in biomasss per\n10% increase in predator mortality', 2, 3.75, cex=cex.amt)
text(4*0:9 +3.5, -20, srt=45,
     labels = '',
     # c('A. Shrimp', 'J. Shrimp',
     #            'A. Red drum', 'J. Red drum',
     #            'A. sm. Sciaenids', 'J. sm. Sciaenids',
     #            'A. Blue crab', 'J. Blue crab',
     #            'A. Menhaden', 'J. Menhaden'), 
     xpd=NA, pos=2, cex=cex.amt)
dev.off()

png('indirect-fishing.png', res=1000, height=7, width=9, units='in')
par(mar = c(1,7.1,2,1)+.1, oma = c(4,0,0,0))
apply(-fishing.res*100, 1, median) %>%
  barplot(names.arg = rep('', 10), ylim=1.05*range(fishing.quantiles), axes=FALSE, col='darkblue')
segments(.7 + 1.2*0:9, fishing.quantiles[1,], .7 + 1.2*0:9, fishing.quantiles[2,], lwd=2)
axis(2, cex.axis=cex.amt, las=2)
mtext('% change in biomass per\n10% reduction in fishing effort', 2, 3.75, cex=cex.amt)
text(.9 + 1.2*0:9, -30, srt=45,
     labels = '',
     # c('A. Shrimp', 'J. Shrimp',
     #            'A. Red drum', 'J. Red drum',
     #            'A. sm. Sciaenids', 'J. sm. Sciaenids',
     #            'A. Blue crab', 'J. Blue crab',
     #            'A. Menhaden', 'J. Menhaden'), 
     xpd=NA, pos=2, cex=cex.amt)
dev.off()

png('ecopath-plot.png', res=100, height=7, width=7, units='in')
webplot(test, labels = TRUE)
dev.off()
