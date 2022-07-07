require(dplyr)
dc <- read.csv('Diet.csv')
dc.mat <- select(dc, -Prey...Predator) %>% as.matrix()
rownames(dc.mat) <- colnames(dc.mat)
View(dc.mat)


# I AM A GENIUS!!!!!!!!!!!!!!!!

# cannibalism.vec must be numeric indices. Is actually a vector
# consumer can either be numeric index of consumer in diet matrix or its name
# producers is a vector of the indices for producers in the diet matrix
calc_carbon_sources <- function(dc.mat, consumer, producers, cannibalism.vec) {
  diet.comp <- dc.mat[,consumer]
  prey.items <- which(diet.comp > 0)
  carbon.sources <- rep(0, length(producers))
  names(carbon.sources) <- names(producers)

  for(prey in prey.items) {
    if(prey %in% producers) {
      # if the prey item is a producer, add it to the carbon sources
      carbon.sources[which(producers==prey)] <- carbon.sources[which(producers==prey)] +
        diet.comp[prey] 
    } else if(!(prey %in% cannibalism.vec)) {
      # if the prey item is a consumer, and the prey item is not in the current chain
      # of consumers, recursively calculate the prey's basal carbon, and add that
      carbon.sources <- carbon.sources + diet.comp[prey]*
        calc_carbon_sources(dc.mat, consumer = prey, producers, c(cannibalism.vec, consumer))
    }
    # print(cannibalism.list)
  }
  # normalize carbon sources vector
  # normalization necessary because of cannibalism
  return(carbon.sources/sum(carbon.sources))
}



producers <- which(colSums(dc.mat)==0) # This includes detritus too.
recursive.func(dc.mat, 33, producers) 
