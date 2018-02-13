## Function for calculating number of polymorphic loci and percent loci polymorphic (PLP) in each population
polymorphic.snps <- function(freq_set){
  polymorphic_snps <- data.frame(population=rownames(freq_set), n_polymorphic=integer(nrow(freq_set)), PLP=numeric(nrow(freq_set)))
  # Number of loci in each population that are polymorphic
  for(i in 1:nrow(freq_set)){
    polymorphic_snps$n_polymorphic[i] <- length(freq_set[i,which(freq_set[i,]<1 & freq_set[i,]>0)])
    polymorphic_snps$PLP[i] <- (polymorphic_snps$n_polymorphic[i]/ncol(freq_set))*100
  }
  return(polymorphic_snps)
}