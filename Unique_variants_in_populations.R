## Function for calculating number of unique SNPs per population given a SNP matrix (ignores missing data)
unique.snps <- function(snpmat_set){
  snp_macount_gt0 <- data.frame(count_gt0=integer(nrow(snpmat_set)))
  unique_snps <- data.frame(population=unique(popdata$population), n_unique_snps=0)
  for(i in 1:nrow(snpmat_set)){
    snp_count_tmp <- snpmat_set[i,]
    snp_count_tmp[snp_count_tmp==9] <- NA
    snp_macount <- aggregate(snp_count_tmp, by=list(popdata$population), FUN=mean, na.rm=TRUE)
    ## If minor allele occurs in only one population, attribute unique SNP to corresponding population
    if(length(which(snp_macount$x!=0))==1){
      unique_snps$n_unique_snps[which(unique_snps$population==snp_macount$Group.1[which(snp_macount$x!=0)])] <- unique_snps$n_unique_snps[which(unique_snps$population==snp_macount$Group.1[which(snp_macount$x!=0)])]+1
    }
    ## If major allele occurs in only one population, attribute unique SNP to corresponding population
    if(length(which(snp_macount$x==0))==1){
      unique_snps$n_unique_snps[which(unique_snps$population==snp_macount$Group.1[which(snp_macount$x==0)])] <- unique_snps$n_unique_snps[which(unique_snps$population==snp_macount$Group.1[which(snp_macount$x==0)])]+1
    }
  }
  return(unique_snps)
}