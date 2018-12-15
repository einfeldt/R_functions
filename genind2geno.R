# Function to convert genind object to geno format object (can be written to file)
genind2geno <- function(genObj){
  loc_names <- gsub("\\..*","",colnames(genObj@tab))[seq(1,length(colnames(genObj@tab)),2)]
  ind_names <- indNames(genObj)
  snpmatObj <- matrix(nrow=nLoc(genObj), ncol=nInd(genObj))
  rownames(snpmatObj) <- loc_names
  colnames(snpmatObj) <- ind_names
  for(i in 1:length(locNames(genObj))){
    for(j in 1:length(indNames(genObj))){
      loc_name <- loc_names[i]
      ind_name <- ind_names[j]
      allele1 <- genObj@tab[ind_name,paste(loc_name,".0", sep="")]
      allele2 <- genObj@tab[ind_name,paste(loc_name,".1", sep="")]
      if(is.na(allele1) | is.na(allele2)){
        snpmatObj[i,j] <- 9
      }else if(allele1==0 & allele2==0){
        snpmatObj[i,j] <- 9
      }else if(allele1==1 & allele2==1){
        snpmatObj[i,j] <- 1
      }else if(allele1==0 & allele2==2){
        snpmatObj[i,j] <- 2
      }else if(allele1==2 & allele2==0){
        snpmatObj[i,j] <- 0
      }
    }
  }
  return(snpmatObj)
}
