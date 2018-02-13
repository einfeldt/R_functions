## Function for converting longitudinal and latitudinal coordinates to a matrix of geographic distances between samples
# coord_obj is a dataframe with samples in rows, latitudes in column 1, longitudes in column 2
geographic.distance <- function(coord_obj){
  Dgeo_tmp <- matrix(0, ncol=length(coord_obj[,1]), nrow=length(coord_obj[,1]))
  for(i in 1:nrow(Dgeo_tmp)){
    for(j in 1:ncol(Dgeo_tmp)){
      Dgeo_tmp[i,j] <- distGeo(coord_obj[i,], coord_obj[j,])/1000
    }
  }
  return(as.dist(Dgeo_tmp))
}
