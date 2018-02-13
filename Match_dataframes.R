## Function to check if any entries in 2 data frames match
row.check  <- function(df1, df2){
  x <- apply(df1, 1, paste, collapse = "")
  y <- apply(df2, 1, paste, collapse = "")
  z <- x %in% y
  return(z)
}  