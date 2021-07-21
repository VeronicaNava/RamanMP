# Align two spectra with different spectra resolution
# First column must be frequency data

spectra.alignment <- function(db1, db2, t) {
  #Verify which database has the higher spectral resolution
  if((db1[2,1]-db1[1,1]) < (db2[2,1]-db2[1,1])){
    for(i in 1:nrow(db2))
      db1[,1][db2[,1][i] >= db1[,1] - t & db2[,1][i] <= db1[,1] + t] <- db2[,1][i] #Alignment based on the t value of tolerance
    alignment<- merge(db1, db2, by = 1, all = TRUE) #Merging the two databases
  }
  else
  {  for(i in 1:nrow(db1))
    db2[,1][db1[,1][i] >= db2[,1] - t & db1[,1][i] <= db2[,1] + t] <- db1[,1][i] #Alignment based on the t value of tolerance
  alignment <- merge(db2, db1, by = 1, all = TRUE) #Merging the two databases

  }

}
