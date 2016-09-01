# DETERMINING IF ANY PHYLOGENETIC AUTOCORRELATION

# INPUT
epi_file <- file.path("0_data", "epi_scores.csv")
ndobj_file <- file.path("0_data", "ndobj.RData")
load(ndobj_file)
epi <- read.csv(file=epi_file)
epi <- epi[!duplicated(epi$txid), ]
epi[['txid']] <- as.character(epi[['txid']])

epi$sstr_pepi <- NA
for(i in 1:nrow(epi)) {
  txid <- epi[['txid']][i]
  sstrs <- ndobj[[txid]][['sstr']]
  sstrs <- which(epi[['txid']] %in% sstrs)
  if(length(sstrs) > 0) {
    epi$sstr_pepi[i] <- mean(sapply(sstrs, function(x) epi[['pepi']][x]))
  }
}

plot(epi$pepi~epi$sstr_pepi)
which(epi[['nm']] == 'Monotremata')
which(epi[['nm']] == 'Theria')
epi[3584,]
is.na(epi[['sstr_pepi']])

which(epi[['pepi']] < -10)
epi[['nm']][epi[['pepi']] < - 10 & !is.na(epi[['sstr_pepi']])]
which(epi[['pepi']] < - 10 & !is.na(epi[['sstr_pepi']]))
