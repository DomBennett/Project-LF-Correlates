# SCRAPE RANGE ESTIMATES FROM IUCN WEBSITE

# START
cat(paste0('\nStage `range` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'ndobj_tools.R'))
source(file.path('tools', 'iucn_dwnld_tools.R'))
source(file.path('tools', 'i_tools.R'))

# PARAMETERS
source('parameters.R')
token <- getToken()

# DIRS
input_file <- file.path("1_download", "epi.RData")
ndobj_file <- file.path("0_data", "ndobj.RData")
output_dir <- "4_range"
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}

# INPUT
load(input_file)
load(ndobj_file)

# LOOP
cat('Getting ranges....\n')
epi[['iucn_range']] <- NA
# limit to birds for now
row_i <- which(epi[['txnmcgrp']] == 'birds')
for(i in row_i) {
  iPrnt(i, max(row_i))
  nms <- getKidNms(epi[i,'txid'])
  res <- rep(NA, length(nms))
  names(res) <- nms
  for(nm in nms) {
    res[nm] <- getRange(nm, token)
  }
  # use the median value by clade
  epi[i, 'iucn_range'] <- median(res, na.rm=TRUE)
}
cat('Done.\n')

# OUTPUT
save(epi, file=file.path(output_dir, "res.RData"))

# END
cat(paste0('\nStage `range` finished at [', Sys.time(), ']\n'))