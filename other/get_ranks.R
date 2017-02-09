# LOOK UP ALL THE NAMED RANKS IN NDOBJ

# FUNCTIONS
isOrder <- function(txid) {
  grepl('^order$', ndobj[[txid]][['rank']])
}
isFamily <- function(txid) {
  grepl('^family$', ndobj[[txid]][['rank']])
}
getNm <- function(txid) {
  ndobj[[txid]][['nm']][['scientific name']]
}

# INPUT
cat('Reading....\n')
load(file.path("0_data", "ndobj.RData"))

# SEARCH
cat('Searching....\n')
txids <- ls(ndobj)
cat('.... orders\n')
orders <- plyr::mdply(txids, .fun=isOrder, .progress='time')[,2]
cat('.... families\n')
families <- plyr::mdply(txids, .fun=isFamily, .progress='time')[,2]

# GET NAMES
cat('Getting....\n')
orders <- sapply(txids[orders], getNm)
families <- sapply(txids[families], getNm)

# SAVE
cat('Saving....\n')
save(orders, file=file.path('0_data', 'orders.RData'))
save(families, file=file.path('0_data', 'families.RData'))
