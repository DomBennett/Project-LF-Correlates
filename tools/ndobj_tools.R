getKidNms <- function(txid) {
  # Return the names of the children of a txid
  kds <- ndobj[[txid]][['kids']]
  if(kds[1] == "none") {
    return(ndobj[[txid]][['nm']][['scientific name']])
  }
  nms <- vector(length=length(kds))
  for(i in 1:length(kds)) {
    nms[i] <- ndobj[[kds[i]]][['nm']][['scientific name']]
  }
  nms
}

getLng <- function(txid, lng=NULL) {
  lng <- c(ndobj[[txid]][['nm']][['scientific name']], lng)
  prid <- ndobj[[txid]][['prid']]
  if(is.null(prid)) {
    return(lng)
  }
  getLng(prid, lng)
}
