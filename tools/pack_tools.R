addToEPI <- function(epi, vrbls, vrbl_data, species_nms, row_i,
                     categorical=FALSE) {
  # add new data to epi using data extracted from variable dataset and
  # name look-up with node obj
  # using MEDIAN for continuous
  # using COUNT for categorical
  for(i in row_i) {
    nms <- getKidNms(epi[i,'txid'])
    res <- matrix(NA, ncol=length(vrbls),
                  nrow=length(nms))
    rownames(res) <- nms
    colnames(res) <- vrbls
    for(nm in nms) {
      pull <- species_nms == nm
      if(sum(pull) == 1) {
        for(vrbl in vrbls) {
          res[nm, vrbl] <- vrbl_data[pull, vrbl]
        }
      }
    }
    if(categorical) {
      epi[i, vrbls] <- apply(res, 2, function(x){
        srtd <- sort(table(x, useNA='ifany'), decreasing=TRUE)
        names(srtd)[[1]]
      })
    } else {
      epi[i, vrbls] <- apply(res, 2, median, na.rm=TRUE)
    }
  }
  epi
}

# NCBI
cache_dir <- "caches"
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}
cache_dir <- file.path("caches", "ncbi_tax")
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}

srchEntrez <- function(txid, rank) {
  res <- NA
  fl <- file.path(cache_dir, paste0(txid, '_', rank, '.RData'))
  if(file.exists(fl)) {
    load(fl)
    return(res)
  }
  dwnld <- rentrez::entrez_fetch(db='taxonomy', id=txid,
                                 rettype='xml', parsed=TRUE)
  tax_list <- XML::xmlToList(dwnld)
  lng <- tax_list$Taxon$LineageEx
  for(i in 1:length(lng)) {
    if(grepl(paste0('^', rank, '$'), lng[[i]]$Rank)) {
      res <- lng[[i]]$ScientificName
    }
  }
  save(res, file=fl)
  res
}