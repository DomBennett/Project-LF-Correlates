# DOWNLOAD IUCN DATA

# START
cat(paste0('\nStage `download` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'i_tools.R'))
source(file.path('tools', 'iucn_dwnld_tools.R'))
source(file.path('tools', 'ndobj_tools.R'))

# PARAMETERS
source('parameters.R')
token <- getToken()

# DIRS
output_dir <- '1_download'
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}
epi_file <- file.path("0_data", "epi_scores.csv")
ndobj_file <- file.path("0_data", "ndobj.RData")

# INPUT
load(ndobj_file)
epi <- read.csv(file=epi_file)
epi <- epi[!duplicated(epi$txid), ]
epi[['txid']] <- as.character(epi[['txid']])

# SET IS
# not point searching for things not on IUCN
is <- which(epi[['txnmcgrp']] %in% c('vertebrates', 'bony_fish', 'plants',
                                     'lepidosaurs', 'birds', 'amphibia', 'mammals'))

# ADD CATES
cat('    Searching IUCN for extinction risk categories ....')
ignr <- NULL  # ignore all DD species
epi$cate <- NA
for(i in is) {
  iPrnt(i, max(is))
  nms <- getKidNms(epi[i,'txid'])
  res <- rep(NA, length(nms))
  names(res) <- nms
  for(nm in nms) {
    #cat('Searching [', nms[j], '] ....\n', sep="")
    cate <- getIUCNCat(nm, token)
    if(class(cate) == "list" && length(cate[['result']]) > 0) {
      cate <- cate[['result']][[1]][['category']]
      if(cate == "DD") {
        ignr <- c(ignr, nm)
      } else {
        res[[nm]] <- cateAsNum(cate)
      }
    }
  }
  epi[['cate']][i] <- mean(res, na.rm=TRUE)
}
cat("Done, found data for [", sum(!is.na(epi[['cate']])), '/',
    nrow(epi), "].\n", sep="")

# ADD NHABITATS
cat('    Searching IUCN for nhabitats ....')
epi[['nhbbts']] <- NA
for(i in is) {
  iPrnt(i, max(is))
  nms <- getKidNms(epi[i,'txid'])
  nms <- nms[!nms %in% ignr]
  res <- rep(NA, length(nms))
  names(res) <- nms
  for(nm in nms) {
    hbbts <- getIUCNHbbts(nm, token)
    if(class(hbbts) == "list" && length(hbbts[['result']]) > 0) {
      nhbbts <- sum(unlist(lapply(hbbts[['result']],
                                  function(x) x[['suitability']] == "Suitable")))
      res[[nm]] <- nhbbts
    }
  }
  epi[['nhbbts']][i] <- mean(res, na.rm=TRUE)
}
cat("Done, found data for [", sum(!is.na(epi[['nhbbts']])), '/',
    nrow(epi), "].\n", sep="")

# ADD NC
cat('    Searching IUCN for ncountries ....')
epi[['ncntrs']] <- NA
for(i in is) {
  iPrnt(i, max(is))
  nms <- getKidNms(epi[i,'txid'])
  nms <- nms[!nms %in% ignr]
  res <- rep(NA, length(nms))
  names(res) <- nms
  for(nm in nms) {
    cntrs <- getIUCNCntrs(nm, token)
    if(class(cntrs) == "list" && length(cntrs[['result']]) > 0) {
      ncntrs <- sum(unlist(lapply(cntrs[['result']],
                                  function(x) x[['origin']] == "Native")))
      res[[nm]] <- ncntrs
    }
  }
  epi[['ncntrs']][i] <- mean(res, na.rm=TRUE)
}
cat("Done, found data for [", sum(!is.na(epi[['ncntrs']])), '/',
    nrow(epi), "].\n", sep="")

# HABITAT AND ECOLOGY AS WORDS
wrds <- vector("list", length=nrow(epi))
cat('    Searching IUCN for descriptions ....')
for(i in is) {
  iPrnt(i, max(is))
  nms <- getKidNms(epi[i,'txid'])
  res <- NULL
  for(nm in nms) {
    nrrtv <- getIUCNNrrtv(nm, token)
    if(class(nrrtv) == "list" && length(nrrtv[['result']]) > 0 &&
       !is.null(nrrtv[['result']][[1]][['habitat']])) {
      nrrtv <- nrrtv[['result']][[1]][['habitat']]
      nrrtv <- gsub("<.*?>", "", nrrtv)  # remove html tags
      res <- c(res, cleanWrds(nrrtv))
    }
  }
  if(length(res) > 0) {
    wrds[[i]] <- unique(res)
  }
}
wwrds <- which(sapply(wrds, length) > 0)
cat("Done, found data for [", length(wwrds), "/", nrow(epi),
    "].\n", sep="")
cat('Saving....\n')
save(epi, wrds, wwrds, file=file.path(output_dir, 'wrds.RData'))
cat('Done.\n')

# HABITATS
cds <- hbbts <- vector("list", length=nrow(epi))
cat('    Searching IUCN for habitats ....')
for(i in is) {
  iPrnt(i, max(is))
  nms <- getKidNms(epi[i,'txid'])
  res_h <- res_c <- list()
  cc <- 0
  for(nm in nms) {
    hs <- getIUCNHbbts(nm, token)
    if(class(hs) == "list" && length(hs[['result']]) > 0) {
      ss <- sapply(hs[['result']], function(x) x[['suitability']])
      cs <- sapply(hs[['result']], function(x) x[['code']])
      hs <- sapply(hs[['result']], function(x) x[['habitat']])
      res_h <- c(res_h, list(hs[ss == "Suitable"]))
      res_c <- c(res_c, list(cs[ss == "Suitable"]))
    }
  }
  if(length(res_h) > 0) {
    hbbts[[i]] <- res_h
    cds[[i]] <- res_c
  }
}
whbbts <- which(sapply(hbbts, length) > 0)
cat("Done, found data for [", length(whbbts), "/",
    length(hbbts), "].\n", sep="")
cat('Saving....\n')
save(epi, hbbts, cds, whbbts, file=file.path(output_dir, 'hbbts.RData'))
cat('Done.\n')

# OUTPUT
save(epi, file=file.path(output_dir, 'epi.RData'))

# END
cat(paste0('\nStage `download` finished at [', Sys.time(), ']\n'))