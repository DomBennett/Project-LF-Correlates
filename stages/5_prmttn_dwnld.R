# DOWNLOAD IUCN DATA ON LIVING FOSSILS AND NULL SPECIES

# START
cat(paste0('\nStage `permutation download` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'i_tools.R'))
source(file.path('tools', 'iucn_dwnld_tools.R'))
source(file.path('tools', 'ndobj_tools.R'))

# PARAMETERS
nlfs <- 1000
nitrtns <- 999
token <- getToken()

# DIRS
output_dir <- '5_prmttn_dwnld'
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

# LOOP THROUGH ANALYSIS GROUP AND DOWNLOAD
cat('Looping through each analysis group ....\n')
res <- list()
ignr <- NULL  # ignore all DD species
grps <- list('verts'=c('vertebrates', 'bony_fish', 'plants',
                       'lepidosaurs', 'birds', 'amphibia', 'mammals'),
             'bony_fish'='bony_fish', 'plants'='plants', 'lepidosaurs'='lepidosaurs',
             'birds'='birds', 'amphibia'='amphibia', 'mammals'='mammals')
for(i in 1:length(grps)) {
  # GET GROUP IDS
  grp_nm <- names(grps)[i]
  grp <- grps[[i]]
  cat('    Working on [', grp_nm, '] ....\n', sep="")
  pull <- epi[['txnmcgrp']] %in% grp
  txids <- epi[pull, 'txid']
  pull <- epi[['n']] == 1 & pull
  spp <- epi[pull, 'txid']
  output_file <- file.path(output_dir, paste0(grp_nm, ".RData"))
  
  # GET LIVING FOSSILS
  cat("    Finding [", nlfs, "] top most living fossil clades ....",
      sep="")
  epi <- epi[order(epi[['pepi']], decreasing=FALSE), ]
  lf_txids <- epi[['txid']][epi[['txid']] %in% txids][1:nlfs]
  lf_txids <- lf_txids[!is.na(lf_txids)]
  if(length(lf_txids) < 10) {
    cat("    \nToo few living fossils for this group!\n")
    next
  }
  cat("Done.\n")
  
  # SEARCH IUCN FOR LIVING FOSSIL NARRATIVES
  lf_dscrptn <- list()
  cat('    Searching IUCN for living fossil descriptions ....')
  for(txid in lf_txids) {
    nms <- getKidNms(txid)
    nms <- nms[!nms %in% ignr]
    res <- list()
    for(nm in nms) {
      nrrtv <- getIUCNNrrtv(nm, token)
      if(class(nrrtv) == "list" && length(nrrtv[['result']]) > 0 &&
         !is.null(nrrtv[['result']][[1]][['habitat']])) {
        nrrtv <- nrrtv[['result']][[1]][['habitat']]
        res[[nm]] <- gsub("<.*?>", "", nrrtv)  # remove html tags
      }
    }
    if(length(res) > 0) {
      lf_dscrptn[[txid]] <- res
    }
  }
  cat("Done, found data for [", length(lf_dscrptn), "].\n", sep="")
  
  # SEARCH IUCN FOR NULL HABITAT AND ECOLOGY DESCRIPTIONS
  cat('    Searching IUCN for null descriptions (iterating) ....\n')
  null_dscrptn <- list()
  ntrys <- 0
  cat('    ')
  for(itrtn in 1:nitrtns) {
    iPrnt(itrtn, nitrtns)
    null_dscrptn[[itrtn]] <- list()
    tmp_spp <- spp
    cc <- 0
    while(cc < length(lf_dscrptn)) {
      sp <- sample(tmp_spp, size=1)
      nm <- ndobj[[sp]][['nm']][['scientific name']]
      nrrtv <- getIUCNNrrtv(nm, token)
      cate <- getIUCNCat(nm, token)
      sccss <- FALSE
      if(class(cate) == "list" && length(cate[['result']]) > 0) {
        cate <- cate[['result']][[1]][['category']]
        sccss <- cate != "DD"
      }
      if(sccss && class(nrrtv) == "list" && length(nrrtv[['result']]) > 0 &&
         !is.null(nrrtv[['result']][[1]][['habitat']])) {
        null_dscrptn[[itrtn]][[nm]] <- nrrtv[['result']][[1]][['habitat']]
        cc <- cc + 1
        tmp_spp <- tmp_spp[tmp_spp != sp]
        ntrys <- 0
      } else {
        ntrys <- ntrys + 1
      }
      if(ntrys > 100) {
        cat("    ----- can't find description data ----")
        break
      }
    }
  }
  cat("       Done.\n")
  
  # OUTPUT
  cat('    Saving ....')
  save(lf_dscrptn, null_dscrptn, file=output_file)
  cat('Done.\n')
}

# END
cat(paste0('\nStage `permutation download` finished at [', Sys.time(), ']\n'))