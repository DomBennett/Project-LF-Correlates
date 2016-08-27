# MODEL HABITTATS AND EPI

# START
cat(paste0('\nStage `model habittats` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'hbbt_tools.R'))

# PARAMETERS
source('parameters.R')

# DIRS
input_file <- file.path("1_download", "hbbts.RData")

# INPUT
load(input_file)
epi <- epi[!duplicated(epi$txid), ]
hbbts <- hbbts[whbbts]
cds <- cds[whbbts]
epi <- epi[whbbts, ]

# IUCN HABITAT TYPES
# http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3
htypes_of_interest <- c("forest", "subterranean",
                        "wetlands", "tundra",
                        "boreal", "temperate",
                        "rocky", "tropical")
ht_tests <- vector("list", length=length(htypes_of_interest))
names(ht_tests) <- htypes_of_interest
ht_pull <- raw_scrs <- bin_scrs <- NULL
for(ht in htypes_of_interest) {
  vals <- calcProp(ht)
  test <- wilcox.test(epi$pepi[vals >= .5],
                      epi$pepi[vals < .5])
  means <- tapply(epi$pepi, factor(vals), mean)
  ht_tests[[ht]] <- list(test, means)
  if(test$p.value < 0.05) {
    raw_scrs <- c(raw_scrs, vals)
    bin_scrs <- c(bin_scrs, as.numeric(vals >= .5))
    ht_pull <- c(ht_pull, TRUE)
  } else {
    ht_pull <- c(ht_pull, FALSE)
  }
}
hts <- factor(rep(htypes_of_interest[ht_pull], each=nrow(epi)))
hbbts_scrs <- data.frame(scr=bin_scrs, type=hts, pepi=epi$pepi)
p <- ggBinomial(hbbts_scrs)
p

# IUCN HABITAT TYPES BY CODE
cds_of_interest <- c("7.1", "7.2")  # subterranean
cds_of_interest <- as.character(seq(1.1, 1.9, .1))  # forests
cd_tests <- vector("list", length=length(cds_of_interest))
names(cd_tests) <- cds_of_interest
cd_pull <- scrs <- NULL
for(cd in cds_of_interest) {
  vals <- as.numeric(sapply(cds, function(x) cd %in% x))
  test <- wilcox.test(epi$pepi[vals == 1],
                      epi$pepi[vals == 0])
  means <- tapply(epi$pepi, factor(vals), mean)
  cd_tests[[cd]] <- list(test, means)
  if(test$p.value < 0.05) {
    scrs <- c(scrs, vals)
    cd_pull <- c(cd_pull, TRUE)
  } else {
    cd_pull <- c(cd_pull, FALSE)
  }
}
cds_scrs <- data.frame(scr=scrs, type=
                         factor(rep(cds_of_interest[cd_pull],
                                    each=length(whbbts))),
                       pepi=epi$pepi)
p <- ggBinomial(cds_scrs)
p