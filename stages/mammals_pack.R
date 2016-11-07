# TAKE OBS FROM PANTHERIA AND ADD TO EPI

# FUNCTIONS
source(file.path('tools', 'ndobj_tools.R'))

# DIRS
pan_file <- file.path('0_data', 'panTHERIA.txt')
epi_file <- file.path('1_download', 'epi.RData')
ndobj_file <- file.path("0_data", "ndobj.RData")

# INPUT
pan <- read.delim (file=pan_file, na.strings=-999,
                   stringsAsFactors=FALSE)
load(ndobj_file)
load(epi_file)

# OBS OF INTEREST
ooi <- c('X5.1_AdultBodyMass_g', "X1.1_ActivityCycle" , "X22.1_HomeRange_km2",
         "X12.1_HabitatBreadth", "X10.1_PopulationGrpSize", "X10.2_SocialGrpSize",
         "X6.2_TrophicLevel", "X6.1_DietBreadth", "X9.1_GestationLen_d",
         "X15.1_LitterSize", "X23.1_SexualMaturityAge_d", "X5.2_BasalMetRateMass_g",
         "X17.1_MaxLongevity_m", "X14.1_InterBirthInterval_d", "X28.2_Temp_Mean_01degC",
         "X28.1_Precip_Mean_mm")
mean_nms <- paste0(sub("^[X0-9\\.]*_", "", ooi), "_mean")
med_nms <- paste0(sub("^[X0-9\\.]*_", "", ooi), "_med")
for(o in mean_nms) {
  epi[[o]] <- NA
}
for(o in med_nms) {
  epi[[o]] <- NA
}

# NAME MATCHING
for(i in 1:nrow(epi)) {
  nms <- getKidNms(epi[i,'txid'])
  res <- matrix(NA, ncol=length(ooi),
                nrow=length(nms))
  rownames(res) <- nms
  colnames(res) <- ooi
  for(nm in nms) {
    pull <- pan[['MSW93_Binomial']] == nm
    if(sum(pull) == 1) {
      for(o in ooi) {
        res[nm, o] <- pan[pull, o]
      }
    }
  }
  epi[i, med_nms] <- apply(res, 2, median, na.rm=TRUE)
  epi[i, mean_nms] <- apply(res, 2, mean, na.rm=TRUE)
}

epi <- epi[!is.na(epi[[mean_nms[1]]]),]
save(epi, file="epi_mammals.RData")
