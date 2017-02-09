# TAKE OBS FROM PANTHERIA AND ADD TO EPI

# START
cat(paste0('\nStage `pack` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'ndobj_tools.R'))
source(file.path('tools', 'pack_tools.R'))

# DIRS
output_dir <- "7_pack"
if(!file.exists(output_dir)) {
  dir.create(output_dir)
}

# DATA FILES
pan_file <- file.path('0_data', 'panTHERIA.txt')
avian_file <- file.path('0_data', 'avian_ssd_jan07.txt')
bmr_file <- file.path('0_data', 'bmr_data.csv')
epi_file <- file.path('4_range', 'res.RData')
ndobj_file <- file.path("0_data", "ndobj.RData")
orders_file <- file.path('0_data', 'orders.RData')
families_file <- file.path('0_data', 'families.RData')

# VARIABLES OF INTEREST
all_vrbls <- c("cate", "nhbbts", "ncntrs", "iucn_range")

# INPUT
cat('Reading data....\n')
load(ndobj_file)
load(epi_file)
load(orders_file)
load(families_file)
cat('Done.\n')

# AVIAN DATA
cat('Adding avian data ....\n')
avian <- read.delim(file=avian_file, na.strings=-999,
                    stringsAsFactors=FALSE)
avian[avian == -999] <- NA
# drop data with too few samples
avian[['unsexed_mass']][avian[['unsexed_mass_N']] < 3] <- NA
avian[['Unsexed_tarsus']][avian[['Unsexed_tarsus_N']] < 3] <- NA
avian[['Unsexed_bill']][avian[['Unsexed_bill_N']] < 3] <- NA
avian[['Unsexed_tail']][avian[['Unsexed_tail_N']] < 3] <- NA
# variables of interest
vrbls <- c('unsexed_mass', 'Unsexed_tarsus', 'Unsexed_bill',
           'Unsexed_tail', 'Clutch_size', 'Egg_mass', 'Mating_System', 'Display')
for(vrbl in vrbls) {
  epi[[vrbl]] <- NA
}
species_names <- avian[['Species_name']]
row_i <- which(epi[['txnmcgrp']] == 'birds')
epi <- addToEPI(epi, vrbls, avian, species_names, row_i)
all_vrbls <- c(all_vrbls, vrbls)
cat('Done.\n')

# MAMMAL DATA
cat('Adding mammal data.....\n')
pan <- read.delim (file=pan_file, na.strings=-999,
                   stringsAsFactors=FALSE)
# add new columns to epi
vrbls <- c('X5.1_AdultBodyMass_g', "X1.1_ActivityCycle" , "X22.1_HomeRange_km2",
           "X12.1_HabitatBreadth", "X10.1_PopulationGrpSize", "X10.2_SocialGrpSize",
           "X6.2_TrophicLevel", "X6.1_DietBreadth", "X9.1_GestationLen_d",
           "X15.1_LitterSize", "X23.1_SexualMaturityAge_d", "X5.2_BasalMetRateMass_g",
           "X17.1_MaxLongevity_m", "X14.1_InterBirthInterval_d", "X28.2_Temp_Mean_01degC",
           "X28.1_Precip_Mean_mm")
for(vrbl in vrbls) {
  epi[[vrbl]] <- NA
}
species_names <- pan[['MSW93_Binomial']]
row_i <- which(epi[['txnmcgrp']] == 'mammals')
epi <- addToEPI(epi, vrbls, pan, species_names, row_i)
all_vrbls <- c(all_vrbls, vrbls)
cat('Done.\n')

# BMR DATA
cat('Adding bmr data.....\n')
bmr <- read.csv(file=bmr_file, stringsAsFactors=FALSE)
bmr$species <- gsub('_', ' ', bmr$species)
# add new columns to epi
vrbls <- c('maximum_lifespan_yr', 'mass_g', 'BMR')
for(vrbl in vrbls) {
  epi[[vrbl]] <- NA
}
species_names <- bmr[['species']]
row_i <- which(epi[['txnmcgrp']] %in% c('mammals', 'birds'))
epi <- addToEPI(epi, vrbls, bmr, species_names, row_i)
# categorical
vrbls <- c('volancy', 'fossoriallity', 'foraging_environment',
           'daily_activity')
for(vrbl in vrbls) {
  epi[[vrbl]] <- NA
}
epi <- addToEPI(epi, vrbls, bmr, species_names, row_i, categorical=TRUE)
all_vrbls <- c(all_vrbls, vrbls)
cat('Done.\n')

# LOG
cat('Log all variables that are not normal....\n')
for(i in 1:length(all_vrbls)) {
  vrbl <- all_vrbls[i]
  obs <- epi[[vrbl]]
  obs <- obs[!is.na(obs)]
  bool <- sapply(obs, is.numeric)
  if(!all(bool)) {
    next
  }
  min_z <- abs((min(obs) - mean(obs))/sd(obs))
  max_z <- (max(obs) - mean(obs))/sd(obs)
  # if either the min or max z is greater than 4.5, log
  if(min_z > 4.5 | max_z > 4.5) {
    vrbl_log <- paste0(vrbl, '_log')
    epi[[vrbl_log]] <- log(epi[[vrbl]])
    epi[[vrbl_log]][epi[[vrbl_log]] == Inf] <- NA
    epi[[vrbl_log]][epi[[vrbl_log]] == -Inf] <- NA
    all_vrbls[i] <- vrbl_log
  }
}
cat('Done.\n')

# ADD ORDERS
cat('Add taxonomic info....\n')
epi[['family']] <- epi[['order']] <- NA
for(i in 1:nrow(epi)) {
  txid <- as.character(epi[i, 'txid'])
  lng <- getLng(txid)
  pssbls <- lng[lng %in% orders]
  if(length(pssbls) > 0) {
    epi[i, 'order'] <- pssbls[length(pssbls)]
  } else {
    epi[i, 'order'] <- srchEntrez(txid, rank='order')
  }
  pssbls <- lng[lng %in% families]
  if(length(pssbls) > 0) {
    epi[i, 'family'] <- pssbls[length(pssbls)]
  } else {
    epi[i, 'family'] <- srchEntrez(txid, rank='family')
  }
}
spp <- epi[['n']] == 1
epi[['genus']] <- NA
epi[['genus']][spp] <- sub('\\s.*$', '', epi[['scinm']][spp])
# repeat higher level taxa for lower ranks
pull <- !is.na(epi[['txnmcgrp']]) & is.na(epi[['order']])
epi[pull, 'order'] <- as.character(epi[pull, 'txnmcgrp'])
pull <- !is.na(epi[['order']]) & is.na(epi[['family']])
epi[pull, 'family'] <- as.character(epi[pull, 'order'])
pull <- !is.na(epi[['family']]) & is.na(epi[['genus']])
epi[pull, 'genus'] <- as.character(epi[pull, 'family'])
cat('Done.\n')

# OUTPUT
save(epi, all_vrbls, file=file.path(output_dir, "res.RData"))

# END
cat(paste0('\nStage `pack` finished at [', Sys.time(), ']\n'))