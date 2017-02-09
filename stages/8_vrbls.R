# RENAME VARIABLES
# make them easier to read

# START
cat(paste0('\nStage `vrbls` started at [', Sys.time(), ']\n'))

# DIRS
input_dir <- "7_pack"
output_dir <- '8_vrbls'
if(!file.exists(output_dir)) {
  dir.create(output_dir)
}
input_file <- file.path(input_dir, "res.RData")

# INPUT
load(input_file)

# all_vrbls
new_vrbls <- c("Category of extinction (log)",
               "No. habitats (log)",
               "No. countries (log)",
               "Avian range est. (log)",
               "Avian mass (log)",
               "Avian tarsus (log)",
               "Avian bill (log)",
               "Avian tail (log)",
               "Avian clutch size (log)",
               "Avian egg mass (log)",
               "Avian mathing system",
               "Avian display",
               "Mammalian body mass (log)",
               "Mammalian activity cycle (log)",
               "Mammalian range est. (log)",
               "Mammalian habitat breadth (log)",
               "Mammalian population group size (log)",
               "Mammalian social group size (log)",
               "Mammalian trophic level",
               "Mammalian diet breadth",
               "Mammalian gestation length (log)",
               "Mammalian litter size (log)",
               "Mammalian age of sexual maturity (log)",
               "Mammalian basal metabolic rate (log)",
               "Mammalian max. longevity (log)",
               "Mammalian inter-birth interval (log)",
               "Mammalian mean temp.",
               "Mammalian mean precip. (log)",
               "Volancy",
               "Fossoriality",
               "Foraging environment",
               "Daily activity")
for(i in 1:length(all_vrbls)) {
  j <- which(colnames(epi) == all_vrbls[i])
  colnames(epi)[j] <- new_vrbls[i]
}
all_vrbls <- new_vrbls

# OUTPUT
save(epi, all_vrbls, file=file.path(output_dir, "res.RData"))

# END
cat(paste0('\nStage `vrbls` finished at [', Sys.time(), ']\n'))
