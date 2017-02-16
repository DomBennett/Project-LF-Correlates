# HABITTATS

# START
cat(paste0('\nStage `habitats` started at [', Sys.time(), ']\n'))

# FUNCTIONS
source(file.path('tools', 'hbbt_tools.R'))

# PARAMETERS
source('parameters.R')

# DIRS
input_file <- file.path("1_download", "hbbts.RData")
output_dir <- "3_habitats"
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}

# INPUT
load(file.path('0_data', 'habitat_codes.RData'))
rownames(hbbt_cds) <- hbbt_cds[['subtype_code']]
load(input_file)
epi <- epi[!duplicated(epi$txid), ]
hbbts <- hbbts[whbbts]
cds <- cds[whbbts]
epi <- epi[whbbts, ]


# HABITAT SETS AND INDEX
# by type
cat('By type....\n')
cds_of_interest <- sapply(unique(hbbt_cds[['type_code']]),
                          function(x) hbbt_cds[['subtype_code']][hbbt_cds[['type_code']] == x])
names(cds_of_interest) <- unique(hbbt_cds[['type_code']])
types <- hbbtPA(cds_of_interest)
# replace 1 and 0s with habitat type names
colnames(types) <-
  hbbt_cds[['type']][match(colnames(types), hbbt_cds[['type_code']])]
epi <- cbind(epi, types)
cat('Done.\n')
# by subtype
cat('By subtype....\n')
cds_of_interest <- hbbt_cds[['subtype_code']]
names(cds_of_interest) <- hbbt_cds[['subtype_code']]
subtypes <- hbbtPA(cds_of_interest)
# add new subtype column to data
colnames(subtypes) <-
  hbbt_cds[['subtype']][match(colnames(subtypes), hbbt_cds[['subtype_code']])]
epi <- cbind(epi, subtypes)
cat('Done.\n')

# OUTPUT
hbbts_epi <- epi
save(hbbts_epi, file=file.path(output_dir, 'res.RData'))

# END
cat(paste0('\nStage `habitats` finished at [', Sys.time(), ']\n'))