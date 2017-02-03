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


# TEST BY HABITAT SETS AND INDEX
# by type
cat('Testing type ~ pepi....\n')
cds_of_interest <- sapply(unique(hbbt_cds[['type_code']]),
                          function(x) hbbt_cds[['subtype_code']][hbbt_cds[['type_code']] == x])
names(cds_of_interest) <- unique(hbbt_cds[['type_code']])
type_pepi <- testing(cds_of_interest, indx='pepi')
# add new type column to data
type_pepi[['test_res']][['type']] <-
  hbbt_cds[['type']][match(type_pepi[['test_res']][['cds']], hbbt_cds[['type_code']])]
type_pepi[['p_data']][['type']] <-
  hbbt_cds[['type']][match(type_pepi[['p_data']][['ht']], hbbt_cds[['type_code']])]
print(type_pepi[['test_res']][ ,c('type', 'diffs')])
cat('Done.\n')
cat('Testing type ~ epi....\n')
type_epi <- testing(cds_of_interest, indx='epi')
# add new type column to data
type_epi[['test_res']][['type']] <-
  hbbt_cds[['type']][match(type_epi[['test_res']][['cds']], hbbt_cds[['type_code']])]
type_epi[['p_data']][['type']] <-
  hbbt_cds[['type']][match(type_epi[['p_data']][['ht']], hbbt_cds[['type_code']])]
print(type_epi[['test_res']][ ,c('type', 'diffs')])
cat('Done.\n')
# by subtype
cat('Testing subtype ~ pepi....\n')
cds_of_interest <- hbbt_cds[['subtype_code']]
names(cds_of_interest) <- hbbt_cds[['subtype_code']]
subtype_pepi <- testing(cds_of_interest, indx='pepi')
# add new subtype column to data
subtype_pepi[['test_res']][['type']] <-
  hbbt_cds[['subtype']][match(subtype_pepi[['test_res']][['cds']], hbbt_cds[['subtype_code']])]
subtype_pepi[['p_data']][['type']] <-
  hbbt_cds[['subtype']][match(subtype_pepi[['p_data']][['ht']], hbbt_cds[['subtype_code']])]
print(subtype_pepi[['test_res']][ ,c('type', 'diffs')])
cat('Done.\n')
cat('Testing subtype ~ epi....\n')
subtype_epi <- testing(cds_of_interest, indx='epi')
# add new type column to data
subtype_epi[['test_res']][['type']] <-
  hbbt_cds[['type']][match(subtype_epi[['test_res']][['cds']], hbbt_cds[['type_code']])]
subtype_epi[['p_data']][['type']] <-
  hbbt_cds[['type']][match(subtype_epi[['p_data']][['ht']], hbbt_cds[['type_code']])]
print(subtype_epi[['test_res']][ ,c('type', 'diffs')])
cat('Done.\n')

# EXTRACT NEW SPECIAL META-GROUPS
# test if there are any other shared habitat features
# by testing whether shared words among the habitats also produce
# sign. results
res <- subtype_pepi[['test_res']]
special_types <- getShrdWrds(res[['type']][res[['diffs']] > 0])
special_types <- c(special_types, getShrdWrds(res[['type']][res[['diffs']] < 0]))
special_types <- special_types[!special_types %in% c('under', 'includes', 'rocky', 'creeks')]
special_codes <- sapply(special_types, function(x) hbbt_cds[['subtype_code']][grepl(x, hbbt_cds[['subtype']], ignore.case=TRUE)])
# test
spcl_pepi <- testing(special_codes, indx='pepi')
print(spcl_pepi[['test_res']])

# OUTPUT
# pack
hbbts_res <- list('type_pepi'=type_pepi,
                  'type_pepi'=type_epi,
                  'type_pepi'=subtype_pepi,
                  'type_epi'=subtype_epi,
                  'spcl_pepi'=spcl_pepi)
save(hbbts_res, file=file.path(output_dir, 'res.RData'))

# END
cat(paste0('\nStage `habitats` finished at [', Sys.time(), ']\n'))