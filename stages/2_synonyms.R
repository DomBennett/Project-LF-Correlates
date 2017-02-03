# SYNONYM/ANTONYM MATCHING
# look up synonyms and antonyms in IUCN texts
# test and plot

# START
cat(paste0('\nStage `synonym` started at [', Sys.time(), ']\n'))

# FUNCTIONS
library(gridExtra)
source(file.path('tools', 'i_tools.R'))
source(file.path('tools', 'synonym_tools.R'))
source(file.path('tools', 'ndobj_tools.R'))
source(file.path('tools', 'iucn_dwnld_tools.R'))

# DIRS
output_dir <- '2_synonyms'
if (!file.exists(output_dir)) {
  dir.create(output_dir)
}
input_file <- file.path("1_download", "wrds.RData")
ndobj_file <- file.path("0_data", "ndobj.RData")

# INPUT
load(input_file)
load(ndobj_file)

# Notes
# loop up example texts
# getExampleTextFrmWrd(epi, pttrn="^cave$")
# getExampleTextFrmI(epi, sample(which(epi$refuge_scr > 0), 1))
# getExampleTextFrmI(epi, sample(which(epi$refuge_scr < 0), 1))

# TEST HERMIT
cat('Testing `hermit`....\n')
pstvs <- c('^sole', '^alone$', '^lone', '^single$', '^solitary$',
           '^unsociable', '^hermit', '^individual$')
ngtvs <- c('^group', '^pack', '^altruistic', '^convivial', '^gather',
           '^herd', '^flock', '^aggregate', '^swarm', '^horde', '^sociable')
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
hermit_counts <- colSums(mtch_mtrx)
hermit_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
hermit_scores$pepi <- epi[rownames(hermit_scores), 'pepi']
hermit_scores$epi <- epi[rownames(hermit_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=hermit_scores,
                     fname=file.path(output_dir, 'hermit.txt'))
loopTests(hermit_scores)
cat('Done.\n')

# TEST LETHARGY
cat('Testing `lethargy`....\n')
pstvs <- c('^lethargic$', '^inactive$', '^torpid$', '^inactivity$', '^passive$',
           '^languid$', '$unresponsive$', '^sedate$', '^slow-moving$', '^slow-going$')
ngtvs <- c('^energetic', '^dynamic', '^vigou?r', '^aggressive$', '^nimble$', '^lively$',
           '^swift', '^fast-moving$')
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
lethargy_counts <- colSums(mtch_mtrx)
lethargy_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
lethargy_scores$pepi <- epi[rownames(lethargy_scores), 'pepi']
lethargy_scores$epi <- epi[rownames(lethargy_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=lethargy_scores,
                     fname=file.path(output_dir, 'lethargy.txt'))
loopTests(lethargy_scores)
cat('Done.\n')

# TEST REFUGIUM
cat('Testing `refugium`....\n')
pstvs <- c("^refug", "^stable$", "^fossorial",
           "^underground$", "^static", "^dig")
ngtvs <- c("^dynamic", "^changing$", "^unstable$")
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
refugium_counts <- colSums(mtch_mtrx)
refugium_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
refugium_scores$pepi <- epi[rownames(refugium_scores), 'pepi']
refugium_scores$epi <- epi[rownames(refugium_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=refugium_scores,
                     fname=file.path(output_dir, 'refugium.txt'))
loopTests(refugium_scores)
cat('Done.\n')

# TEST SPECIALIST
cat('Testing `specalist`....\n')
pstvs <- c('^specialist$', '^unique$', '^specialised$', '^specialized$',
           '^idiosyncratic', '^adapted$', '^distinctive$', '^distinct',
           '^uncommon')
ngtvs <- c('^generalist', '^generic', '^unspecialised',
           '^unspecialized', '^opportunist$', '^opportunistic',
           '^omnivore$')
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
specialist_counts <- colSums(mtch_mtrx)
specialist_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
specialist_scores$pepi <- epi[rownames(specialist_scores), 'pepi']
specialist_scores$epi <- epi[rownames(specialist_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=specialist_scores,
                     fname=file.path(output_dir, 'specialist.txt'))
loopTests(specialist_scores)
cat('Done.\n')

# TEST PRIMITIVE
cat('Testing `primitive`....\n')
pstvs <- c('^primitive', '^undeveloped', '^basic', '^simple', '^rudimentary$')
ngtvs <- c('^developed$', '^specialised$', '^specialized$', '^adapted$',
           '^advanced$', '^evolved$')
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
primitive_counts <- colSums(mtch_mtrx)
primitive_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
primitive_scores$pepi <- epi[rownames(primitive_scores), 'pepi']
primitive_scores$epi <- epi[rownames(primitive_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=primitive_scores,
                     fname=file.path(output_dir, 'primitive.txt'))
loopTests(primitive_scores)
cat('Done.\n')

# TEST ODDNESS
cat('Testing `oddness`....\n')
pstvs <- c('^odd', '^unusual', '^bizarre', '^aberrant',
           '^unconventional', '^peculiar', '^strange',
           '^atypical', '^unexpected', '^untypical',
           '^anomalous', '^remarkable')
ngtvs <- c('^standard', '^conventional', '^typical$',
           '^unremarkable$')
mtch_mtrx <- matchMatrix(c(pstvs, ngtvs))
oddness_counts <- colSums(mtch_mtrx)
oddness_scores <- senseScores(mtch_mtrx, pstvs, ngtvs)
oddness_scores$pepi <- epi[rownames(oddness_scores), 'pepi']
oddness_scores$epi <- epi[rownames(oddness_scores), 'epi']
writeOutExampleTexts(epi, mtch_mtrx, scores=oddness_scores,
                     fname=file.path(output_dir, 'oddness.txt'))
loopTests(oddness_scores)
cat('Done\n')

# OUTPUT
# pack
synonyms_res <- list('hermit'=list('scores'=hermit_scores, 'wrd_counts'=hermit_counts),
                     'lethargy'=list('scores'=lethargy_scores, 'wrd_counts'=lethargy_counts),
                     'refugium'=list('scores'=refugium_scores, 'wrd_counts'=refugium_counts),
                     'specialist'=list('scores'=specialist_scores, 'wrd_counts'=specialist_counts),
                     'primitive'=list('scores'=primitive_scores, 'wrd_counts'=primitive_counts),
                     'oddness'=list('scores'=oddness_scores, 'wrd_counts'=oddness_counts))
save(synonyms_res, file=file.path(output_dir, 'res.RData'))

# END
cat(paste0('\nStage `synonyms` finished at [', Sys.time(), ']\n'))