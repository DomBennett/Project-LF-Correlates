# Convert web text (http://www.iucnredlist.org/technical-documents/classification-schemes/habitats-classification-scheme-ver3)
# into csv

lines <- readLines(file.path('0_data', 'habitat_codes.txt'))

# type code, type, subtype code, subtype
res <- data.frame(type=NA, type_code=NA, subtype=NA, subtype_code=NA)

for(l in lines) {
  l <- sub('^\\s+', '', l)
  # type
  if(grepl('^[0-9]+\\s', l)) {
    type <- sub('[0-9]+\\s+', '', l)
    type_code <- gsub('\\s+.+', '', l)
  }
  # subtype
  if(grepl('^[0-9]+\\.', l)) {
    subtype <- sub('[0-9\\.]+\\s+', '', l)
    subtype_code <- gsub('\\s+.+', '', l)
    res <- rbind(res, c(type, type_code, subtype, subtype_code))
  }
}

hbbt_cds <- res[-1,]

save(hbbt_cds, file=file.path('0_data', 'habitat_codes.RData'))
