# Extract epi and ndobj from res.RData originating from Project-EPI

load('0_data/res.RData')
epi <- cld_data
write.csv(epi, file='0_data/epi_scores.csv', row.names=FALSE)
ndobj <- node_obj
save(ndobj, file="0_data/ndobj.RData")
