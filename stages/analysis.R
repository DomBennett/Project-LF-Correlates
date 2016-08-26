
# Non-normal data, use rank correlation
hist(epi$nhbbts)
pull <- !is.na(epi$nhbbts) &
  !is.na(epi$pepi)
cor.test(epi$pepi[pull], epi$nhbbts[pull],
    method="spearman")

hist(epi$ncntrs)
pull <- !is.na(epi$ncntrs) &
  !is.na(epi$pepi)
cor.test(epi$pepi[pull], epi$ncntrs[pull],
    method="spearman")

hist(epi$cate)
pull <- !is.na(epi$cate) &
  !is.na(epi$pepi)
cor.test(epi$pepi[pull], epi$cate[pull],
    method="spearman")

# All signficiant, but very weak