# FOLDERS
cache_dir <- "caches"
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}
cache_dir <- file.path("caches", "iucn")
if(!file.exists(cache_dir)) {
  dir.create(cache_dir)
}
nrrtv_dir <- file.path("caches", "iucn", "narratives")
if(!file.exists(nrrtv_dir)) {
  dir.create(nrrtv_dir)
}
hbbts_dir <- file.path("caches", "iucn", "habitats")
if(!file.exists(hbbts_dir)) {
  dir.create(hbbts_dir)
}
cntrs_dir <- file.path("caches", "iucn", "countries")
if(!file.exists(cntrs_dir)) {
  dir.create(cntrs_dir)
}
cat_dir <- file.path("caches", "iucn", "category")
if(!file.exists(cat_dir)) {
  dir.create(cat_dir)
}
rng_dir <- file.path("caches", "iucn", "range")
if(!file.exists(rng_dir)) {
  dir.create(rng_dir)
}

# FUNCTIONS
getToken <- function() {
  if(file.exists("iucn_token.R")) {
    source("iucn_token.R")
  } else {
    msg <- "No token found!
Apply for one here: http://apiv3.iucnredlist.org/api/v3/token
Save the token in an R script called `iucn_token.R` in the working dir:
    `token <- [USER TOKEN]`\n"
    stop(msg)
  }
  token
}

cleanNm <- function(nm) {
  # Return name that is html safe and API safe
  nm <- sub("\\/.*", "", nm)
  nm <- gsub("[0-9]", "", nm)
  nm <- sub("sp\\.", "", nm)
  nm <- paste0(toupper(substring(nm, 1,1)),
               tolower(substring(nm, 2)), collapse="")
  nm
}

getIUCNCat <- function(nm, token) {
  # Get category data for species from IUCN API
  # first make sure nm is html safe
  nm <- cleanNm(nm)
  # second check if not already downloaded
  fl <- file.path(cat_dir, paste0(gsub(" ", "_", nm), '.RData'))
  if(file.exists(fl)) {
    load(fl)
  } else {
    url <- paste0("http://apiv3.iucnredlist.org/api/v3/species/", nm,"?token=", token)
    res <- .safeFromJSON(url)
    save(res, file=fl)
  }
  res
}

cateAsNum <- function(cate) {
  # Convert IUCN redlist category to number
  if(cate == 'LC') {
    return(0)
  }
  if(cate == 'NT') {
    return(1)
  }
  if(cate == 'VU') {
    return(2)
  }
  if(cate == 'EN') {
    return(3)
  }
  if(cate == 'CR') {
    return(4)
  }
  if(cate == 'EW') {
    return(5)
  }
  if(cate == 'EX') {
    return(6)
  }
  NA
}

.safeFromJSON <- function (url, max.trys=12, power=2) {
  # Safe wrapper for fromJSON
  trys <- 0
  waittime <- 2
  while (trys < max.trys) {
    json.obj <- try (RJSONIO::fromJSON(url), silent = TRUE)
    if (class (json.obj) == 'try-error') {
      cat ('---- Connection failed: trying again in [', waittime,
           's]----\n', sep='')
      trys <- trys + 1
      Sys.sleep (waittime)
      waittime <- waittime*power
    } else {
      return (json.obj)
    }
  }
  stop ("Failed to connect, server may be down.")
}

getIUCNNrrtv <- function(nm, token) {
  # Get narrative data for species from IUCN API
  # first make sure nm is html safe
  nm <- cleanNm(nm)
  # second check if not already downloaded
  fl <- file.path(nrrtv_dir, paste0(gsub(" ", "_", nm), '.RData'))
  if(file.exists(fl)) {
    load(fl)
  } else {
    url <- paste0("http://apiv3.iucnredlist.org/api/v3/species/narrative/", nm,"?token=", token)
    res <- .safeFromJSON(url)
    save(res, file=fl)
  }
  res
}

getIUCNHbbts <- function(nm, token) {
  # Get habitat data for species from IUCN API
  # first make sure nm is html safe
  nm <- cleanNm(nm)
  # second check if not already downloaded
  fl <- file.path(hbbts_dir, paste0(gsub(" ", "_", nm), '.RData'))
  if(file.exists(fl)) {
    load(fl)
  } else {
    url <- paste0("http://apiv3.iucnredlist.org/api/v3/habitats/species/name/",
                  nm,"?token=", token)
    res <- .safeFromJSON(url)
    save(res, file=fl)
  }
  res
}

getIUCNCntrs <- function(nm, token) {
  # Get country data for species from IUCN API
  # first make sure nm is html safe
  nm <- cleanNm(nm)
  # second check if not already downloaded
  fl <- file.path(cntrs_dir, paste0(gsub(" ", "_", nm), '.RData'))
  if(file.exists(fl)) {
    load(fl)
  } else {
    url <- paste0("http://apiv3.iucnredlist.org/api/v3/species/countries/name/",
                  nm,"?token=", token)
    res <- .safeFromJSON(url)
    save(res, file=fl)
  }
  res
}

cleanWrds <- function(txt) {
  # Take text and turn into list of unique words
  wrds <- strsplit(txt, " ")[[1]]
  wrds <- tolower(wrds)
  wrds <- tm::removePunctuation(wrds)
  wrds <- tm::removeNumbers(wrds)
  wrds <- gsub("[[:space:]]", "", wrds)
  wrds <- unique(wrds)
  wrds <- wrds[sapply(wrds, nchar) > min_nchar]
  wrds
}

getID <- function(nm, token) {
  # Get ID from sp name
  res <- getIUCNCat(nm, token)
  if('result' %in% names(res) && length(res[['result']]) > 0 &&
     'taxonid' %in% names(res[['result']][[1]])) {
    return(res[['result']][[1]][['taxonid']])
  }
  NULL
}

getRange <- function(nm, token) {
  # Get range from IUCN website
  nm <- cleanNm(nm)
  # second check if not already downloaded
  fl <- file.path(rng_dir, paste0(gsub(" ", "_", nm), '.RData'))
  if(file.exists(fl)) {
    load(fl)
  } else {
    val <- NA
    id <- getID(nm, token)
    if(!is.null(id)) {
      url <- paste0("http://www.iucnredlist.org/details/", id)
      res <- suppressWarnings(try(expr=readLines(url),
                                  silent=TRUE))
      unlink(url)
      if(class(res) != 'try-error') {
        bool <- grepl("Estimated extent of occurrence", res)
        if(any(bool)) {
          res <- res[bool]
          res <- strsplit(res, '<\\/td>')
          i <- which(grepl("Estimated extent of occurrence", res[[1]])) + 1
          val <- as.numeric(sub(".*>", "", res[[1]][i]))
        }
      }
    }
    save(val, file=fl)
  }
  val
}