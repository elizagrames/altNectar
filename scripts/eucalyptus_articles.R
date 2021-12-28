# Functions --------------------------------------------------------------------

clean_html <- function(html, basedir=NULL){
  
  if(!is.null(basedir)){keyname <- gsub(basedir, "", html)}else{keyname <- html}
  x <- paste(readLines(html), collapse="; ")
  
  x <- strsplit(x, "p-0 container-xl")[[1]][2]
  x <- strsplit(x, "Monitor Newspapers.com about ")[[1]][1]
  
  y <- strsplit(x, "py-3 search-record border-bottom  no-gutters row")[[1]]
  y <- strsplit(y, "h5 mb-1\"><b>")
  y <- y[-1]
  
  tmp.dat <- array(dim=c(length(y), 5))
  
  for(i in 1:length(y)){
    tmp <- y[[i]][2]
    source <- strsplit(tmp, "</b>")[[1]][1]
    pages <- strsplit(strsplit(tmp, "text-muted text-uppercase text-small mb-1\">")[[1]][2], "</p>")[[1]][1]
    location <- strsplit(strsplit(tmp, "location2 text-muted\"></span>")[[1]][2], "</p>")[[1]][1]
    date <- strsplit(strsplit(tmp, "<span class=\"news-date icon-news icon-cal text-muted mr-1\"></span><span>")[[1]][2], "</span")[[1]][1]
    
    tmp.dat[i,] <- cbind(source, pages, location, date, keyname)
  }
  
  tmp.dat <- data.frame(apply(tmp.dat, 2, trimws))
  colnames(tmp.dat) <- c("source", "page", "location", "date", "keywords")

  return(tmp.dat)
}

# Note: only works for format exported from clean_html
split_date <- function(dat){
  
  dat$year <- unlist(lapply(dat$date, function(x){
    as.numeric(tail(strsplit(x, ", ")[[1]], 1))
  }))
  
  dat$day <- unlist(lapply(dat$date, function(x){
    as.numeric(strsplit(strsplit(x, ", ")[[1]][2], " ")[[1]][2])
  }))
  
  dat$month <- unlist(lapply(dat$date, function(x){
    strsplit(strsplit(x, ", ")[[1]][2], " ")[[1]][1]
  }))
  
  dat$weekday <- unlist(lapply(dat$date, function(x){
    strsplit(x, ", ")[[1]][1]
  }))
  
  return(dat)
  
}

# Read in articles -------------------------------------------------------------

file.list <- list.files("./data/eucalyptus-articles", full.names = T)

html.read <- lapply(file.list, 
                 clean_html, basedir="/home/elizagrames/eucalyptus-articles/")

articles.read <- do.call(rbind, html.read)

articles <- articles.read[!duplicated(articles.read),]

# Clean article dates and locations --------------------------------------------

articles <- split_date(articles)

articles$location <- trimws(gsub(", California", "", articles$location))
county.ref <- read.csv("./data/ca-city-reference.csv")
articles$county <- county.ref$County[match(articles$location, county.ref$City)]

county.newspapers <- read.csv("./data/newspaper-records.csv")
county.newspapers$County <- factor(append(county.newspapers$County, 
                                          county.ref$County))[1:nrow(county.newspapers)]

# Set up placeholders ----------------------------------------------------------

# Need to merge counties by name because not all counties are present in data

ref.articles <- dplyr::left_join(data.frame(County=levels(county.newspapers$County)), 
                                 county.newspapers, "County")
rownames(ref.articles) <- ref.articles[,1]
ref.articles <- ref.articles[,-1] 
ref.articles <- ref.articles[,ncol(ref.articles):1] # reverse order
colnames(ref.articles) <- 1860:2021

articles$county <- factor(append(articles$county, 
                                 county.ref$County))[1:nrow(articles)]

placeholder <- array(dim=c(nlevels(articles$county), length(1860:2021)))
rownames(placeholder) <- levels(articles$county)
colnames(placeholder) <- 1860:2021

# Create dataset ---------------------------------------------------------------

data.mat <- table(articles$county, articles$year)
years <- 1860:2021

for(i in 1:length(years)){
  if(years[i] %in% colnames(data.mat)){
    placeholder[,i] <- data.mat[,colnames(data.mat)==years[i]]
  }
}

placeholder[is.na(placeholder)] <- 0

relative.counts <- as.matrix(placeholder)/as.matrix(ref.articles)*1000


# Write outputs ----------------------------------------------------------------

write.csv(relative.counts, file="./output/relative_counts_eucalyptus.csv")
write.csv(placeholder, file="./output/absolute_counts_eucalyptus.csv")


