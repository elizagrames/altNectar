par(pty="s", las=1)

counties <- rgdal::readOGR("./data/ca-counties/CA_Counties_TIGER2016.shp")

dat <- read.csv("./output/absolute_counts_hummingbirds.csv", row.names = 1)
all_years <- rowSums(dat, na.rm=T)

dat <- apply(dat, 2, as.numeric)
years <- 1860:2021

for(i in 1:nrow(dat)){
  if(i==1){
    plot(dat[i,] ~ years, type="l", ylim=c(0,100),
         axes=F, ylab="Article counts", xlab="Year")
    axis(1, at = seq(1860, 2021, 20)); axis(2)
  }else{
    lines(dat[i,] ~ years)
  }
}

counties$ad_counts <- log(all_years[match(counties$NAME, names(all_years))])+1
counties$col.regions <- colorRampPalette(viridis::inferno(256, begin = 1, end=0))(max(counties$ad_counts))[counties$ad_counts]

sp::plot(counties, col=counties$col.regions)
#raster::text(counties, counties$NAME, cex=0.7)

# Click somewhere sensible; will hard code later
legend(locator(1), legend=round(seq(1,max(all_years), length.out=7), 0), 
       fill=colorRampPalette(viridis::inferno(256, begin = 1, end=0))(max(all_years))[round(seq(1,max(all_years), length.out=7), 0)], 
       cex=1, bty="n")
legend("top", legend=c("All mentions 1860-2021"), bty="n", cex=1.5, text.font = 2)

plot(colSums(dat) ~ years, type="n", axes=F, ylab="Mentions of hummingbird feeder", xlab="Year",
     lwd=2, ylim=c(0,1500))
axis(1, seq(1860, 2021, 10)); axis(2)

for(i in 1:nrow(dat)){
  lines(cumsum(dat[i,]) ~ years, col="#00000080")
  text((max(which(dat[i,]>0))+1861), 
       max(cumsum(dat[i,])), names(all_years)[i], cex=0.6, adj=0) 
}

ncounties <- colSums(apply(dat, 2, function(x){x>0}))

plot(ncounties ~ years, type="l", 
     ylab="Number of counties with >1 mentions", xlab="Years", 
     lwd=3, axes=F, ylim=c(0,30))
axis(1, seq(1860, 2021, 20)); axis(2)


firstyear <- apply(dat, 1, function(x){(1860:2021)[min(which(x>0))]})

counties$firstyear <- firstyear[match(counties$NAME, names(all_years))]


col.regions2 <- viridis::viridis(120, begin = 0, end=1)[counties$firstyear-1860]

sp::plot(counties, col=col.regions2)

# Click somewhere sensible; will hard code later
legend(locator(1), legend=round(seq(1870,1983,length.out=7), 0),
       fill=viridis::magma(120, begin = 0, end=1)[round(seq(1870,1993,length.out=7)-1859, 0)],
       cex=1.5, bty="n")
legend("top", legend=c("Year of first mention"), bty="n", cex=1.5, text.font = 2)
