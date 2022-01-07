par(pty="s", las=1)

counties <- rgdal::readOGR("./data/ca-counties/CA_Counties_TIGER2016.shp")
cali <- rgeos::gUnaryUnion(counties)

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

pal <- c("#ea698b", "#d55d92", "#973aa8", "#571089", '#47126b', "#320b32") # coral purple

counties$ad_counts <- log(all_years[match(counties$NAME, names(all_years))]+1)+1

counties$col.regions <- colorRampPalette(pal)(max(counties$ad_counts))[counties$ad_counts]

counties$col.regions[counties$ad_counts==1] <- NA

sp::plot(counties, col=counties$col.regions, lty=1, lwd=0.25, border="grey60")
sp::plot(counties, col=counties$col.regions, lty=1, lwd=0.01, add=T)
sp::plot(cali, add=T, lwd=1.5)



# Click somewhere sensible; will hard code later
legend(locator(1), legend=round(seq(1,max(all_years), length.out=7), 0), 
       fill=colorRampPalette(pal)(max(all_years))[round(seq(1,max(all_years), length.out=7), 0)], 
       cex=1.5, bty="n")
legend("topright", legend=c("Number of \narticles"), bty="n", cex=1.5, text.font = 2)

dat[is.na(dat)] <- 0
dat2 <- dat
for(i in 1:nrow(dat)){
  x <- dat[i,]
  if(any(x>0) & x[162]==0){
    x[(max(which(x>0))+1):length(x)] <- NA
  }else{
    x <- rep(NA, length(x))
  }
  x <- as.numeric(x)
  dat2[i,]  <- x
}




#savedat <- dat
#dat <- savedat
par(xpd=T, las=1)
plot(colSums(dat) ~ years, type="n", axes=F, 
     ylab="Number of articles", xlab="Year",
     lwd=2, ylim=c(0,1500), xlim=c(1860,2025))
axis(1, seq(1860, 2021, 10)); axis(2)


for(i in 1:nrow(dat)){
  lines(cumsum(dat[i,]) ~ years, col="#00000080")
  text(2021, 
       max(cumsum(dat[i,]), na.rm = T), names(all_years)[i], 
       cex=0.7, adj=0) 
}

ncounties <- colSums(apply(dat, 2, function(x){x>0}))

plot(ncounties ~ years, type="l", 
     ylab="Number of counties with >1 mentions", xlab="Years", 
     lwd=3, axes=F, ylim=c(0,30))
axis(1, seq(1860, 2021, 20)); axis(2)


firstyear <- apply(dat, 1, function(x){(1860:2021)[min(which(x>0))]})

counties$firstyear <- firstyear[match(counties$NAME, names(all_years))]


col.regions2 <- rev(colorRampPalette(pal)(123))[counties$firstyear-1860]

sp::plot(counties, col=col.regions2, lty=1, lwd=0.25, border="grey60")
sp::plot(counties, col=col.regions2, lty=1, lwd=0.01, add=T)
sp::plot(cali, add=T, lwd=1.5)





# Click somewhere sensible; will hard code later
legend(locator(1), legend=round(seq(1870,1983,length.out=7), 0),
       fill=rev(colorRampPalette(pal)(123))[round(seq(1870,1993,length.out=8)-1859, 0)],
       cex=1.5, bty="n")
legend("topright", legend=c("Year of \nfirst mention"), bty="n", cex=1.5, text.font = 2)
