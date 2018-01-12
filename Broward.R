#create maps showing the percentage of black and white residents in each census tract in Broward county

#use the development version of ggplot2, so it has the geom_sf() function
#devtools::install_github("tidyverse/ggplot2")

library(tidycensus)
library(tidyverse)
library(reshape2)
library(sf)
library(viridis)
library(ggthemes)
library(leaflet)
library(stringr)

setwd("~/R/Mapping Broward")
#dir.create("./plots")

StateFIPS <- 12
names(StateFIPS) <- "FL"

CountyFIPS <- 011
names(CountyFIPS) <- "Broward"

apikey <- "336e3fb621e924285143c2d9e1abec0f019046f8"
totalPop <- "B02001_001E"
whitePop <- "B02001_002E"
blackPop <- "B02001_003E"
nativePop <- "B02001_004E"
asianPop <- "B02001_005E"
nativeHaPIpop <- "B02001_006E"
otherPop <- "B02001_007E"
twoOrMorePop <- "B02001_008E"

popvars <- c(totalPop, whitePop, blackPop, nativePop, asianPop, nativeHaPIpop, otherPop, twoOrMorePop)
names(popvars) <- c("totalPop", "whitePop", "blackPop", "nativePop", "asianPop", "nativeHaPIpop", "otherPop", "twoOrMorePop")

BrowardPop <- get_acs(geography="tract", variables=popvars, endyear = 2015, output = "wide",
        state = StateFIPS, county = CountyFIPS, geometry = TRUE,
        summary_var = NULL, key = apikey, moe_level = 90)

#rename BrowardPop with human-readable names
popvarsME <- paste0(names(popvars), "ME")
names(BrowardPop) <- names(BrowardPop) <- c("GEOID", rbind(names(popvars), popvarsME), 
                                            "NAME", "geometry") 

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

#I can't do a the column transforms I want to do with the geometry variable. Not something I can avoid
#So, not to mess up the order of the columns referenced below, we can just set geometry to NULL.
#This has to do with sf, which I don't know.
st_geometry(BrowardPop) <- NULL

#We're going to create columns that show percentage of the population represented by each demographic
#Start by initializing several variables for loop
count <- 0
estimateCols <- 0
demog <- c()
newPercentVars <- c()

for (name in names(popvars)){
  count <- count + 1
  estimateCols <- 2*count
  colnum <-  count + 19
  #first create columns that reflect the percent total for each demographic
  BrowardPop[colnum] <- BrowardPop[estimateCols]/BrowardPop[2]
  
  #then create human-readable names for each column
  demog[count] <- gsub('Pop', '', name)
  newPercentVars[count] <- paste0("percent", .simpleCap(demog[count]))
  colnames(BrowardPop)[colnum] <- newPercentVars[count]
}

#Summary stats for all of Broward, we'll add these as median lines to the plots later
BrowardWideTotalPops <- apply(BrowardPop[, seq(2, 16, 2)], 2, sum)
BrowardWideMedianPercentages<- apply(BrowardPop[, c(19:26)], 2, median, na.rm=T)

BrowardWidePercentPops <- BrowardWideTotalPops/BrowardWideTotalPops[1]

#create and save pie chart for basic demographics
png(filename="./plots/BasicDemographicInformation.png")
pie(BrowardWideTotalPops[-1])
dev.off()


#bins for percentages
br <- seq(0,1,by=0.1)

#Residents? Does ACS capture non-citizens?

#Create two plots for Number of Census Tracts by Percentage White/Black Residents
png(filename="./plots/NumberofNeighborhoodsbyPercentageWhite.png")
hist(BrowardPop$percentWhite, br, plot = T, col="black", 
     main="Number of Census Tracts by \n Percentage White Residents",
     xlab="Percentage of White Residents", ylab="Number of Census Tracts", 
     border="white")
abline(v = BrowardWidePercentPops[2], col="red", lwd=4)
abline(v = BrowardWideMedianPercentages[2], col="blue", lwd=4)
legend("topleft", c("Median", "Mean"), col=c("blue", "red"), lwd=4)
dev.off()

png(filename="./plots/NumberofNeighborhoodsbyPercentageBlack.png")
hist(BrowardPop$percentBlack, br, plot = T, col="black", 
     main="Number of Census Tracts by \n Percentage Black Residents",
     xlab="Percentage of Black Residents", ylab="Number of Census Tracts",
     border="white")
abline(v = BrowardWidePercentPops[3], col="red", lwd=4)
abline(v = BrowardWideMedianPercentages[3], col="blue", lwd=4)
legend("topright", c("Median", "Mean"), col=c("blue", "red"), lwd=4)
dev.off()


BrowardPopGeom <- get_acs(geography="tract", variables=popvars, endyear = 2015, output = "wide",
                      state = StateFIPS, county = CountyFIPS, geometry = TRUE,
                      summary_var = NULL, key = apikey, moe_level = 90)

names(BrowardPopGeom) <- c("GEOID", rbind(names(popvars), popvarsME), 
                                            "NAME", "geometry") 

BrowardPopGeom$percentWhite <- BrowardPopGeom$whitePop/(BrowardPopGeom$totalPop+1)
  
#First let's map percentWhite
BrowardPopWhite <- subset(BrowardPopGeom, select=c('geometry', 'percentWhite'))

BrowardPopWhite %>% 
  ggplot(aes(fill = percentWhite, color = percentWhite)) + 
  geom_sf() + 
  coord_sf() + 
  scale_fill_viridis() + 
  scale_color_viridis() + 
  theme_pander()

pal <- colorQuantile(palette = "viridis", domain = BrowardPopWhite$percentWhite, n = 10)

PercentageWhiteMap <- BrowardPopGeom %>%
  st_transform(crs="+init=epsg:4269") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(percentWhite)) %>%
  addLegend("bottomleft",
            #pal is color palette
            pal = pal, 
            values = ~ percentWhite,
            title = "Percentage White Population",
            opacity = 1)

#saveWidget(PercentageWhiteMap, file="./plots/PercentWhiteMap.html")
#Save Widget doesn't work, so have to save on its own.  

#white to black dissimilarity index
BrowardTotalPop <- sum(BrowardPop$totalPop)
blackPropBroward <- sum(BrowardPop$blackPop)/ BrowardTotalPop
whitePropBroward <- sum(BrowardPop$whitePop)/ BrowardTotalPop
nonwhitePropBroward <- 1 - whitePropBroward
whitePopBroward <- sum(BrowardPop$whitePop)
nonwhitePopBroward <- BrowardTotalPop - whitePopBroward

BrowardPop$dissimilarityNumerator <- (BrowardPop$totalPop * abs(BrowardPop$percentBlack-blackPropBroward))
disSum <- sum(BrowardPop$dissimilarityNumerator, na.rm=T)

whiteToBlackdissimilarityIndexBroward <- disSum/(2 * BrowardTotalPop * blackPropBroward*(1-  blackPropBroward))

#white to non-white dissimilarity
#Another dissimilarity index (1/2) SUM (bi /B - wi / W)
BrowardPop$percentNonwhite <- 1- BrowardPop$percentWhite
BrowardPop$totalNonwhite <- BrowardPop$totalPop - BrowardPop$whitePop

BrowardPop$dissimilarityPreSum <- abs((BrowardPop$totalNonwhite/nonwhitePopBroward) - (BrowardPop$whitePop/whitePopBroward))
dissimilarityIndexBroward2 <- sum(BrowardPop$dissimilarityPreSum)/2

#Measures of Centralization, relative centralization

#Measures of Exposure, relative clustering

#Some resources:
#http://strimas.com/r/tidy-sf/
#http://moderndata.plot.ly/interactive-r-visualizations-with-d3-ggplot2-rstudio/
