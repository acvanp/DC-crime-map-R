# Alex Van Plantinga
# March 2018

# The goal here is 
# that for each week of 2017, make a new street map of crime
# where streets are color coded for the weekly crime rate
# combine the maps sequentially into a gif.

# Merge block centroids and crime stats from shape files into dfs
# by the BLOCK column (data conserved?)

# Merge streets and blocks by STREETSEGI column (data conserved?)
library(plyr)
library(base)
library(bnlearn)
library("rgdal")
library(lubridate)

library("caTools") # make gifs with write.gif() function or use ImageMagick software
library(purrr) # for mapping over a function
library(magick) 

colfunc = colorRampPalette(c("red", "darkorange", "orange", "yellow"))

legend_image <- as.raster(matrix(colfunc(4), ncol=1))


setwd("C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\DC open data\\DC crime 2017")
crimes = readOGR("Crime_Incidents_in_2017.shp")
streets = readOGR("C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\DC open data\\Street_Centerlines\\Street_Centerlines.shp")
blocks = readOGR("C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\DC open data\\Block_Centroids\\Block_Centroids.shp")
boundaries = readOGR("C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\boundaries\\cb_2016_us_state_500k.shp")

# this new color column gets repopulated with a 
# loop depending on monthly crime rate
streets$color  = NA

df.crimes = data.frame(crimes)
df.crimes$week = week(df.crimes$REPORT_DAT)
df.crimes$month = month(df.crimes$REPORT_DAT)

df.streets = data.frame(streets)
df.blocks = data.frame(blocks)
df.blocks$BLOCK = df.blocks$BLOCKNAME

blocks.crimes = merge(df.blocks, df.crimes, "BLOCK")
blocks.crimes.streets = merge(blocks.crimes, streets, "STREETSEGI")

bcs = blocks.crimes.streets
bcs$week = week(bcs$REPORT_DAT)

#collist = c("red", "darkred")
#colfunc<-colorRampPalette(collist)
#streets$color = NA
#streets$color = colfunc(nrow(streets))
#plot(streets, col = streets$color)


mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")


for (i in 1:12){
  a = NA
  b = NA
  c = NA
  d = NA
  e = NA
  df.d = NA
  df.streets = data.frame(streets)
  streets$color = NA
  
  a = df.crimes[which(df.crimes$month == i),]
  b = merge(a, df.blocks, "BLOCK")
  b$counter = 1
  c = aggregate(b$counter ~ b$STREETSEGI, FUN = sum)
  c$color = NA
  
  c$color[which(c$`b$counter` == 1)] = "yellow"
  
  c$color[which(c$`b$counter` == 2)] = "orange"
  
  c$color[which(c$`b$counter` == 3)] = "darkorange"
  
  c$color[which(c$`b$counter` >= 4)] = "red"  
  
  c$color[which(is.na(c$color))] = "gray"  
  
  df.d = c
  colnames(df.d) = c("STREETSEGI", "counts", "color")
  
  df.d = df.d[which(!duplicated(df.d$STREETSEGI)),]
  
  df.d$color = lapply(df.d$color, function(x) if(is.factor(x)) factor(x) else x)
  df.d$color = unlist(df.d$color)
  
  #df.d = join(df.streets, df.d, by = "STREETSEGI")
  
  df.d = merge(df.streets, df.d, "STREETSEGI", all.x = T)
  #df.d = df.d[1:nrow(df.streets),]
  #df.d = nrow(df.d[which(!is.na(df.d$STREETSEGI)),])
  #df.d = data.frame(df.d, stringsAsFactors = FALSE)
  #hist(df.d$counts, breaks = 50)
  
  #hist(df.d$counts, breaks = 50)
  
  df.streets$color = as.character(df.d$color.y)
  
    # not sure whether to gray the NA's
  df.streets$color[which(is.na(df.streets$color))] = "gray"
  
  streets$color = df.streets$color
  id = i
  if(id <= 9){id = paste(0,i, sep = "")}
  
  setwd("C:\\Users\\avanplan\\Documents\\GitHub\\DC-crime-map-R\\gif")
  jpeg(paste(id,"DCcrimeRoads.jpg", sep = ""), width = 800, height = 700)
  
  msq = 9
  mat = rep(1,msq)
  mat[3] = 2
  mat[9] = 3
  layout(matrix(mat, sqrt(msq),sqrt(msq),byrow = TRUE))
    # c(bottom, left, top, right)
  par(mar=c(4,2,1,1))
  
    
  plot(boundaries, ylim = c(38.79323, 38.99526), xlim = c(-77.11664, -76.90953))
  plot(streets, col = streets$color, ylim = c(38.79323, 38.99526), xlim = c(-77.11664, -76.90953), add = TRUE)
  #text(x = -76.9, y = 38.95, mymonths[i], cex = 4)
  
  
  # work on adding histogram subplot/legend
  barcolors = rep(NA, 12)
  barcolors[i] = "black"
  
  #plot(c(0,2),c(0,2),type = 'n', axes = F,xlab = '', ylab = '')
  
  hist(df.crimes$month, breaks = seq(0,12), 
       col = barcolors, main = "2017", xlab = "DC monthly crime rate", ylab = "",
       xaxt = "n", cex.main = 2, cex.lab = 2)
  text(i-0.6, 600, mymonths[i], srt = 90, 
       col = "white", cex = 2)
  
  # color legend
  plot(legend_image, type = 'n', axes = F,xlab = '', ylab = '')
  text(x = -0.5, y = 2, "crime rate per block", cex = 1.5, srt = 90)
  text(x = 1.1, y = 0.5, "1", cex = 2, srt = 0)
  text(x = 1.1, y = 1.5, "2", cex = 2, srt = 0)
  text(x = 1.1, y = 2.5, "3", cex = 2, srt = 0)
  text(x = 1.2, y = 3.5, "4+", cex = 2, srt = 0)
  
  dev.off()
  
}  


list.files(path = "C:\\Users\\avanplan\\Documents\\GitHub\\DC-crime-map-R\\gif", pattern = "*.jpg", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("dc_crime_roads_months_gif.gif") # write to current dir

