# Alex Van Plantinga
# March 2018

# The goal here is 
# that for each week of 2017, make a new street map of crime
# where streets are color coded for the weekly crime rate
# combine the maps sequentially into a gif.

# Merge block centroids and crime stats from shape files into dfs
# by the BLOCK column (data conserved?)

# Merge streets and blocks by STREETSEGI column (data conserved?)

library(base)
library(bnlearn)
library("rgdal")
library(lubridate)

library("caTools") # make gifs with write.gif() function or use ImageMagick software
library(purrr) # for mapping over a function
library(magick) 


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

collist = c("deeppink","darkred")
colfunc<-colorRampPalette(collist)

streets$color = colfunc(nrow(streets))
plot(streets, col = streets$color)
plot(streets.df)


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
  df.streets$color = NA
  streets$color = NA

  a = df.crimes[which(df.crimes$month == i),]
  b = merge(a, df.blocks, "BLOCK")
  b$counter = 1
  c = aggregate(b$counter ~ b$BLOCK, FUN = sum)
  c$color = NA
  cfunc = colfunc(max(c$`b$counter`))
    for(j in 1:nrow(c)){
      c$color[j] = cfunc[c$`b$counter`[j]]
    }
  c$BLOCK = c$`b$BLOCK`
  #c$STREETSEGI = NA
  d = merge(c, df.blocks, "BLOCK")
  df.d = data.frame(d$STREETSEGI, d$color)
  colnames(df.d) = c("STREETSEGI", "color")
  
  df.d = df.d[which(!duplicated(df.d$STREETSEGI)),]
  df.d = df.d[order(df.streets$STREETSEGI),]
  df.streets$color = as.character(df.d$color)
  df.streets$color[which(is.na(df.streets$color))] = "gray"
  # make default no crime color
  
  streets$color = df.streets$color
  id = i
  if(id <= 9){id = paste(0,i, sep = "")}
  
  setwd("C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\DC open data\\DC crime 2017\\plots\\gif")
  jpeg(paste(id,"DCcrimeRoads.jpg", sep = ""), width = 800, height = 700)
  
  plot(boundaries, ylim = c(38.79323, 38.99526), xlim = c(-77.11664, -76.90953))
  plot(streets, col = streets$color, ylim = c(38.79323, 38.99526), xlim = c(-77.11664, -76.90953), add = TRUE)
  text(x = -77, y = 38.8, mymonths[i], cex = 1)
  
  dev.off()

}  


list.files(path = "C:\\Users\\avanplan\\Dropbox\\Personal projects\\data sets\\DC open data\\DC crime 2017\\plots\\gif", pattern = "*.jpg", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("dc_crime_roads_months_gif.gif") # write to current dir
