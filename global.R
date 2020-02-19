library(leaflet)
library(dplyr)
library(rgdal)
library(ggplot2)
library(tidyr)
library(googleVis)
library(RColorBrewer)
library(shinyWidgets) 
library(colorspace)
library(RColorBrewer)
library(colorRamps)
library(shinydashboard)
library(visNetwork) 
library(geosphere)
library(igraph)
library(rgeos)



labxy<- read.csv('Testing Lab-XY.CSV', header = T, sep = ',', stringsAsFactors = T)
df2 <-  read.csv('lab_fac_agg_perc.csv', header = T, sep = ',', stringsAsFactors = T)
dyadxy <- read.csv('dyadxy.csv', header = T) 

labfac.df <- read.csv("lab_fac_agg_network.csv", header=T, as.is=T)

mwi1<-readOGR(dsn='Malawi_shape_file', layer = 'MWI_adm0')

dyadxy <- dyadxy %>% group_by(lab) %>%
  mutate(per=(round(patients*100/sum(patients),4))) %>%
  ungroup

# dyadxy_hull= na.omit(dyadxy) %>%
#   group_by(lab) %>%
#   mutate(hull = 1:n(), hull = factor(hull, chull(lat, lon))) %>%
#   arrange(hull)

names(labfac.df) <- c("lab", "fac", "fac.code", "patients", 
                      "fac.lon", "fac.lat", "lab.lon", "lab.lat")

dist <- distGeo(p1=data.matrix(labfac.df[5:6]),
                p2=data.matrix(labfac.df[7:8]), 
                a=6378137, f=1/298.257223563)

labfac.d <-cbind(labfac.df,dist)

labfac.d$dist.k <- labfac.d$dist/1000


lf.edge <- labfac.d[,c(1,2,4,10,3)]


## Creating the node dataset

# Getting  unique labs and facilities to assign IDs
lab.node <- unique(labfac.d[1])
lab.node.c <- unique(labfac.d[,c(1,7,8)])

fac.node <- unique(labfac.d[2])
fac.node.c <- unique(labfac.d[,c(2,5,6)])

# Creating unique id for each lab
lab.node.c$ID <- seq.int(nrow(lab.node.c))
lab.node.c$IDx <- paste("L", lab.node.c$ID, sep="")

# Creating unique id for each facility
fac.node.c$ID <- seq.int(nrow(fac.node.c))
fac.node.c$IDx <- paste("F", fac.node.c$ID, sep="")

lab.node.c$types <- "Lab"
fac.node.c$types <- "Facility"

#restructuring the data
labs.node <- lab.node.c[,c(5,1,2,3,6)]
facs.node <- fac.node.c[,c(5,1,2,3,6)]


names(labs.node) <- c("id", "Site", "lon", "lat", "Types")
names(facs.node) <- c("id", "Site", "lon", "lat", "Types")

lf.nodes <- rbind(labs.node, facs.node)

## Creating edges dataset
# Lab id dataset
lab.id <- labs.node[,c(1,2)]
names(lab.id) <- c("from", "Site")
# Merging Lab IDs on edges datasetL
lab.edge <- left_join(lf.edge, lab.id, by = c("lab"="Site"))

# Facility id dataset
fac.id <- facs.node[,c(1,2)]
names(fac.id) <- c("to", "Site")
# Merging Lab IDs on edges datasetL
labfac.e <- left_join(lab.edge, fac.id, by = c("fac"="Site"))


edge.lf <- labfac.e[,c(6,7,3,4)]
node.lf <- lf.nodes


names(node.lf) <- c("id", "name", "lon", "lat", "group")


#Adding attributes of colors, shapes based on whether they are labs or facilities
node.lf$color <- ifelse(node.lf$group %in% ("Lab"), "orange", "blue")
# node.lf$border <- ifelse(node.lf$group %in% ("Lab"), "red", "darkblue")
node.lf$title = paste0("<p><b>", node.lf$name)
node.lf$shape <- ifelse(node.lf$group %in% ("Lab"), "square", "circle")
node.lf$Laboratory <- ifelse(node.lf$id %in% (paste("L",c(1:9), sep="")), 
                             paste(node.lf$id, ": ", node.lf$name, sep=""),
                             "")

meanx <- mean(edge.lf$dist.k, na.rm=T)


# Adding attributes to the edges
edge.lf$length <- ifelse(is.na(edge.lf$dist.k), 
                         mean(edge.lf$dist.k, na.rm=T)/2, 
                         edge.lf$dist.k)

edge.lf$dashes <- ifelse(is.na(edge.lf$dist.k), T, F)



edge.lfx <- edge.lf %>% group_by(from) %>%
  mutate(per=(round(patients/sum(patients)*100,4))) %>%
  ungroup

edge.lfx$width <- edge.lfx$per
edge.lfx$title <- paste0("<p>","n=", edge.lfx$patients, ", ", 
                         round(edge.lfx$dist.k,1), "km")
#edge.lfx$color <- c("green")
edge.lfx$smooth <- c(F)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###graphing Final overall network
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
visNetwork(node.lf, edge.lfx) %>% 
  visOptions(highlightNearest =list(enabled=T),
             selectedBy = list(variable = "Laboratory", multiple = T)) %>%
  visNodes(color=list(border = "darkblue")) %>%
  visLayout(randomSeed = 123) %>%
  visEdges(physics = F) %>%
  #visPhysics(enabled=F) %>%
  visIgraphLayout() %>%
  visInteraction(navigationButtons = TRUE)