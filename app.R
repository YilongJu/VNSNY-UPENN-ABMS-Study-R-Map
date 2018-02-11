# [Load packages] ----
library(rgdal)
library(raster)
library(data.table)
library(shiny)
# library(lazyeval)
library(reshape2)
library(scales)
library(ggmap)
library(Cairo)
library(maptools)
library(rgeos)
library(scales)
library(RColorBrewer)
library(rsconnect)
library(crosstalk)
library(leaflet)
library(OneR)
library(Hmisc)
library(moments)
library(colorspace)
library(classInt)
library(dplyr)


Sys.setenv(TZ = "US/Eastern")
# sudo su -   -c "R -e \"install.packages(c('lazyeval', 'reshape2', 'scales', 'ggmap', 'Cairo', 'maptools', 'rgeos', 'scales', 'RColorBrewer', 'rsconnect', 'plotly', 'crosstalk', 'doParallel', 'leaflet', 'OneR', 'Hmisc', 'moments', 'colorspace', 'classInt', 'data.table'), repos='http://cran.rstudio.com/')\""
# sudo su -   -c "R -e \"install.packages('Cairo', repos='http://cran.rstudio.com/')\""
# sudo su -   -c "R -e \"install.packages('log4r', repos='http://cran.rstudio.com/')\""
# libgtk2.0-dev libcairo2-dev xvfb xauth xfonts-base



# library(rsvg)

# author: "Yilong", "Brooke"
# date: [21 Sep, 2017]

# [Function Declarations] ----
GetBinforVar <- function(data, varName, nbins = 6) {
  # data <- uCT
  # varName <- "subacc"
  # varName <- "propoa"
  numericValues <- unlist(c(data[, varName]), use.names = FALSE)
  nbins_t = nbins
  l <- 0
  iter = 0
  while (l < nbins && iter < 20) {
    bins <- cut2(x = numericValues, g = nbins_t,
                 levels.mean = T, onlycuts = T, digits = 2)
    l <- length(bins)
    nbins_t <- nbins_t + 1
    iter <- iter + 1
  }
  bins <- signif(bins, 3)
  return(bins)
}
GetRadius <- function(varValue, l = 2, u = 32, cap = Inf) {
  if (cap != Inf) {
    cat(varValue, '\n\n') 
    cat(max(varValue, na.rm = T), '\n\n')
    cat(varValue[which(varValue >= cap)], '\n\n') 
    varValue[which(varValue >= cap)] <- max(varValue[which(varValue < cap)],
                                            na.rm = T)
    cat(varValue, '\n\n') 
    cat(max(varValue, na.rm = T), '\n\n')
  }
  range <- range(varValue, na.rm = TRUE)
  varValue <- (varValue - range[1])/(range[2] - range[1]) * (u - l) + l
  return(varValue)
}
ShowColors <- function(col, border = "light gray", ...) {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
# Covert real number vector to percentage vector
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}
GetColorPalByBins <- function(varValues, intervals, color1, color2 = "white") {
  pal <- function(varValues) {
    intervals <- sort(intervals)
    colfunc <- colorRampPalette(c(color2, color1))
    colors <- colfunc(length(intervals) + 1)
    
    colorDeter <- data.frame(sapply(intervals, function(x) {
      x < varValues
    }))
    
    varColorIdx <- apply(colorDeter, 1, sum) + 1
    varColors <- colors[varColorIdx]
    varColors[is.na(varColors)] <- "#000000"
    return(varColors)
  }
  return(pal)
}
GetIcon <- function(iconFilename, width = NULL, height = NULL, iconFileLoc = "data/icons/", iconFileType = "png") {
  iconSize <- 16
  if (is.null(width)) {
    width <- iconSize
  }
  if (is.null(height)) {
    height <- width
  }
  iconFilePath <- paste0(iconFileLoc, iconFilename, ".", iconFileType)
  return (makeIcon(
    iconUrl = iconFilePath,
    iconWidth = width,
    iconHeight = height))
}
# [Read data] ----
# setwd("/Users/yilongju/Dropbox/Study/GitHub/VNSNY-UPENN-ABMS-Study-R-Map")

data <- read.csv("data/ABM_censustract_precinct_111617.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
data_precinct <- data %>% dplyr::select(precpop:offpcap)
data_precinct2 <- data %>% dplyr::select(BoroCT2000, precpop:offpcap)

varDef <- read.csv("data/Variable_Definitions.csv")

ct2000shp <- shapefile("data/nyct2000_12c/nyct2000_12c/nyct2000")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
nypp <- readOGR("data/nypp_17c_police_precinct_shapefile/nypp.shp")

NPIData <- read.csv("NPI_ctuniq.csv", row.names = 1)

# pluto2007_ctuniq <- fread("data/pluto2007_ctuniq.csv")

# [Preprocessing of PLUTO data] ----
#   Select PLUTO data by building types
# str(pluto2007_ctuniq)
#   Building of Interest
BOI <- c("I1", "I3", "I5", "I6", "I7", "K1", "K2", "K3", "K4", "K5", "K6", "P5", "P8", "M1")
# pluto_bldCT <- pluto2007_ctuniq %>%
#   select(ctuniq,
#          xcoord,
#          ycoord,
#          lot,
#          bldgclass,
#          borocode,
#          borough) %>%
#   filter(bldgclass %in% BOI)
# write.csv(pluto_bldCT, "pluto_bldCT.csv")
pluto_bldCT <- fread("data/pluto_bldCT.csv")
#   Give id to each row
pluto_bldCT$id <- rownames(pluto_bldCT)
pluto_bldCT %>% arrange(ctuniq, bldgclass)
head(pluto_bldCT, 20)

pluto_bldCT_summary <- pluto_bldCT %>%
  group_by(ctuniq, bldgclass) %>%
  summarise(count = n())

# [Create building varnames] ----
buildingVarnames <- list(
  I1 = "HOSPITAL",
  I3 = "PHARMACY",
  I5 = "CLINIC",
  I6 = "NURSINGHOME",
  I7 = "ADULTFACILITY",
  K1 = "ONESTORYSTORE",
  K2 = "TWOSTORYSTORE",
  K3 = "DEPTSTORE",
  K4 = "STOREW_APTMNT",
  K5 = "DINERS",
  K6 = "SHOPPINGCNTR",
  K = "STORES",
  P5 = "CMTYCNTR",
  P8 = "LIBRARY",
  M1 = "CHURCHSYN"
)

# [Create PLUTO data columns for ABM data] ----
head(pluto_bldCT_summary)
pluto_bldCT_summary %>% arrange(ctuniq)
bldCount <- data.frame(acast(pluto_bldCT_summary, ctuniq ~ bldgclass))
bldCount[is.na(bldCount)] <- 0

bldCount$K <- bldCount$K1 + bldCount$K2 + bldCount$K3 + bldCount$K4 + bldCount$K5 + bldCount$K6
colnames(bldCount) <- unlist(buildingVarnames[colnames(bldCount)], use.names = F)
bldCount$ctuniq <- rownames(bldCount)
rownames(bldCount) <- 1:nrow(bldCount)
head(bldCount)

head(data)

ABM_PLUTO <- full_join(data, bldCount, by = "ctuniq")
head(ABM_PLUTO, 20)
dim(data)
dim(bldCount)
dim(ABM_PLUTO)

write.csv(ABM_PLUTO, "ABM_PLUTO.csv")



pluto_bldCT[200:210, ]
pluto_bldCT[is.na(pluto_bldCT$xcoord), ]
#   Project coordinates of buildings into ny.map coordinate system
pluto_bldCT_coords <- pluto_bldCT %>% dplyr::select(id, xcoord, ycoord)
pluto_bldCT_coords <- pluto_bldCT_coords[complete.cases(pluto_bldCT_coords), ]
head(pluto_bldCT_coords, 20)
dim(pluto_bldCT_coords)
coordinates(pluto_bldCT_coords) <- ~xcoord + ycoord
proj4string(pluto_bldCT_coords) <- ct2000shp@proj4string
pluto_bldCT_coords <- spTransform(pluto_bldCT_coords, ny.map@proj4string)
head(pluto_bldCT_coords@coords)
pluto_bldCT_coords <- data.frame(id = pluto_bldCT_coords$id,
                                 x = pluto_bldCT_coords@coords[, 1],
                                 y = pluto_bldCT_coords@coords[, 2])
head(pluto_bldCT_coords)
pluto_bldCT <- left_join(pluto_bldCT, pluto_bldCT_coords, by = "id") %>% dplyr::select(-xcoord, -ycoord)
head(pluto_bldCT, 20)

#   Project coordinates of other shape data
ct2000shp <- spTransform(ct2000shp, ny.map@proj4string)
head(ct2000shp@polygons[1])
boros <- spTransform(boros, ny.map@proj4string)
nypp <- spTransform(nypp, ny.map@proj4string)

# [Desciption of attributes] ----
beginRow <- 6
endCol <- 4
varNames <- as.character(varDef$varName[beginRow:nrow(varDef) - 1])
varShortNames <- as.character(varDef$varShortName[beginRow:nrow(varDef) - 1])
showPercentage <- varDef$showPercentage[beginRow:nrow(varDef) - 1]
varDefinitions <- varDef$varFullDefinition[beginRow:nrow(varDef) - 1]
checkboxGroupListIndex <- setNames(as.list(c(1:length(varNames))), varNames)
checkboxGroupList <- setNames(as.list(as.character(varNames)), as.character(varShortNames))
shapeDataList <- setNames(as.list(c("CT", "NB")), c("Census Tract Map", "Neighborhood Map"))
varColors <- rainbow_hcl(length(varNames), c = 190, l = 60, start = 12, end = 300)
# ShowColors(varColors)
# varColors <- terrain.colors(length(varNames))
# varColors <- cm.colors(length(varNames))
# varColors <- rainbow_hcl(length(varNames), start = 60, end = 240)
# varColors <- diverge_hcl(length(varNames),
#                          h = c(800, 300), c = 100, l = c(20, 130), power = 0.4)
# ShowColors(varColors)

# [Prepare precinct shape data] ----
nypp@data$id <- rownames(nypp@data)
f_nypp <- fortify(nypp, polyname = "Precinct")
nypp_DF <- merge(f_nypp, nypp@data, by = "id")

# [Prepare Boros shape data] ----
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
#   aggregate to an upper level
#   offset the label
boros@data$id <- rownames(boros@data)
f_boros <- fortify(boros, polyname = "BoroCode")
boros_DF <- merge(f_boros, boros@data, by = "id")
bnames <- aggregate(data = boros_DF, cbind(long,lat) ~ BoroName, FUN=function(x) mean(range(x)))
# bnames[4,3] <- 200741.5
bnames$BoroName <- as.character(bnames$BoroName)
bnames$BoroName <- as.factor(bnames$BoroName)

# [Prepare ct shape data] ----
#   add to data a new column termed "id" composed of the rownames of data'
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
ct2000shp@data$id <- rownames(ct2000shp@data)
f_ct2000shp <- fortify(ct2000shp, polyname = "BoroCT2000")
ct2000shp_DF <- merge(f_ct2000shp, ct2000shp@data, by = "id")

# [Prepare ny neighborhood shape data, <by Brooke>] ----
#   project the dataframe onto the shape file
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
sodo <- ny.map[ny.map$City == "New York", ]
dat <- data.frame(Longitude = data$ctrdlong, Latitude = data$ctrdlat)
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(sodo)
location = over(dat, sodo)
data = cbind(data, location)
dataProjected <- sodo
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected, region = "id")
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

# [Prepare useful data] ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ==== Add new variable into CT layer:
#       1) Add variable description in "Varialbe_Definitions.csv"
#       2) Add a correspoding column to "data_vars", whose name should be   consistent with the varName in "Varialbe_Definitions.csv"
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_ids <- data %>% dplyr::select(BoroCT2000, Name)
data_vars <- data %>% dplyr::select(popdens:propnonw, offpcap)
data_coords <- data[, c("ctrdlong", "ctrdlat")]
data_necessary <- cbind(data_ids, data_vars, data_coords)
# --- For CT
uCT <- data_necessary %>%
  group_by(BoroCT2000) %>%
  summarise_all(funs(mean))
ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
uCT$BoroCT2000 <- as.character(uCT$BoroCT2000)
dfCT <- dplyr::left_join(ct2000shp_DF, uCT, by = "BoroCT2000")
#   --- Find center of view
center_ct.map <- ct2000shp_DF %>%
  dplyr::select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ct.map # -73.91271 40.69984
#   --- Combine shapefile with data
ct2000shp_attr <- ct2000shp
ct2000shp_attr@data <- dplyr::left_join(ct2000shp_attr@data, uCT, by = "BoroCT2000")
head(ct2000shp_attr@data)

# --- For NB
uNB <- data_necessary %>%
  group_by(Name) %>%
  summarise_all(funs(mean))
dfNB <- dplyr::left_join(watershedDF, uNB, by = "Name")
#   --- Find center of view
center_ny.map <- watershedDF %>%
  dplyr::select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ny.map # -73.92194 40.68922
#   --- Combine shapefile with data
ny.map_attr <- ny.map
ny.map_attr@data <- dplyr::left_join(ny.map_attr@data, uNB, by = "Name")

data_necessary$BoroCT2000 <- as.character(data_necessary$BoroCT2000)
NBname_CTntaname <- full_join(data_necessary, ct2000shp_DF, by = "BoroCT2000")
NBname_CTntaname <- NBname_CTntaname %>%
  distinct(Name, NTANAme) %>%
  arrange(Name)
NBname_CTntaname <- aggregate(NTANAme ~ Name, data = NBname_CTntaname,
                              FUN = paste0, collapse = "<br/>")

ny.map_attr@data <- left_join(ny.map_attr@data, NBname_CTntaname, by = "Name")

# --- For Boro
data_necessary_Boro <- cbind(data_necessary, borocodenum = data$borocodenum)
tbl_df(data_necessary_Boro)
data_necessary_Boro[is.na(data_necessary_Boro)] <- 0
uBR <- data_necessary_Boro %>%
  group_by(borocodenum) %>%
  summarise_all(funs(mean))
head(uBR)

dfBR <- dplyr::left_join(boros_DF, uBR, by = c("BoroCode" = "borocodenum"))
#   --- Combine shapefile with data
boros_attr <- boros
boros_attr@data <- dplyr::left_join(boros_attr@data, uBR, by = c("BoroCode" = "borocodenum"))

# --- For Precinct
data_precinct[is.na(data_precinct)]

uPP <- data_precinct %>%
  group_by(precinct) %>%
  summarise_all(funs(mean))
head(uPP)

dfPP <- dplyr::left_join(nypp_DF, uPP, by = c("Precinct" = "precinct"))
#   --- Combine shapefile with data
nypp_attr <- nypp
nypp_attr@data <- dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct"))

CTname <- dfCT %>% dplyr::select(BoroCT2000, NTANAme)
data_precinct2$BoroCT2000 <- as.character(data_precinct2$BoroCT2000)
CTname$BoroCT2000 <- as.character(CTname$BoroCT2000)
Precinct_CTntaname <- full_join(data_precinct2, CTname, by = "BoroCT2000")

Precinct_CTntaname <- Precinct_CTntaname %>%
  distinct(precinct, NTANAme) %>%
  arrange(precinct)
head(Precinct_CTntaname, 20)
Precinct_CTntaname_aggr <- aggregate(NTANAme ~ precinct, data = Precinct_CTntaname,
                              FUN = paste0, collapse = "<br/>")
head(Precinct_CTntaname_aggr, 20)
nypp_attr@data <- left_join(nypp_attr@data, Precinct_CTntaname_aggr,
                            by = c("Precinct" = "precinct"))

#   --- Prepare precinct data for plot
precinct_varValues <- nypp_attr$offpcap
precinct_labels <- sprintf("<strong>Precinct %s</strong><br/><b><u>Major offense per capita:</u></b> %g<br/>",
                  nypp_attr@data$Precinct, signif(precinct_varValues, 4))
precinct_labels <- paste0(precinct_labels, "<strong>CTs:</strong><br/>", nypp_attr@data$NTANAme)
precinct_labels <- lapply(precinct_labels, HTML)
intervals <- c(0.015, 0.020, 0.025, 0.030, 0.035, 0.065, 0.1, 0.2, 0.5, 1, 5)
intervalsA <- c(0, intervals[1:length(intervals)-1])
intervalsB <- c(intervals)
intervalLable <- paste0(sprintf("%.3f", intervalsA),
                        " - ",
                        sprintf("%.3f", intervalsB))
intervalLable <- c(intervalLable, paste0(" > ", sprintf("%.3f", intervals[length(intervals)])), "NA")

colfunc <- colorRampPalette(c("white", "red"))
colors <- colfunc(length(intervals) + 1)
colors <- c(colors, "#000000")

precinct_pal <- GetColorPalByBins(precinct_varValues, intervals, "red")

precinct_groupName <- "Percapita offense"
boro_groupName <- "Boros"

# --- For PLUTO data
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

# For all kinds of buildings, refer to https://stackoverflow.com/questions/46286599/custom-markers-on-shiny-leaflet-map
# I1icon <- rsvg("data/icons/I1.svg")
# dim(I1icon)
# rsvg_png("data/icons/I1.svg", "data/icons/I1.png")

# [Load icons] ----
buildingIcons <- iconList(
  I1 = GetIcon("I1"),
  I3 = GetIcon("I3"),
  I5 = GetIcon("I5"),
  I6 = GetIcon("I6"),
  I7 = GetIcon("I7"),
  K1 = GetIcon("K1", 12),
  K2 = GetIcon("K2"),
  K3 = GetIcon("K3"),
  K4 = GetIcon("K4"),
  K5 = GetIcon("K5"),
  K6 = GetIcon("K6"),
  P5 = GetIcon("P5"),
  P8 = GetIcon("P8"),
  M1 = GetIcon("M1")
)
# [Create building labels] ----
buildingLabels <- list(
  I1 = "Hospital",
  I3 = "Dispensary",
  I5 = "Clinic",
  I6 = "Nursing Home",
  I7 = "Adult Facility",
  K1 = "One Story Store",
  K2 = "Two Story, Store & Office",
  K3 = "Department Stores, Multi-Story",
  K4 = "Stores, Apartment Above",
  K5 = "Diners, Franchised Type Stand",
  K6 = "Shopping Centers with Parking",
  P5 = "Community Center",
  P8 = "Library",
  M1 = "Church"
)
unlist(buildingLabels["I1"], use.names = FALSE)
uniqueBuildingLabel <- data.frame(buildingLabel = unlist(buildingLabels, use.names = FALSE))
uniqueBuildingLabel <- uniqueBuildingLabel %>% distinct(buildingLabel)
uniqueBuildingLabel <- as.character(uniqueBuildingLabel[, 1])

pluto_bldCT_tmp <- pluto_bldCT %>% filter(bldgclass == "I3")

# --- For NPI data
head(NPIData)
NPI_majorOrganization <- NPIData %>% 
  group_by(organization_name.legal_business_name.) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
NPI_majorOrganization <- NPI_majorOrganization[-1, ]
colnames(NPI_majorOrganization) <- c("organization", "count")
head(NPI_majorOrganization, 20)
dim(NPI_majorOrganization)
organization_fullName <- NPI_majorOrganization$organization
organization_fullName_20 <- NPI_majorOrganization$organization[1:20]
organization_abbr <- c(
  "NYU",
  "Montefiore",
  "MSSM",
  "Weill Cornell"
)
organizationLabels <- list(
  
)
NPI_tmp <- pluto_bldCT %>% filter(bldgclass == "I3")



# [Test leaflet] ----
if (0) {
  map <- leaflet(ny.map_attr) %>% 
    setView(-73.91271, 40.69984, 11) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
    # addPolygons(weight = 4, color = "red") %>%
    # addPolygons(data = ct2000shp_attr, weight = 4, color = "blue") %>%
    # [Add polygons] ----
    addPolygons(
      data = boros_attr,
      fillColor = "white",
      weight = 4,
      opacity = 1,
      color = "black",
      dashArray = "3",
      fillOpacity = 0.3,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.3,
        bringToFront = F),
      group = boro_groupName) %>%
    addPolygons(
      data = nypp_attr,
      fillColor = precinct_pal(precinct_varValues),
      weight = 1,
      opacity = 1,
      color = "red",
      dashArray = "3",
      fillOpacity = 0.3,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.3,
        bringToFront = F),
      label = precinct_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      # popup = tileVar,
      group = precinct_groupName
    ) %>%
    addLegend(
      colors = colors,
      labels = intervalLable,
      opacity = 0.7,
      title = "offpcap",
      position = "bottomright",
      labFormat = labelFormat(),
      group = precinct_groupName
    )
    # [Add markers] ----
  
  for (buildingSymbol in BOI) {
    pluto_bldCT_tmp <- pluto_bldCT %>%
      filter(bldgclass == buildingSymbol)
    map <- map %>%
      addMarkers(data = pluto_bldCT_tmp,
                 lng = ~x, lat = ~y,
                 # icon = greenLeafIcon
                 icon = buildingIcons[buildingSymbol],
                 group = unlist(buildingLabels[buildingSymbol], use.names = FALSE)
                 ) %>%
      hideGroup(unlist(buildingLabels[buildingSymbol], use.names = FALSE))
  }
  map <- map %>%
    addLayersControl(
      baseGroups = c("Grey map", "Standard map", "Dark map"),
      overlayGroups = uniqueBuildingLabel,
      position = "topleft",
      options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE))
  
  map
  map <- leaflet(ny.map_attr) %>% 
    setView(-73.91271, 40.69984, 11) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
    # addPolygons(weight = 4, color = "red") %>%
    # addPolygons(data = ct2000shp_attr, weight = 4, color = "blue") %>%
    # [Add polygons] ----
  addPolygons(
    data = boros_attr,
    fillColor = "white",
    weight = 4,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.3,
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.3,
      bringToFront = F),
    group = boro_groupName) %>%
    addPolygons(
      data = nypp_attr,
      fillColor = precinct_pal(precinct_varValues),
      weight = 1,
      opacity = 1,
      color = "red",
      dashArray = "3",
      fillOpacity = 0.3,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.3,
        bringToFront = F),
      label = precinct_labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal",
                     padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      # popup = tileVar,
      group = precinct_groupName
    ) %>%
    addLegend(
      colors = colors,
      labels = intervalLable,
      opacity = 0.7,
      title = "offpcap",
      position = "bottomright",
      labFormat = labelFormat(),
      group = precinct_groupName
    ) %>%
    
  # [Add markers] ----
  map <- map %>%
    addLayersControl(
      baseGroups = c("Grey map", "Standard map", "Dark map"),
      position = "topleft",
      options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE))
  
  map
}


# [Define server ui] ----
ui <- navbarPage(title = "VNSNY/UPENN ABMS Study",
  tabPanel(
   title = "ABM Census",
   h3("Select a variable to show on the tile and check others on the map. (Wait for about 10s for plotting.)"),
    # Sidebar layout with input and output definitions
    sidebarLayout(
      fluidRow(
        column(
          radioButtons(inputId = "ChooseShapefileID",
                      label = h4("Select which map to show"),
                      choices = shapeDataList),
          selectInput("ChooseTileVar",
                      "Variable to show on the tile:",
                      unlist(checkboxGroupList)),
          verbatimTextOutput("displaySomething"),
          # radioButtons(inputId = "ChooseTileVarID2",
          #              label = h4("Select which variable to show on tile"),
          #              choices = checkboxGroupList),
          # actionButton("Plot", "Draw plot"),
          
          # fluidRow("Click the button to plot."),
          width = 3,
          offset = 1
        ),
        column(
          checkboxGroupInput(inputId = "ChooseLabelVars",
                       label = h4("Check variable definitions"),
                       choices = checkboxGroupList,
                       inline = FALSE),
          width = 3
        ),
        column(
          verbatimTextOutput("displaySomething2"),
          uiOutput("varDefOutput"),
          width = 5
        )
      ),
      fluidRow(
        column(
          leafletOutput(outputId = "outputMap", width = "99%", height = 1000),
          width = 11,
          offset = 1
        )
      )
    )
  ),
  tabPanel("Reserved Slot", h3("This is the second panel"),
    # Sidebar layout with input and output definitions
    sidebarLayout(
      # Sidebar panel for inputs
      sidebarPanel(
      # Input: Slider for the number of bins
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 1,
        max = 50,
        value = 30)
      ),
      # Main panel for displaying outputs
      mainPanel(
        # Output: Histogram
        plotOutput(outputId = "distPlot")
      )
    )
  )
)

cat("==================")
# [Define server logic] ----
server <- function(input, output, session) {
  
  # A test plot for other usage
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  # Create reactive variables that will reactive to user input (Select CT / NB)
  #   Variable shown on tile
  tileVar_r <- reactive({input$ChooseTileVar})
  map_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr)
    }
  })
  #   Label of shapedata
  mapDataDisplayLabel_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr$NTANAme)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr$Name)
    }
  })
  #   Useful data for tile
  utilData_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(uCT)
    } else if (input$ChooseShapefileID == "NB") {
      return(uNB)
    }
  })
  #   Set range of radius of circles
  radiusRange_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(c(1,12))
    } else if (input$ChooseShapefileID == "NB") {
      return(c(2,32))
    }
  })
  #   Whether show CT names in hoverinfo
  CTNames_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(FALSE)
    } else if (input$ChooseShapefileID == "NB") {
      return(TRUE)
    }
  })
  # Generate variable definitions
  varDefOutput_r <- reactive({
    label <- "<h4>Variable Definitions</h4>"
    for (var in input$ChooseLabelVars) {
      varId <- checkboxGroupListIndex[[var]]
      varSN <- varShortNames[varId]
      varD <- varDefinitions[varId]
      label <- paste0(label, "<h5>", varSN, ":</h5>", varD, "<br/>")
    }
    HTML(label)
  })
  
  # Show variable definitions
  output$varDefOutput <- renderUI({
    varDefOutput_r()
  })
  
  # Show leaflet base map
  output$outputMap <- renderLeaflet({
    # Initialize map components
    map <- map_r()
    mapData <- map@data
    mapDataDisplayLabel <- mapDataDisplayLabel_r()
    utilData <- utilData_r()
    radiusRange <- radiusRange_r()
    CTNames <- CTNames_r()
  
    Lmap <- leaflet(map) %>% 
    setView(-73.91271, 40.69984, 11) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
    addLayersControl(
      baseGroups = c("Grey map", "Standard map", "Dark map"),
      position = "topleft",
      options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
    )
    
    # Add boro layer
    Lmap <- Lmap %>%
      addPolygons(
        data = boros_attr,
        weight = 7,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = 0,
        group = boro_groupName)  
    
    # Add building layer
    for (buildingSymbol in BOI) {
      pluto_bldCT_tmp <- pluto_bldCT %>%
        filter(bldgclass == buildingSymbol)
      Lmap <- Lmap %>%
        addMarkers(data = pluto_bldCT_tmp,
                   lng = ~x, lat = ~y,
                   icon = buildingIcons[buildingSymbol],
                   clusterOptions = markerClusterOptions(
                     disableClusteringAtZoom = 14,
                     PlacementStrategies = "original-locations"
                   ),
                   group = unlist(buildingLabels[buildingSymbol], use.names = FALSE)) %>%
        hideGroup(unlist(buildingLabels[buildingSymbol], use.names = FALSE))
    }

    cat("------------ 1 ------------")
    labelVars <- varNames
    # Add all other variables to be shown as circles
    if (length(labelVars) > 0) {
      groupNameList <- c(NULL)
      for (labelVar in labelVars) {
        labelVarIdx <- checkboxGroupListIndex[[labelVar]]
        pal2 <- colorBin(varColors[labelVarIdx], domain = mapData[, labelVar], bins = 6)
        groupName <- varShortNames[labelVarIdx]
        
        if (labelVar == "offpcap") {
          currentWeights <- GetRadius(mapData[, labelVar],
                                      radiusRange[1], radiusRange[2], 0.7278)
        } else {
          currentWeights <- GetRadius(mapData[, labelVar],
                                      radiusRange[1], radiusRange[2])
        }
        
        Lmap <- Lmap %>%
          addCircles(
            lng = ~ctrdlong, lat = ~ctrdlat,
            weight = currentWeights,
            fill = FALSE,
            color = varColors[labelVarIdx],
            stroke = T, fillOpacity = 0.6, opacity = 0.6,
            group = groupName
          ) %>%
          addLegend(
            colors = varColors[labelVarIdx],
            labels = paste0("<b>", varShortNames[labelVarIdx], "</b>"),
            opacity = 0.7,
            title = NULL,
            position = "bottomright",
            group = groupName
          ) %>%
          hideGroup(groupName)
        groupNameList <- c(groupNameList, groupName)
      }
    }
    Lmap
  })
  cat("------------ 1.5 ------------")
  # Observe command from map control, hide / show layers
  observe({
    # Initialize map components
    map <- map_r()
    mapData <- map@data
    mapDataDisplayLabel <- mapDataDisplayLabel_r()
    utilData <- utilData_r()
    radiusRange <- radiusRange_r()
    CTNames <- CTNames_r()
    
    # map <- ct2000shp_attr
    tileVar <- input$ChooseTileVar
    # tileVar <- "subdens"
    # tileVar <- "subacc"
    # tileVar <- "popdens"
    restVars <- varNames[varNames != tileVar]
    labelVars <- restVars
    # labelVars <- varNames
    tileVarIdx <- checkboxGroupListIndex[[tileVar]]
    
    if(showPercentage[tileVarIdx] == 1) {
      varValues <- mapData[, tileVar]*100
      labels <- sprintf("<h3><strong>%s</strong></h3><br/><b><u>%s:</u></b> %g%%<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
    } else {
      varValues <- mapData[, tileVar]
      labels <- sprintf("<strong>%s</strong><br/><b><u>%s:</u></b> %g<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
    }
    for (labelVar in labelVars) {
      # labelVar <- labelVars[1]
      labelVarIdx <- checkboxGroupListIndex[[labelVar]]
      if(showPercentage[labelVarIdx] == 1) {
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(100*mapData[, labelVar], 4), "%<br/>")
      } else {
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(mapData[, labelVar], 4), "<br/>")
      }
    }
    if (CTNames) {
      labels <- paste0(labels, "<strong>CTs:</strong><br/>", mapData$NTANAme)
    }
    cat("------------ 1.6 ------------")
    labels <- lapply(labels, HTML)
    colfunc <- colorRampPalette(c("white", varColors[tileVarIdx]))
    
    if (abs(skewness(as.numeric(as.character(varValues)), na.rm = T)) > 1 | tileVar == "subacc") {
      pal <- colorBin(colfunc(6), domain = varValues, n = 6)
    } else {
      pal <- colorQuantile(colfunc(6), domain = varValues, n = 6)
    }
    
    # Create a proxy for Leaflet map, saving render time
    proxy <- leafletProxy("outputMap", data = map)
    cat("------------ 2 ------------")
    # Generate Leaflet map layers
    proxy <- proxy %>%
      clearGroup(group = "tileLayer") %>%
      # Add tile layer
      addPolygons(
        fillColor = pal(varValues),
        weight = 1,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = T),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        # popup = tileVar,
        group = "tileLayer"
      ) %>% 
      clearControls() %>%
      # Add corresponding legend
      addLegend(
        pal = pal,
        values = varValues,
        opacity = 0.7,
        title = varShortNames[tileVarIdx],
        position = "bottomright",
        group = paste0("T_", varShortNames[tileVarIdx])
      )
      
    cat("------------ 3 ------------")
      # Add map control
      proxy <- proxy %>%
        addLayersControl(
          baseGroups = c("Grey map", "Standard map", "Dark map"),
          overlayGroups = c(uniqueBuildingLabel,
                            varShortNames),
          position = "topleft",
          options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
        )
      
      for (groupName in varShortNames) {
        proxy <- proxy %>%
          showGroup(groupName) %>%
          hideGroup(groupName)
      }
      # proxy <- proxy %>%
      #   hide
    
    proxy
  })
  
  # [Reserved functionalities] ----
  # # Set up a variable to control the drawing [For buttons]
  # v <- reactiveValues(doPlot = FALSE)
  # # Detect whether the button is clicked
  # observeEvent(input$Plot, {
  #   v$doPlot <- input$Plot
  # })

  # Use for buttons and progress bars
  # plotGIS_r <- reactive({
  #   # # If the button is not clicked, do not draw
  #   # if (v$doPlot == FALSE) {}
  #   # Create a Progress object [For progess bar]
  #   # progress <- shiny::Progress$new()
  #   # Initialize the progressbar
  #   # progress$set(message = "Plotting...", value = 0)
  #     # progress$inc(1/5, detail = "Initializing...")
  #     # progress$inc(1/5, detail = "Adding tiles...")
  #     # progress$inc(1/5, detail = "Showing variables...")
  #     # progress$inc(1/5, detail = "Showing more variables...")
  #     # progress$inc(1/5, detail = "Adding legends...")
  #   # on.exit(progress$close())
  #   # Isolate the plot code, maintain the old plot until the button is clicked again
  #   # Make sure it closes when we exit this reactive, even if there's an error
  # })
  
  # # For debugging
  # output$displaySomething <- renderPrint({})
  # output$displaySomething2 <- renderPrint({})
}
# [Run server] ----
shinyApp(ui = ui, server = server)

