# [Load packages] ----
library(rgdal)
library(raster)
library(data.table)
library(shiny)
library(shinyBS)
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
library(stringdist)
library(ggplot2)
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
    varValue[which(varValue >= cap)] <- max(varValue[which(varValue < cap)],
                                            na.rm = T)
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
debugMode <- T


varDef <- read.csv("data/Variable_Definitions.csv")

ct2000shp <- shapefile("data/nyct2000_12c/nyct2000_12c/nyct2000")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
nypp <- readOGR("data/nypp_17c_police_precinct_shapefile/nypp.shp")

# NPIData <- read.csv("NPI_ctuniq.csv", row.names = 1)
data <- fread("data/ABM_censustract_precinct_111617.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]

CMS_patient <- fread("data/CMS_patient_data2.csv")
CMS_patient <- CMS_patient[, -1]
dim(CMS_patient)
head(CMS_patient)
sum(CMS_patient$num_patients == CMS_patient$num_pat_08, na.rm = T)
CMS_patient[CMS_patient == -100] <- NA

CMS_patient <- CMS_patient %>% mutate(
  avg_er_charges_tot = tot_er_charges / tot_er_visits,
  avg_charges_tot = tot_charges / tot_visits
) %>% dplyr::select(CT2000_unique, avg_er_charges_tot, avg_charges_tot)

data <- left_join(data, CMS_patient, by = c("ctuniq" = "CT2000_unique"))
head(data)

chrono_conditions <- fread("data/chronic_conditons_by_CT.csv")
chrono_conditions <- chrono_conditions[, -1]
chrono_conditions <- chrono_conditions %>% dplyr::select(-numPatients)
head(chrono_conditions)
data <- left_join(data, chrono_conditions, by = c("ctuniq" = "CT2000_unique"))
head(data)

# dataName <- read.csv("data/data.csv")
# head(dataName)
# dataName <- dataName %>% select(ctuniq, Name)
# write.csv(dataName, "CTNames.csv")
dataName <- read.csv("data/CTNames.csv")
# data <- left_join(data, dataName, by = "ctuniq")

# pluto2007_ctuniq <- fread("data/pluto2007_ctuniq.csv")

# [Preprocessing of PLUTO data] ----
#   Select PLUTO data by building types
# str(pluto2007_ctuniq)
#   Building of Interest
BOI <- c("I1", "I3", "I5", "I6", "I7", "K1", "K2", "K3", "K4", "K5", "K6", "P5", "P8", "M1")
# pluto_bldCT <- pluto2007_ctuniq %>%
#   select(ctuniq,
#          x,
#          y,
#          lot,
#          bldgclass,
#          borocode,
#          borough) %>%
#   filter(bldgclass %in% BOI)
# write.csv(pluto_bldCT, "pluto_bldCT.csv")
pluto_bldCT <- fread("data/pluto_bldCT.csv")
#   Give id to each row
pluto_bldCT$id <- rownames(pluto_bldCT)

# pluto_bldCT_summary <- pluto_bldCT %>%
#   group_by(ctuniq, bldgclass) %>%
#   summarise(count = n())

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
# bldCount <- data.frame(acast(pluto_bldCT_summary, ctuniq ~ bldgclass))
# bldCount[is.na(bldCount)] <- 0
# 
# bldCount$K <- bldCount$K1 + bldCount$K2 + bldCount$K3 + bldCount$K4 + bldCount$K5 + bldCount$K6
# colnames(bldCount) <- unlist(buildingVarnames[colnames(bldCount)], use.names = F)
# bldCount$ctuniq <- rownames(bldCount)
# rownames(bldCount) <- 1:nrow(bldCount)
# 
# ABM_PLUTO <- full_join(data, bldCount, by = "ctuniq")
# write.csv(ABM_PLUTO, "ABM_PLUTO.csv")

if (debugMode) cat("*************** 1 ***************")
#   Project coordinates of buildings into ny.map coordinate system
pluto_bldCT_coords <- pluto_bldCT %>% dplyr::select(id, x, y)
pluto_bldCT_coords <- pluto_bldCT_coords[complete.cases(pluto_bldCT_coords), ]
coordinates(pluto_bldCT_coords) <- ~x + y
proj4string(pluto_bldCT_coords) <- ct2000shp@proj4string
pluto_bldCT_coords <- spTransform(pluto_bldCT_coords, ny.map@proj4string)
pluto_bldCT_coords <- data.frame(id = pluto_bldCT_coords$id,
                                 x = pluto_bldCT_coords@coords[, 1],
                                 y = pluto_bldCT_coords@coords[, 2])
pluto_bldCT <- left_join(pluto_bldCT, pluto_bldCT_coords, by = "id") %>% dplyr::select(-x, -y)
write.csv(pluto_bldCT[, -1], "data/pluto_bldCT.csv")
# pluto_bldCT <- read.csv("data/pluto_bldCT.csv")

#   Project coordinates of other shape data
ct2000shp <- spTransform(ct2000shp, ny.map@proj4string)
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

# [Prepare precinct shape data] ----
nypp@data$id <- rownames(nypp@data)
f_nypp <- fortify(nypp, polyname = "Precinct")
nypp_DF <- merge(f_nypp, nypp@data, by = "id")
# if (debugMode) cat("*************** 2 ***************")
# [Prepare Boros shape data] ----
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
#   aggregate to an upper level
#   offset the label
boros@data$id <- rownames(boros@data)

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
location <- over(dat, sodo)
data <- cbind(data, location)
# write.csv(data, "data/data.csv")
# data <- read.csv("data/data.csv")
dataProjected <- sodo
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected, region = "id")
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

# [Prepare useful data] ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ==== Add new variable into CT layer:
#       1) Add variable description in "Varialbe_Definitions.csv" !!csv file!!
#       2) Add a correspoding column to "data_vars", whose name should be consistent with the varName in "Varialbe_Definitions.csv"
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_ids <- data %>% dplyr::select(BoroCT2000, Name)
data_vars <- data %>% dplyr::select(popdens:propnonw, offpcap, num_er_08:avg_charges_tot)
data_coords <- data[, c("ctrdlong", "ctrdlat")]
data_necessary <- cbind(data_ids, data_vars, data_coords)
# --- For CT
uCT <- data_necessary %>%
  group_by(BoroCT2000) %>%
  summarise_all(funs(mean))
write.csv(uCT, "data/uCT.csv")
# uCT <- read.csv("data/uCT.csv")
ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
uCT$BoroCT2000 <- as.character(uCT$BoroCT2000)
dfCT <- dplyr::left_join(ct2000shp_DF, uCT, by = "BoroCT2000")
# #   --- Find center of view
# center_ct.map <- ct2000shp_DF %>%
#   dplyr::select(long, lat) %>%
#   summarise(ctrlong = mean(long), ctrlat = mean(lat))
# center_ct.map # -73.91271 40.69984
# #   --- Combine shapefile with data
ct2000shp_attr <- ct2000shp
ct2000shp_attr@data <- dplyr::left_join(ct2000shp_attr@data, uCT, by = "BoroCT2000")
write.csv(ct2000shp_attr@data, "data/ct2000shp_attr_data.csv")
# ct2000shp_attr@data <- read.csv("data/ct2000shp_attr_data.csv")


# --- For NB
uNB <- data_necessary %>%
  group_by(Name) %>%
  summarise_all(funs(mean))

# write.csv(uNB, "data/uNB.csv")
uNB <- read.csv("data/uNB.csv")
#   --- Find center of view
# center_ny.map <- watershedDF %>%
#   dplyr::select(long, lat) %>%
#   summarise(ctrlong = mean(long), ctrlat = mean(lat))
# center_ny.map # -73.92194 40.68922
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
write.csv(ny.map_attr@data, "data/ny_map_attr_data.csv")
# ny.map_attr@data <- read.csv("data/ny_map_attr_data.csv")
if (debugMode) cat("*************** 3 ***************")
# --- For Boro
data_necessary_Boro <- cbind(data_necessary, borocodenum = data$borocodenum)
tbl_df(data_necessary_Boro)
data_necessary_Boro[is.na(data_necessary_Boro)] <- 0
uBR <- data_necessary_Boro %>%
  group_by(borocodenum) %>%
  summarise_all(funs(mean))
# #   --- Combine shapefile with data
boros_attr <- boros
boros_attr@data <- dplyr::left_join(boros_attr@data, uBR, by = c("BoroCode" = "borocodenum"))

write.csv(boros_attr@data, "data/boros_attr_data.csv")
# boros_attr@data <- read.csv("data/boros_attr_data.csv")

# --- For Precinct


#   --- Combine shapefile with data
nypp_attr <- nypp
uPP <- data_precinct %>%
  group_by(precinct) %>%
  summarise_all(funs(mean))
nypp_attr@data <- dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct"))
write.csv(dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct")), "data/nypp_attr_data.csv")
# nypp_attr@data <- read.csv("data/nypp_attr_data.csv")
CTname <- dfCT %>% dplyr::select(BoroCT2000, NTANAme)
data_precinct2$BoroCT2000 <- as.character(data_precinct2$BoroCT2000)
CTname$BoroCT2000 <- as.character(CTname$BoroCT2000)
Precinct_CTntaname <- full_join(data_precinct2, CTname, by = "BoroCT2000")

Precinct_CTntaname <- Precinct_CTntaname %>%
  distinct(precinct, NTANAme) %>%
  arrange(precinct)
write.csv(Precinct_CTntaname, "data/Precinct_CTntaname.csv")
# Precinct_CTntaname <- read.csv("data/Precinct_CTntaname.csv")
Precinct_CTntaname_aggr <- aggregate(NTANAme ~ precinct, data = Precinct_CTntaname, FUN = paste0, collapse = "<br/>")
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

# For all kinds of buildings, refer to https://stackoverflow.com/questions/46286599/custom-markers-on-shiny-leaflet-map
# I1icon <- rsvg("data/icons/I1.svg")
# dim(I1icon)
# rsvg_png("data/icons/I1.svg", "data/icons/I1.png")


# ---- For NPI data ----
# NPI_majorOrganization <- NPIData %>%
#   group_by(organization_name.legal_business_name.) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count))
# NPI_majorOrganization <- NPI_majorOrganization[-1, ]
# colnames(NPI_majorOrganization) <- c("organization", "count")
# head(NPI_majorOrganization, 50)
# 
# 
# organization_fullName_original <- as.character(NPI_majorOrganization$organization)
# organization_fullName <- as.character(NPI_majorOrganization$organization)
# write.csv(organization_fullName, "data/organization_fullName.csv")
organization_fullName <- read.csv("data/organization_fullName.csv")
# 
# nameNum <- length(organization_fullName)
# ttt <- NPI_majorOrganization %>% filter(count >= 2)
# nameNum <- nrow(ttt)
# # nameNum <- 30
# orgNameMatching <- sapply(organization_fullName[1:nameNum], function(x) {
#   agrep(x, organization_fullName[1:nameNum], max.distance = 0.05, value=T)
# })
# orgNameMatchingLen <- sapply(orgNameMatching, length)
# orgNameMatchingInfo <- data.frame(id = 1:length(orgNameMatchingLen), orgNameMatchingLen = orgNameMatchingLen)
# orgNameMatchingInfo$orgName <- rownames(orgNameMatchingInfo)
# rownames(orgNameMatchingInfo) <- orgNameMatchingInfo$id
# orgNameMatchingInfo <- orgNameMatchingInfo %>% filter(orgNameMatchingLen > 1)
# orgNameCombIdx <- orgNameMatchingInfo$id
# orgNameMatchingMto1 <- orgNameMatching[orgNameCombIdx]
# 
# for (i in 1:length(orgNameMatchingMto1)) {
#   for (j in orgNameMatchingMto1[[i]]) {
#     organization_fullName[which(organization_fullName == j)] <- orgNameMatchingMto1[[i]][1]
#   }
# }
# 
# orgNameCompare <- data.frame(before = organization_fullName_original, after = organization_fullName)
# head(orgNameCompare, 20)
# 
# write.csv(orgNameCompare, "data/orgNameCompare.csv")

# --- --- Fuzzy name matching ----
# orgNameCompare <- read.csv("data/orgNameCompare.csv")
# NPI_organization <- NPIData %>% select(ctuniq, organization_name.legal_business_name., interpolated_latitude, interpolated_longitude)
# head(NPI_organization)
# colnames(NPI_organization) <- c("ctuniq", "organization", "lat", "long")
organization_fullName_20 <- as.character(organization_fullName[1:20, 2])
# head(NPI_organization)
# 
# weillIdx <- which(NPI_organization$organization == "WEILL MEDICAL COLLEGE OF CORNELL UNIVERSITY")
# 
# NPI_organization <- left_join(NPI_organization, orgNameCompare, by = c("organization" = "before"))
# NPI_organization$organization <- NPI_organization$after
# NPI_organization <- NPI_organization[, -ncol(NPI_organization)]
# 
# write.csv(NPI_organization, "data/NPI_organization.csv")
NPI_organization <- read.csv("data/NPI_organization.csv")
# NPI_organization[weillIdx,]

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

# [Necessary Datasets] ----
# uCT
# uNB
# ct2000shp_attr
# ny.map_attr
# varDef
# pluto_bldCT
