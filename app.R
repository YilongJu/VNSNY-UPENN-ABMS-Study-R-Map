# https://vnsnyupenn.shinyapps.io/vnsny-upenn-abms-study-r-map/
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
debugMode <- F

data <- fread("data/ABM_censustract_precinct_111617.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
# data_precinct <- data %>% dplyr::select(precpop:offpcap)
# data_precinct2 <- data %>% dplyr::select(BoroCT2000, precpop:offpcap)

varDef <- fread("data/Variable_Definitions.csv")

ct2000shp <- shapefile("data/nyct2000_12c/nyct2000_12c/nyct2000")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp")
nypp <- readOGR("data/nypp_17c_police_precinct_shapefile/nypp.shp")

# NPIData <- read.csv("NPI_ctuniq.csv", row.names = 1)

CMS_patient <- fread("data/CMS_patient_data2.csv")
CMS_patient <- CMS_patient[, -1]
dim(CMS_patient)
head(CMS_patient)
sum(CMS_patient$num_patients == CMS_patient$num_pat_08, na.rm = T)
CMS_patient[CMS_patient == -100] <- NA
# CMS_patient <- CMS_patient %>% mutate(
#   avg_er_charges_08 = er_charges_08 / num_er_08,
#   avg_er_charges_09 = er_charges_09 / num_er_09,
#   avg_er_charges_10 = er_charges_10 / num_er_10,
#   avg_er_charges_tot = tot_er_charges / tot_er_visits,
#   avg_charges_08 = charges_08 / num_pat_08,
#   avg_charges_09 = charges_09 / num_pat_09,
#   avg_charges_10 = charges_10 / num_pat_10,
#   avg_charges_tot = tot_charges / tot_visits,
#   num_high_utils_08,
#   num_high_utils_09,
#   num_high_utils_10
# )
CMS_patient <- CMS_patient %>% mutate(
  avg_er_charges_tot = tot_er_charges / tot_er_visits,
  avg_charges_tot = tot_charges / tot_visits
) %>% dplyr::select(CT2000_unique, avg_er_charges_tot, avg_charges_tot)
# head(CMS_patient)
# CMS_patient <- CMS_patient[, c(1, 2, 3, 4, 22, 11, 12, 26, 19, 5, 6, 23, 13, 14, 27, 20, 7, 8, 24, 15, 16, 28, 21, 9, 10, 25, 17, 18, 29)]
# head(CMS_patient)
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
data <- left_join(data, dataName, by = "ctuniq")

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
# pluto_bldCT_coords <- pluto_bldCT %>% dplyr::select(id, xcoord, ycoord)
# pluto_bldCT_coords <- pluto_bldCT_coords[complete.cases(pluto_bldCT_coords), ]
# coordinates(pluto_bldCT_coords) <- ~xcoord + ycoord
# proj4string(pluto_bldCT_coords) <- ct2000shp@proj4string
# pluto_bldCT_coords <- spTransform(pluto_bldCT_coords, ny.map@proj4string)
# pluto_bldCT_coords <- data.frame(id = pluto_bldCT_coords$id,
#                                  x = pluto_bldCT_coords@coords[, 1],
#                                  y = pluto_bldCT_coords@coords[, 2])
# pluto_bldCT <- left_join(pluto_bldCT, pluto_bldCT_coords, by = "id") %>% dplyr::select(-xcoord, -ycoord)
# write.csv(pluto_bldCT[, -1], "data/pluto_bldCT.csv")
pluto_bldCT <- read.csv("data/pluto_bldCT.csv")

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
shapeDataList <- shapeDataList[1]
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
# f_nypp <- fortify(nypp, polyname = "Precinct")
# nypp_DF <- merge(f_nypp, nypp@data, by = "id")
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
# f_ct2000shp <- fortify(ct2000shp, polyname = "BoroCT2000")
# ct2000shp_DF <- merge(f_ct2000shp, ct2000shp@data, by = "id")

# [Prepare ny neighborhood shape data, <by Brooke>] ----
#   project the dataframe onto the shape file
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
# sodo <- ny.map[ny.map$City == "New York", ]
# dat <- data.frame(Longitude = data$ctrdlong, Latitude = data$ctrdlat)
# coordinates(dat) <- ~ Longitude + Latitude
# proj4string(dat) <- proj4string(sodo)
# location <- over(dat, sodo)
# data <- cbind(data, location)
# write.csv(data, "data/data.csv")
# data <- read.csv("data/data.csv")
# dataProjected <- sodo
# dataProjected@data$id <- rownames(dataProjected@data)
# watershedPoints <- fortify(dataProjected, region = "id")
# watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

# [Prepare useful data] ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ==== Add new variable into CT layer:
#       1) Add variable description in "Varialbe_Definitions.csv" !!csv file!!
#       2) Add a correspoding column to "data_vars", whose name should be consistent with the varName in "Varialbe_Definitions.csv"
#       3) Apply the same change in "UpdateData.R" and run it.
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_ids <- data %>% dplyr::select(BoroCT2000, Name)
data_vars <- data %>% dplyr::select(popdens:propnonw, offpcap, avg_er_charges_tot:avgChronCond)
data_coords <- data[, c("ctrdlong", "ctrdlat")]
data_necessary <- cbind(data_ids, data_vars, data_coords)
# --- For CT
# uCT <- data_necessary %>%
#   group_by(BoroCT2000) %>%
#   summarise_all(funs(mean))
# write.csv(uCT, "data/uCT.csv")
uCT <- read.csv("data/uCT.csv")
# ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
# uCT$BoroCT2000 <- as.character(uCT$BoroCT2000)
# dfCT <- dplyr::left_join(ct2000shp_DF, uCT, by = "BoroCT2000")
# #   --- Find center of view
# center_ct.map <- ct2000shp_DF %>%
#   dplyr::select(long, lat) %>%
#   summarise(ctrlong = mean(long), ctrlat = mean(lat))
# center_ct.map # -73.91271 40.69984
# #   --- Combine shapefile with data
ct2000shp_attr <- ct2000shp
# ct2000shp_attr@data <- dplyr::left_join(ct2000shp_attr@data, uCT, by = "BoroCT2000")
# write.csv(ct2000shp_attr@data, "data/ct2000shp_attr_data.csv")
ct2000shp_attr@data <- read.csv("data/ct2000shp_attr_data.csv")


# --- For NB
# uNB <- data_necessary %>%
#   group_by(Name) %>%
#   summarise_all(funs(mean))

# write.csv(uNB, "data/uNB.csv")
uNB <- read.csv("data/uNB.csv")
#   --- Find center of view
# center_ny.map <- watershedDF %>%
#   dplyr::select(long, lat) %>%
#   summarise(ctrlong = mean(long), ctrlat = mean(lat))
# center_ny.map # -73.92194 40.68922
#   --- Combine shapefile with data
ny.map_attr <- ny.map
# ny.map_attr@data <- dplyr::left_join(ny.map_attr@data, uNB, by = "Name")
# 
# data_necessary$BoroCT2000 <- as.character(data_necessary$BoroCT2000)
# NBname_CTntaname <- full_join(data_necessary, ct2000shp_DF, by = "BoroCT2000")
# NBname_CTntaname <- NBname_CTntaname %>%
#   distinct(Name, NTANAme) %>%
#   arrange(Name)
# NBname_CTntaname <- aggregate(NTANAme ~ Name, data = NBname_CTntaname,
#                               FUN = paste0, collapse = "<br/>")
# 
# ny.map_attr@data <- left_join(ny.map_attr@data, NBname_CTntaname, by = "Name")
# write.csv(ny.map_attr@data, "data/ny_map_attr_data.csv")
ny.map_attr@data <- read.csv("data/ny_map_attr_data.csv")
if (debugMode) cat("*************** 3 ***************")
# --- For Boro
# data_necessary_Boro <- cbind(data_necessary, borocodenum = data$borocodenum)
# tbl_df(data_necessary_Boro)
# data_necessary_Boro[is.na(data_necessary_Boro)] <- 0
# uBR <- data_necessary_Boro %>%
#   group_by(borocodenum) %>%
#   summarise_all(funs(mean))
# #   --- Combine shapefile with data
boros_attr <- boros
# boros_attr@data <- dplyr::left_join(boros_attr@data, uBR, by = c("BoroCode" = "borocodenum"))

# write.csv(boros_attr@data, "data/boros_attr_data.csv")
boros_attr@data <- read.csv("data/boros_attr_data.csv")

# --- For Precinct


#   --- Combine shapefile with data
nypp_attr <- nypp
# uPP <- data_precinct %>%
#   group_by(precinct) %>%
#   summarise_all(funs(mean))
# nypp_attr@data <- dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct"))
# write.csv(dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct")), "data/nypp_attr_data.csv")
nypp_attr@data <- read.csv("data/nypp_attr_data.csv")
# CTname <- dfCT %>% dplyr::select(BoroCT2000, NTANAme)
# data_precinct2$BoroCT2000 <- as.character(data_precinct2$BoroCT2000)
# CTname$BoroCT2000 <- as.character(CTname$BoroCT2000)
# Precinct_CTntaname <- full_join(data_precinct2, CTname, by = "BoroCT2000")
# 
# Precinct_CTntaname <- Precinct_CTntaname %>%
#   distinct(precinct, NTANAme) %>%
#   arrange(precinct)
# write.csv(Precinct_CTntaname, "data/Precinct_CTntaname.csv")
Precinct_CTntaname <- read.csv("data/Precinct_CTntaname.csv")
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
# organization_fullName <- read.csv("data/organization_fullName.csv")
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
# orgNameCompare <- read.csv("data/orgNameCompare.csv")

# --- --- Fuzzy name matching ----
# orgNameCompare <- read.csv("data/orgNameCompare.csv")

# head(NPI_organization)
# NPI_majorOrganization <- NPIData %>%
#     group_by(organization_name.legal_business_name.) %>%
#     summarise(count = n()) %>%
#     arrange(desc(count))
# head(NPI_majorOrganization, 20)
# head(NPIData, 20)
# NPIData %>% filter(organization_name.legal_business_name. == "NEW YORK UNIVERSITY")
# NPIData$full_name <- paste(NPIData$first_name, NPIData$middle_name, NPIData$last_name.legal_name., sep = " ")
# head(NPIData$full_name, 20)
# NPI_organization <- NPIData %>% 
#   dplyr::select(ctuniq, NPI, full_name, credential, street_addr = business_mailing_street, zip = business_mailing_zipcode, organization = organization_name.legal_business_name., lat = interpolated_latitude, long = interpolated_longitude)
# head(NPI_organization)
# NPI_organization %>% filter(organization == "NEW YORK UNIVERSITY")
# 
# organization_fullName_20 <- as.character(organization_fullName[1:20, 2])
# # head(NPI_organization)
# # 
# # weillIdx <- which(NPI_organization$organization == "WEILL MEDICAL COLLEGE OF CORNELL UNIVERSITY")
# # 
# NPI_organization <- left_join(NPI_organization, orgNameCompare, by = c("organization" = "before"))
# NPI_organization %>% filter(organization == "NEW YORK UNIVERSITY")
# head(NPI_organization, 20)
# NPI_organization$organization <- as.character(NPI_organization$organization)
# NPI_organization$after <- as.character(NPI_organization$after)
# NPI_organization$organization[!is.na(NPI_organization$after)] <- NPI_organization$after[!is.na(NPI_organization$after)]
# NPI_organization <- NPI_organization[, -ncol(NPI_organization)]
# NPI_organization <- NPI_organization[, -ncol(NPI_organization)]
# # 
# write.csv(NPI_organization, "data/NPI_organization.csv")
NPI_organization <- read.csv("data/NPI_organization.csv")
NPI_majorOrganization <- NPI_organization %>%
    group_by(organization) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
# head(NPI_majorOrganization)
organization_fullName_20 <- as.character(NPI_majorOrganization$organization[2:21])
# head(NPI_organization)

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


# *********************************************
# [[[[Necessary Datasets]]]] ----
# uCT
# uNB
# ct2000shp_attr
# ny.map_attr
# varDef
# pluto_bldCT
# *********************************************

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
ui <- bootstrapPage(
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js")
  ),
  
  leafletOutput(outputId = "outputMap", width = "99%", height = 2000),
  
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = T, draggable = T, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
    h2("Options"),
    selectInput("ChooseTileVar",
                label = h4("Select which variable to show on the tile"),
                unlist(checkboxGroupList)),
    radioButtons(inputId = "ChooseShapefileID",
                 label = h4("Select which level of map to show"),
                 choices = shapeDataList),
    selectInput("ChooseLabelVars",
                label = h4("Select which variable definition to show"),
                unlist(checkboxGroupList),
                multiple = T),
    uiOutput("varDefOutput"),
    # verbatimTextOutput("displaySomething"),
    # verbatimTextOutput("displaySomething2"),
    h4("Click a CT once to add it to the barplot, twice to remove"),
    actionButton("clearDataPoints", "Clear all points"),
    selectInput("ChooseCircleVars",
                label = h4("Select which variable to show as circles (also in the barplot)"),
                unlist(checkboxGroupList),
                multiple = T),
    plotOutput("varBarPlot", height = 200),
    actionButton("viewDataPlot", "View plot in new window", class="btn-block"),
    actionButton("viewDocumentation", "See how to use this app", class="btn-block")
    # ,
    # conditionalPanel("input.viewDataPlot % 2 == 1",
    #    # Only prompt for threshold when coloring or sizing by superzip
    #    verbatimTextOutput("displaySomething")
    # )
  ),
  bsModal(id = "plotWindow",
          title = "Plot of selected variables",
          trigger = "viewDataPlot",
          size = "large",
          plotOutput("varBarPlotLarge"),
          downloadButton(outputId = "download", label = "Download the plot"),
          # verbatimTextOutput("varBarPlotLarge"),
          tags$head(tags$style("#id1 .modal-footer{ display:none}"))
  ),
  bsModal(id = "documentationWindow",
          title = "How to use this app",
          trigger = "viewDocumentation",
          size = "large",
          # htmlOutput("documentation"),
          includeMarkdown("README.md"),
          tags$head(tags$style("#id1 .modal-footer{ display:none}"))
  )
)

# ui <- navbarPage(title = "VNSNY/UPENN ABMS Study", ----
#                  
#                  
#    tabPanel(
#      title = "New ABM Census",
#      div(class = "outer",
#          
#          tags$head(
#            # Include our custom CSS
#            includeCSS("styles.css"),
#            includeScript("gomap.js")
#          ),
#          
#          leafletOutput(outputId = "outputMap", width = "99%", height = 2000),
#          
#          absolutePanel(id = "controls", class = "panel panel-default", fixed = T, draggable = T, top = 60, left = "auto", right = 20, bottom = "auto", width = 330, height = "auto",
#          h2("Options"),
#          selectInput("ChooseTileVar",
#                      label = h4("Select which variable to show on the tile"),
#                      unlist(checkboxGroupList)),
#          radioButtons(inputId = "ChooseShapefileID",
#                       label = h4("Select which level of map to show"),
#                       choices = shapeDataList),
#          selectInput("ChooseLabelVars",
#                      label = h4("Select which variable definition to show"),
#                      unlist(checkboxGroupList)),
#          # checkboxGroupInput(inputId = "ChooseLabelVars",
#          #              label = h4("Check variable definitions"),
#          #              choices = checkboxGroupList,
#          #              inline = FALSE),
#          uiOutput("varDefOutput"),
#          verbatimTextOutput("displaySomething"),
#          verbatimTextOutput("displaySomething2"),
#          actionButton("clearDataPoints", "Clear"),
#          
#          plotOutput("varBarPlot", height = 200),
#          actionButton("viewDataPlot", "View plot in new window", class="btn-block"),
#          selectInput("ChooseCircleVars",
#                      label = h4("Select which variable to show as circles"),
#                      unlist(checkboxGroupList),
#                      multiple = T)
#          # ,
#          # conditionalPanel("input.viewDataPlot % 2 == 1",
#          #    # Only prompt for threshold when coloring or sizing by superzip
#          #    verbatimTextOutput("displaySomething")
#          # )
#        )
#        ,
#        bsModal(id = "plotWindow",
#                title = "Plot of selected variables",
#                trigger = "viewDataPlot",
#                size = "large",
#                # plotOutput("varBarPlotLarge"),
#                verbatimTextOutput("varBarPlotLarge"),
#                tags$head(tags$style("#id1 .modal-footer{ display:none}"))
#        )
#      )
#    ),
#    
#   # tabPanel(
#   #  title = "ABM Census",
#   #  h3("Select a variable to show on the tile and check others on the map. (Wait for about 10s for plotting.)"),
#   #   # Sidebar layout with input and output definitions
#   #   sidebarLayout(
#   #     fluidRow(
#   #       column(
#   #         radioButtons(inputId = "ChooseShapefileID",
#   #                     label = h4("Select which map to show"),
#   #                     choices = shapeDataList),
#   #         selectInput("ChooseTileVar",
#   #                     "Variable to show on the tile:",
#   #                     unlist(checkboxGroupList)),
#   #         verbatimTextOutput("displaySomething"),
#   #         # radioButtons(inputId = "ChooseTileVarID2",
#   #         #              label = h4("Select which variable to show on tile"),
#   #         #              choices = checkboxGroupList),
#   #         # actionButton("Plot", "Draw plot"),
#   #         
#   #         # fluidRow("Click the button to plot."),
#   #         width = 3,
#   #         offset = 1
#   #       ),
#   #       column(
#   #         checkboxGroupInput(inputId = "ChooseLabelVars",
#   #                      label = h4("Check variable definitions"),
#   #                      choices = checkboxGroupList,
#   #                      inline = FALSE),
#   #         width = 3
#   #       ),
#   #       column(
#   #         verbatimTextOutput("displaySomething2"),
#   #         uiOutput("varDefOutput"),
#   #         width = 5
#   #       )
#   #     ),
#   #     fluidRow(
#   #       column(
#   #         leafletOutput(outputId = "outputMap", width = "99%", height = 2000),
#   #         width = 11,
#   #         offset = 1
#   #       )
#   #     )
#   #   )
#   # ),
#   
#   # tabPanel("Reserved Slot", h3("This is the second panel"),
#   #   # Sidebar layout with input and output definitions
#   #   sidebarLayout(
#   #     # Sidebar panel for inputs
#   #     sidebarPanel(
#   #     # Input: Slider for the number of bins
#   #     sliderInput(
#   #       inputId = "bins",
#   #       label = "Number of bins:",
#   #       min = 1,
#   #       max = 50,
#   #       value = 30)
#   #     ),
#   #     # Main panel for displaying outputs
#   #     mainPanel(
#   #       # Output: Histogram
#   #       plotOutput(outputId = "distPlot")
#   #     )
#   #   )
#   # )
# )

if (debugMode) cat("==================")
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
      return(c(1,40))
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
    # label <- "<h4>Variable Definitions</h4>"
    label <- ""
    for (var in input$ChooseLabelVars) {
      varId <- checkboxGroupListIndex[[var]]
      # varSN <- varShortNames[varId]
      varD <- varDefinitions[varId]
      # label <- paste0(label, "<h5>", varSN, ":</h5>", varD, "<br/>")
      label <- paste0(label, varD, "<br/>")
    }
    HTML(label)
  })
  
  # create a reactive value that will store the click position
  dataClicked <- reactiveValues(clickedMarker = NULL)
  areaClickedID <- reactiveValues(ID = NULL, lat = NULL, lng = NULL)
  
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
    
    if (debugMode) cat("------------ 0.9 ------------")
    
    i <- 0
    for (orgName in organization_fullName_20) {
      i <- i + 1
      NPI_organization_tmp <- NPI_organization %>%
        filter(organization == orgName)
      
      # signif(varValues, 4)
      labels <- sprintf("<b>NPI: </b>%d, <b>Name: </b>%s<br/>
                        <b>Organization: </b>%s<br/>
                        <b>CT: </b>%s<br/>
                        <b>Credential: </b>%s<br/>
                        <b>Address: </b>%s, zip: %s<br/>",
                        NPI_organization_tmp$NPI, NPI_organization_tmp$full_name,
                        NPI_organization_tmp$organization,
                        NPI_organization_tmp$ctuniq,
                        NPI_organization_tmp$credential,
                        NPI_organization_tmp$street_addr, NPI_organization_tmp$zip)
      
      labels <- lapply(labels, HTML)
      
      Lmap <- Lmap %>%
        # addCircles(data = NPI_organization_tmp,
        #   lng = ~lat, lat = ~long,
        #   weight = 20,
        #   fill = T,
        #   popup = labels,
        #   color = varColors[i],
        #   stroke = T, fillOpacity = 0.6, opacity = 0.6,
        #   group = orgName
        # ) %>%
        addMarkers(data = NPI_organization_tmp,
                   lng = ~lat, lat = ~long,
                   popup = labels,
                   group = orgName
        ) %>%
        addLegend(
          colors = varColors[i],
          labels = paste0("<b>", orgName, "</b>"),
          opacity = 0.7,
          title = NULL,
          position = "bottomright",
          group = orgName
        ) %>%
        hideGroup(orgName)
    }
    # NPI_organization
    # organization_fullName_20

    if (debugMode) cat("------------ 1 ------------")
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
  if (debugMode) cat("------------ 1.5 ------------")
  # Observe command from map control, hide / show layers
  observe({
    # Initialize map components
    if (debugMode) cat("------------ 1.51 ------------")
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
    if (debugMode) cat(tileVar, '\n')
    if (debugMode) cat(labelVars, '\n\n')
    if (debugMode) cat(varShortNames, '\n')
    if (debugMode) cat(colnames(mapData), '\n')
    
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
    if (debugMode) cat("------------ 1.55 ------------", '\n')
    if (debugMode) print(head(mapData))
    
    for (labelVar in labelVars) {
      # labelVar <- labelVars[1]
      labelVarIdx <- checkboxGroupListIndex[[labelVar]]
      
      if (debugMode) cat(labelVar, '\n')
      if (debugMode) cat(labelVarIdx, '\n')
      if (debugMode) cat(varShortNames[labelVarIdx], '\n')
      if (debugMode) cat("------------ 1.55a ------------", '\n')
      if(showPercentage[labelVarIdx] == 1) {
        if (debugMode) cat("------------ 1.55b ------------", '\n')
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(100*mapData[, labelVar], 4), "%<br/>")
      } else {
        if (debugMode) cat("------------ 1.55c ------------", '\n')
        if (debugMode) cat(mapData[1, labelVar], '\n')
        if (debugMode) cat(varShortNames[labelVarIdx], '\n')
        if (debugMode) cat("------------ 1.55c2 ------------", '\n')
        print(labelVar)
        print(labelVarIdx)
        print(varShortNames[labelVarIdx])
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(mapData[, labelVar], 4), "<br/>")
        # avg_er_charges_08
        if (debugMode) cat("------------ 1.55c3 ------------", '\n')
      }
      if (debugMode) cat("------------ 1.55d ------------", '\n')
    }
    if (debugMode) cat("------------ 1.6 ------------", '\n')
    if (CTNames) {
      labels <- paste0(labels, "<strong>CTs:</strong><br/>", mapData$NTANAme)
    }
    labels <- paste0(labels, "<br/><br/><br/>")
    labels <- lapply(labels, HTML)
    colfunc <- colorRampPalette(c("white", varColors[tileVarIdx]))
    
    # Special treatment for offpcap
    if (tileVar == 'offpcap') {
      varValues[which(varValues > 0.7278)] <- max(varValues[which(varValues <= 0.7278)], na.rm = T)
    }
    if (debugMode) cat("------------ 1.7 ------------")
    
    if (abs(skewness(as.numeric(as.character(varValues)), na.rm = T)) > 1 | tileVar == "subacc") {
      pal <- colorBin(colfunc(6), domain = varValues, n = 6)
    } else {
      pal <- colorQuantile(colfunc(6), domain = varValues, n = 6)
    }
    
    # Create a proxy for Leaflet map, saving render time
    proxy <- leafletProxy("outputMap", data = map)
    if (debugMode) cat("------------ 2 ------------")
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
        popup = labels,
        popupOptions = popupOptions(
          closeOnClick = T,
          autoPan = T,
          keepInView = F,
          closeButton = T,
          className = "POP"),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "25px",
          direction = "auto"),
        # popup = tileVar,
        group = "tileLayer",
        data = map,
        layerId = ~X
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
      
    if (debugMode) cat("------------ 3 ------------")
      # Add map control
      proxy <- proxy %>%
        addLayersControl(
          baseGroups = c("Grey map", "Standard map", "Dark map"),
          # overlayGroups = c("==== NPI Provider by Organizations ====",
          #                   organization_fullName_20,
          #                   "============== Buildings ==============",
          #                   uniqueBuildingLabel,
          #                   "============== Variables ==============",
          #                   varShortNames),
          # overlayGroups = c(organization_fullName_20,
          #                   uniqueBuildingLabel,
          #                   varShortNames),
          overlayGroups = c(organization_fullName_20,
                            uniqueBuildingLabel),
          position = "topleft",
          options = layersControlOptions(autoZIndex = F, collapsed = F)
        )
      
      for (groupName in varShortNames) {
        proxy <- proxy %>%
          showGroup(groupName) %>%
          hideGroup(groupName)
      }
      for (orgName in organization_fullName_20) {
        proxy <- proxy %>%
          showGroup(orgName) %>%
          hideGroup(orgName)
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
  # ---- Code for barplot
  # dt <- data.frame(ID = c(1,2), var1 = c(30, 40), var2 = c(50, 60))
  # dt_melt <- melt(dt, id = "ID")
  # ggplot(dt_melt, aes(x =ID, y = value, fill = variable)) +
  #   geom_bar(stat="identity", position = position_dodge())
  
  # [Debugging] ----
  observeEvent(input$outputMap_shape_click, {
    event <- input$outputMap_shape_click
    if (is.null(event)) {
      return()
    }
    # areaClickedID <- NULL
    remove <- NULL
    if (event$id %in% areaClickedID$ID) {
      remove <- event$id
      areaClickedID$ID <- areaClickedID$ID[!areaClickedID$ID %in% remove]
      areaClickedID$lat <- areaClickedID$lat[!areaClickedID$ID %in% remove]
      areaClickedID$lng <- areaClickedID$lng[!areaClickedID$ID %in% remove]
      remove <- NULL
      if (length(areaClickedID$ID) == 0) {
        areaClickedID$ID <- NULL
        areaClickedID$lat <- NULL
        areaClickedID$lng <- NULL
      }
    } else {
      areaClickedID$ID <- c(areaClickedID$ID, event$id)
      areaClickedID$lat <- c(areaClickedID$lat, event$id)  
      areaClickedID$lng <- c(areaClickedID$lng, event$id)  
    }
    
    print(areaClickedID$ID)
    map <- map_r()
    
    data4Plot <- map@data[event$id, c(input$ChooseTileVar, input$ChooseCircleVars)]
    data4Plot <- data.frame(ID = paste0("CT", map@data$BoroCT2000[event$id], ": " , map@data$NTANAme[event$id]), data4Plot)
    colnames(data4Plot) <- c("ID", input$ChooseTileVar, input$ChooseCircleVars)
    map@data <- data4Plot
    
    proxy <- leafletProxy("outputMap")
    proxy %>%
      addPopups(event$lng,
                event$lat,
                data4Plot$ID,
                layerId = event$id)
      # clearGroup(group = "highlightTileLayer") %>%
      # # Add tile layer
      # addPolygons(
      #   fillColor = "white",
      #   weight = 1,
      #   opacity = 0.5,
      #   color = "white",
      #   dashArray = "3",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 3,
      #     color = "#666",
      #     dashArray = "",
      #     fillOpacity = 0.7,
      #     bringToFront = T),
      #   label = ~ID,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal",
      #                  padding = "3px 8px"),
      #     textsize = "25px",
      #     direction = "auto"),
      #   # popup = tileVar,
      #   group = "highlightTileLayer",
      #   data = map
      # )
  })
  
  observeEvent(input$ChooseCircleVars, {
    # map <- map_r()@data
    proxy <- leafletProxy("outputMap")
    for (var in varNames) {
      varIdx <- checkboxGroupListIndex[[var]]
      proxy %>% hideGroup(varShortNames[varIdx])
    }
    
    for (var in input$ChooseCircleVars) {
      varIdx <- checkboxGroupListIndex[[var]]
      proxy %>% showGroup(varShortNames[varIdx])
    }
  })
  
  output$displaySomething <- renderPrint({
    mapData <- map_r()@data
    mapData[areaClickedID$ID, input$ChooseTileVar]
    # mapData[areaClickedID$ID, 1:5]
    input$ChooseCircleVars
    mapData[areaClickedID$ID, c("BoroCT2000", "NTANAme", input$ChooseTileVar, input$ChooseCircleVars)]
  })
  output$documentation <- renderPrint({
    areaClickedID$ID
  })
  
  observeEvent(input$clearDataPoints, {
    areaClickedID$ID <- NULL
  })
  
  output$varBarPlot <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (length(areaClickedID$ID) == 0) {
      return(NULL)
    }
    mapData <- map_r()@data
    
    data4Plot_origin <- mapData[areaClickedID$ID, c(input$ChooseTileVar, input$ChooseCircleVars)]
    data4Plot_origin <- data.frame(ID = paste0("CT", mapData$BoroCT2000[areaClickedID$ID], ": " , mapData$NTANAme[areaClickedID$ID]), data4Plot_origin)
    colnames(data4Plot_origin) <- c("ID", input$ChooseTileVar, input$ChooseCircleVars)
    data4Plot_origin_melt <- melt(data4Plot_origin, id = "ID")
    
    for (j in c(input$ChooseTileVar, input$ChooseCircleVars)) {
      values <- mapData[, j]
      values <- values - min(values, na.rm = T)
      values <- values / max(values, na.rm = T)
      mapData[, j] <- values
    }
    
    data4Plot <- mapData[areaClickedID$ID, c(input$ChooseTileVar, input$ChooseCircleVars)]
    data4Plot <- data.frame(ID = paste0("CT", mapData$BoroCT2000[areaClickedID$ID], ": " , mapData$NTANAme[areaClickedID$ID]), data4Plot)
    colnames(data4Plot) <- c("ID", input$ChooseTileVar, input$ChooseCircleVars)
    print(data4Plot)
    
    data4Plot_melt <- melt(data4Plot, id = "ID")
    data4Plot_melt <- data.frame(data4Plot_melt, value_origin = signif(data4Plot_origin_melt$value, 5))
    print(data4Plot_melt)
    
    tileVarIdx <- checkboxGroupListIndex[[input$ChooseTileVar]]
    tileVarShortName <- varShortNames[tileVarIdx]
    circleVarShortName <- c(NULL)
    for (circleVar in input$ChooseCircleVars) {
      circleVarIdx <- checkboxGroupListIndex[[circleVar]]
      circleVarShortName <- c(circleVarShortName, varShortNames[circleVarIdx])
    }
    legendLabels <- c(tileVarShortName, circleVarShortName)
    print(legendLabels)
    
    ggplot(data4Plot_melt, aes(x = ID, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = value_origin),
                position = position_dodge(width = 1),
                # vjust = 1.6,
                vjust = -0.3,
                color = "black",
                size = 3.5) +
      scale_fill_discrete(name = "Variables",
                          breaks = c(input$ChooseTileVar, input$ChooseCircleVars),
                          labels = legendLabels) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  largePlot_r <- reactiveValues(plot = NULL)
  
  
  output$varBarPlotLarge <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (length(areaClickedID$ID) == 0) {
      return(NULL)
    }

    mapData <- map_r()@data
    
    data4Plot_origin <- mapData[areaClickedID$ID, c(input$ChooseTileVar, input$ChooseCircleVars)]
    data4Plot_origin <- data.frame(ID = paste0("CT", mapData$BoroCT2000[areaClickedID$ID], ": " , mapData$NTANAme[areaClickedID$ID]), data4Plot_origin)
    colnames(data4Plot_origin) <- c("ID", input$ChooseTileVar, input$ChooseCircleVars)
    data4Plot_origin_melt <- melt(data4Plot_origin, id = "ID")
    
    for (j in c(input$ChooseTileVar, input$ChooseCircleVars)) {
      values <- mapData[, j]
      values <- values - min(values, na.rm = T)
      values <- values / max(values, na.rm = T)
      mapData[, j] <- values
    }

    data4Plot <- mapData[areaClickedID$ID, c(input$ChooseTileVar, input$ChooseCircleVars)]
    data4Plot <- data.frame(ID = paste0("CT", mapData$BoroCT2000[areaClickedID$ID], ": " , mapData$NTANAme[areaClickedID$ID]), data4Plot)
    colnames(data4Plot) <- c("ID", input$ChooseTileVar, input$ChooseCircleVars)
    
    data4Plot_melt <- melt(data4Plot, id = "ID")
    data4Plot_melt <- data.frame(data4Plot_melt, value_origin = signif(data4Plot_origin_melt$value, 5))
    
    tileVarIdx <- checkboxGroupListIndex[[input$ChooseTileVar]]
    tileVarShortName <- varShortNames[tileVarIdx]
    circleVarShortName <- c(NULL)
    for (circleVar in input$ChooseCircleVars) {
      circleVarIdx <- checkboxGroupListIndex[[circleVar]]
      circleVarShortName <- c(circleVarShortName, varShortNames[circleVarIdx])
    }
    legendLabels <- c(tileVarShortName, circleVarShortName)
    
    
    largePlot_r$plot <- ggplot(data4Plot_melt, aes(x = ID, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = value_origin),
                position = position_dodge(width = 1),
                # vjust = 1.6,
                vjust = -0.3,
                color = "black",
                size = 3.5) +
      scale_fill_discrete(name = "Variables",
                          breaks = c(input$ChooseTileVar, input$ChooseCircleVars),
                          labels = legendLabels) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
    largePlot_r$plot
  })
  
  
  # output$varBarPlotLarge <- renderPrint({
  #   "XXXX"
  # })
  
  # downloadHandler contains 2 arguments as functions, namely filename, content
  output$download <- downloadHandler(
    
    filename = "new plot.png",
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      png(file) # open the png device
      print(largePlot_r$plot)
      dev.off()  # turn the device off
    } 
  )
  
}
# [Run server] ----
shinyApp(ui = ui, server = server)

