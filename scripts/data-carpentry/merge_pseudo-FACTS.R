library(rgdal)
library(sf)
library(raster)







sn <- readOGR(dsn = "features/SierraEcoregion_TNC", layer="SierraEcoregion_TNC", stringsAsFactors = FALSE)

facts.fueltrt <- readOGR(dsn = "features/FACTS/CA clips",layer="CA_Activity_HazFuelTrt", stringsAsFactors = FALSE)
facts.reforest <- readOGR(dsn = "features/FACTS/CA clips",layer="CA_Activity_SilvReforestation", stringsAsFactors = FALSE)
facts.tsi <- readOGR(dsn = "features/FACTS/CA clips",layer="CA_Activity_SilvTSI", stringsAsFactors = FALSE)
facts.harvest <- readOGR(dsn = "features/FACTS/CA clips",layer="CA_Activity_TimberHarvest", stringsAsFactors = FALSE)


names(facts.reforest)[names(facts.reforest) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.reforest)[names(facts.reforest) == "METHOD_DES"] <- "METHOD"

names(facts.tsi)[names(facts.tsi) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.tsi)[names(facts.tsi) == "METHOD_DES"] <- "METHOD"

names(facts.harvest)[names(facts.harvest) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.harvest)[names(facts.harvest) == "METHOD_DES"] <- "METHOD"


keep.cols <- c("SUID","ACTIVITY","DATE_ACCOM","DATE_COMPL","TREATMENT_","METHOD")

facts.fueltrt <- facts.fueltrt[,keep.cols]
facts.reforest <- facts.reforest[,keep.cols]
facts.tsi <- facts.tsi[,keep.cols]
facts.harvest <- facts.harvest[,keep.cols]

facts.reforest <- spChFIDs(facts.reforest, paste("b", row.names(facts.reforest), sep="."))
facts.tsi <- spChFIDs(facts.tsi, paste("c", row.names(facts.tsi), sep="."))
facts.harvest <- spChFIDs(facts.harvest, paste("d", row.names(facts.harvest), sep="."))


#merge them
facts <- rbind(facts.fueltrt,facts.reforest,facts.tsi,facts.harvest)

writeOGR(facts,dsn = "features/FACTS/CA clips",layer="CA_Activity_merged",driver="ESRI Shapefile")

