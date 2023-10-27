# Load data
load("data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
fg <- read.csv("data/fishglob_taxa_per_country.csv", sep=",", as.is=T)


# Read RAMLDB
stock_key <- stock  # 1374 stocks
area_key <- area 
all_stocks <- merge(stock_key, area_key, by="areaid", all.x=TRUE)
ramldb <- all_stocks[,c(2,6,4,5,7:9,13:17)]
table(ramldb$region)
table(ramldb$areaname)

# Read FishGlob data
colnames(fg)
dim(fg)  # 6297 11
fg$unique_key <- paste(fg$survey, fg$region, fg$accepted_name, sep="_")
length(unique(fg$unique_key))  # 6297

unique(fg$title)

fg$region <- NA
fg[!is.na(fg$title) & (fg$title=="Aleutian Islands" | fg$title=="Eastern Bering Sea"),]$region <- "US Alaska"
fg[!is.na(fg$title) & fg$title=="Baltic Sea",]$region <- "European Union"
fg[!is.na(fg$title) & fg$title=="France",]$region <- "European Union"
fg[!is.na(fg$title) & fg$title=="Gulf of Alaska",]$region <- "US Alaska"
fg[!is.na(fg$title) & fg$title=="Gulf of Saint Lawrence N",]$region <- "Canada East Coast"
fg[!is.na(fg$title) & fg$title=="Gulf of Saint Lawrence S",]$region <- "Canada East Coast"
fg[!is.na(fg$title) & fg$title=="Ireland",]$region <- "Europe non EU"
fg[!is.na(fg$title) & fg$title=="Northern Ireland",]$region <- "Europe EU"
fg[!is.na(fg$title) & fg$title=="North Sea",]$region <- "Europe non EU"
fg[!is.na(fg$title) & fg$title=="Portugal",]$region <- "Europe EU"
fg[!is.na(fg$title) & fg$title=="Canada - Hecate Strait",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Canada - Queen Charlotte Sound",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Canada - Strait of Georgia",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Canada - Haida Gwaii",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Canada - West Coast Vancouver Island",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Bay of Biscay",]$region <- "European Union"
fg[!is.na(fg$title) & fg$title=="Gulf of Mexico",]$region <- "US Southeast and Gulf"
fg[!is.na(fg$title) & fg$title=="West Coast Triennial US",]$region <- "US West Coast"
fg[!is.na(fg$title) & fg$title=="Norway",]$region <- "Europe non EU"
fg[!is.na(fg$title) & fg$title=="Northeast US",]$region <- "US East Coast"
fg[!is.na(fg$title) & fg$title=="North Sea - ROCKALL",]$region <- "Europe non EU"
fg[!is.na(fg$title) & fg$title=="Canada Maritimes",]$region <- "Canada East Coast"
fg[!is.na(fg$title) & fg$title=="Southeast US",]$region <- "US Southeast and Gulf"
fg[!is.na(fg$title) & fg$title=="Canada Maritimes",]$region <- "Canada West Coast"
fg[!is.na(fg$title) & fg$title=="Scotian Shelf",]$region <- "Canada East Coast"
fg[!is.na(fg$title) & fg$title=="Scotian Shelf",]$region <- "Canada East Coast"
fg[!is.na(fg$title) & fg$title=="Canada - West Coast",]$region <- "Canada West Coast"

fg$region <- as.factor(fg$region)

fg$key <-paste(fg$region, fg$accepted_name, sep="_")
cc <- as.data.frame(table(fg$key))
dim(cc)  # 4209
table(cc$Freq) # Attention on >=2

#############################################################################

# Pick common taxa between FishGlob and RAMLDB
common_species_ramldb <- ramldb[ramldb$scientificname %in% fg$accepted_name,]
dim(common_species_ramldb)  # 947 stocks under common taxa

fg_common_species <- fg[fg$accepted_name %in% unique(common_species_ramldb$scientificname),]
dim(fg_common_species)  # 1195 rows
cc2 <- as.data.frame(table(fg_common_species$key))
dim(cc2)  # 622
table(cc2$Freq) # Attention on >=2

fg_common_species$match_key <- paste(fg_common_species$region, fg_common_species$accepted_name, sep="_")
common_species_ramldb$match_key <- paste(common_species_ramldb$region, common_species_ramldb$scientificname, sep="_")

#############################################################################

# FishGlob match
fg_common_species$stockid_ramldb <- NA
for (i in 1:dim(fg_common_species)[1]){
  fg_common_species$stockid_ramldb[i] <- ifelse(length(common_species_ramldb[common_species_ramldb$match_key==fg_common_species$match_key[i],]$stockid)==0,
                                                NA, ifelse(length(common_species_ramldb[common_species_ramldb$match_key==fg_common_species$match_key[i],]$stockid)==1,
                                                           common_species_ramldb[common_species_ramldb$match_key==fg_common_species$match_key[i],]$stockid, "At_least_1_match"))
}
dim(fg_common_species[!is.na(fg_common_species$stockid_ramldb),])  # 461 rows with at-least_1_match with RAMLDB

fg_keep <- fg_common_species[!is.na(fg_common_species$stockid_ramldb),]

#############################################################################
# RAMLDB match
common_species_ramldb$fg_survey <- NA
for (i in 1:dim(common_species_ramldb)[1]){
  common_species_ramldb$fg_survey[i] <- ifelse(length(fg_common_species[fg_common_species$match_key==common_species_ramldb$match_key[i],]$unique_key)==0,
                                               NA, ifelse(length(fg_common_species[fg_common_species$match_key==common_species_ramldb$match_key[i],]$unique_key)==1,
                                                          fg_common_species[fg_common_species$match_key==common_species_ramldb$match_key[i],]$unique_key, "At_least_1survey_match"))
}

dim(common_species_ramldb[!is.na(common_species_ramldb$fg_survey),])  # 434 stocks with at-least_1survey_match with FishGlob (but some may mismatched due to non-coverage even within the same region)
dim(common_species_ramldb[is.na(common_species_ramldb$fg_survey),])  # 513 stocks without survey match with FishGlob 

# RAMLDB stocks with at-least_1survey_match with FishGlob
final_ramldb <- common_species_ramldb[!is.na(common_species_ramldb$fg_survey),]  # 434 stocks

#################################################################################################################################

# MAP
fg_keep$LME <- NA
unique(fg_keep$title)

fg_keep[fg_keep$title=="Aleutian Islands",]$LME <- "Aleutian Islands"
fg_keep[fg_keep$title=="Baltic Sea",]$LME <- "Baltic Sea"
fg_keep[fg_keep$title=="Eastern Bering Sea",]$LME <- "East Bering Sea"
fg_keep[fg_keep$title=="Bay of Biscay",]$LME <- "Celtic-Biscay Shelf"
fg_keep[fg_keep$title=="Gulf of Mexico",]$LME <- "Gulf of Mexico"
fg_keep[fg_keep$title=="Gulf of Alaska",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Northeast US",]$LME <- "Northeast U.S. Continental Shelf"
fg_keep[fg_keep$title=="Norway",]$LME <- "Norwegian Sea"
fg_keep[fg_keep$title=="North Sea",]$LME <- "North Sea"
fg_keep[fg_keep$title=="North Sea - ROCKALL",]$LME <- "North Sea"
fg_keep[fg_keep$title=="Southeast US",]$LME <- "Southeast U.S. Continental Shelf"
fg_keep[fg_keep$title=="Scotian Shelf",]$LME <- "Scotian Shelf"
fg_keep[fg_keep$title=="Gulf of Saint Lawrence N",]$LME <- "Labrador - Newfoundland"
fg_keep[fg_keep$title=="Gulf of Saint Lawrence S",]$LME <- "Labrador - Newfoundland"
fg_keep[fg_keep$title=="Ireland",]$LME <- "Celtic-Biscay Shelf"
fg_keep[fg_keep$title=="France",]$LME <- "Celtic-Biscay Shelf"
fg_keep[fg_keep$title=="Canada Maritimes",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - Hecate Strait",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - Queen Charlotte Sound",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - Strait of Georgia",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - Haida Gwaii",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - West Coast Vancouver Island",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="Canada - West Coast",]$LME <- "Gulf of Alaska"
fg_keep[fg_keep$title=="West Coast Triennial US",]$LME <- "California Current"

table(fg_keep$LME)

fg_keep$accepted_name %>% unique()

rm(list=setdiff(ls(), c("fg_keep", "timeseries_values_views",
                        "find_UTM_zone")))
