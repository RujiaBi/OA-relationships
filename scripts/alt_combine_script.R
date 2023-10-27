#Zoe J Kitchel
#24 October 2023
#Occupancy-Abundance Project

#This script checks spatial coverage of RAM stocks in fishglob surveys by year

#setup
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
library(viridis)

#shapefile names
shapefile_list<-list.files(file.path(
  "data","RAM_shapefiles","ramldb_boundaries"), pattern="(.shp$)", full.names=TRUE)

#load in list of stocks in case any attributes are missing
stock_boundary_table <- fread(
         file.path("data","RAM_shapefiles","ramldb_v3.8_stock_boundary_table_v2_formatted.csv"))

#load FishGlob lat lon
#fg <- fread(file.path("data","fishglob_taxa_per_country.csv")) #this isn't helpful because it doesn't have coordinates of all tows in fishglob

#instead load up Zoes (will only work for Zoe)
fg <- fread(file.path("~","Dropbox","Repositories","trawl_spatial_turnover_git",
                      "data", "FISHGLOB_v1.5_clean.csv"))

fg <- unique(fg[,.(accepted_name, survey_unit, year, latitude, longitude, haul_id)])

#set up data table to fill
RAM_fishglob_overlap <- data.table(RAM_stock = as.character(),
                                   species = as.character(),
                                   fg_match = as.logical(),
                                   fg_survey_overlap = as.character(),
                                   fg_survey_year = as.numeric(),
                                   fg_total_haul_ids = as.numeric(),
                                   fg_stock_perc_haul_id_overlap = as.numeric(),
                                   reason_excluded = as.character())
#set up plots to show overlap
overlap_plots <- list()
# Create a world map sf
world_map <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

for (i in 1:length(shapefile_list)) {
  #load RAM polygon
  stock_shapefile <- read_sf(shapefile_list[i])
  assessid <- substring(shapefile_list[i],first = 39,
                        last = nchar(shapefile_list[i])-4)
  species <- ifelse("species" %in% colnames(stock_shapefile),
    stock_shapefile$species,
    stock_boundary_table[assessid == assessid, species])
  
  
  if(species %in% fg$accepted_name){

    #rows of fishglob with this species
    tow_overlaps <- unique(fg[accepted_name == species,
                              .(survey_unit, longitude, latitude, haul_id,year)])
    
    #delete rows with either missing longitude or latitude
    tow_overlaps <- tow_overlaps[complete.cases(tow_overlaps[,.(longitude,  latitude)])]
    
  #only keep rows that intersect with stock polygon
    
    #turn coordinates into simple feature points
    tow_overlaps.sf <- st_as_sf(tow_overlaps, coords =c("longitude","latitude"),
                                   crs = 4326)
    #match coordinate system to stock shapefiles
    tow_overlaps.sf <- st_transform(tow_overlaps.sf, crs = st_crs(stock_shapefile))

    sf_use_s2(FALSE) #switches off spherical geometry (s2) switched off
    pt_intersect<- suppressWarnings(suppressMessages(
      st_intersection(tow_overlaps.sf,  stock_shapefile)
      ))
    
    if(nrow(pt_intersect) > 0){ #do fishglob survey tow points for this species intersect stock boundary, if yes, proceed
    
    tow_overlaps_names <- unique(pt_intersect$survey_unit)
    
    #for each year of fishglob, this stock overlaps with X tows of Y total tows
    for (j in 1:length(tow_overlaps_names)) {
      fg.survey <- fg[survey_unit == tow_overlaps_names[j],]
      
      #limit to one survey of lat lon points
      tow_overlaps_survey.sf <- tow_overlaps.sf %>% filter(survey_unit == tow_overlaps_names[j])
      
      #years
      fg.years <- levels(as.factor(fg.survey$year))
 #     for (k in 1:length(fg.years)){
 #       fg.survey_yr <- fg.survey[year == fg.years[k],]
 #       
 #       #total haul ids in this survey year
 #       haul_id_total_num <- length(unique(fg.survey_yr[,haul_id])) #290
 #       
 #       #total haul ids that overlap with stock
 #       pt_intersect_overlap <- pt_intersect %>%
 #         filter(survey_unit == tow_overlaps_names[j] & year == fg.years[k])
 #       
 #       #count haul ids that overlap
 #       pt_intersect_overlap.count <- length(unique(pt_intersect_overlap$haul_id)) #75
 #       
 #       row <- data.table(matrix(ncol = 8, nrow = 1))
 #       row[,1] <- assessid
 #       row[,2] <- species
 #       row[,3] <- TRUE
 #       row[,4] <- tow_overlaps_names[j]
 #       row[,5] <- fg.years[k]
 #       row[,6] <- haul_id_total_num
 #       row[,7] <- round(pt_intersect_overlap.count/haul_id_total_num,2)*100
 #       row[,8] <- NA
#
 #         #link with full data table
 #         RAM_fishglob_overlap <- rbind(RAM_fishglob_overlap, row, use.names = F)
 #         
 #       
 #     }
      
      
      # Calculate the bounding box of stock boundary
      stock_bbox <- st_bbox(stock_shapefile)
      
      #Calculate the bounding box of FG survey points
      tow_bbox <- st_bbox(tow_overlaps_survey.sf)
      
      #map_bbox takes outer value of each
      map_bbox <- st_bbox(c(xmin = min(stock_bbox[[1]],tow_bbox[[1]]), xmax = max(stock_bbox[[3]],tow_bbox[[3]]),
                            ymax = max(stock_bbox[[4]],tow_bbox[[4]]), ymin = min(stock_bbox[[2]],tow_bbox[[2]])),
                          crs = st_crs(stock_shapefile))
      
      # Clip the world map to the bounding box of your data
      clipped_world_map <- st_intersection(world_map, st_as_sfc(map_bbox))
      
      #what portion of hauls overlap?
      prop_survey_overlap <- round((nrow(st_intersection(stock_shapefile, tow_overlaps_survey.sf))/nrow(tow_overlaps_survey.sf)),2)
      
      
      if(tow_overlaps_names[j] %in% c("AI","NZ-WCSI","NZ-ECSI","NZ-SUBA","NZ-CHAT","GOA","EBS")) { #crosses dateline!
      #map of overlap, if there is one
      overlap_plot <- ggplot() +
        # Add the world map as a background
        geom_sf(data = clipped_world_map, color=NA,fill = "grey") +
        # Add the points layer
        geom_sf(data = tow_overlaps_survey.sf, aes(color =year), size = 0.5) +
        # Add the polygon layer
        geom_sf(data = stock_shapefile, color = "lightgoldenrod3", fill = "lightgoldenrod1", alpha = 0.3) +
        #add color scale
        scale_color_viridis_c(option = "mako",begin = 0, end = 0.8, guide = guide_colorbar(frame.colour = "black", ticks.colour = NA,
                                                                  title.position = "top", title.hjust = 0.5, title = "FISHGLOB\nsurvey year")) +
        # Customize the plot
        theme_classic() +
        labs(title = paste0(assessid,"\n",species," sampled by ", tow_overlaps_names[j], "\nProportion FISHGLOB tows in stock boundary across all years: ",  prop_survey_overlap)) +
        # Change projection to pacific centric
        coord_sf(crs = "+proj=robin +lon_0=-180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
        theme(legend.direction = "horizontal", legend.position = "bottom")
   
         }else{ #doesn't cross dateline
           overlap_plot <- ggplot() +
             # Add the world map as a background
             geom_sf(data = clipped_world_map, color=NA,fill = "grey") +
             # Add the points layer
             geom_sf(data = tow_overlaps_survey.sf, aes(color =year), size = 0.5) +
             # Add the polygon layer
             geom_sf(data = stock_shapefile, color = "lightgoldenrod3", fill = "lightgoldenrod1", alpha = 0.3) +
             #add color scale
             scale_color_viridis_c(option = "mako",begin = 0, end = 0.8, guide = guide_colorbar(frame.colour = "black", ticks.colour = NA,
                                                                                                title.position = "top", title.hjust = 0.5, title = "FISHGLOB\nsurvey year")) +
             # Customize the plot
             theme_classic() +
             labs(title = paste0(assessid,"\n",species," sampled by ", tow_overlaps_names[j], "\nProportion ",tow_overlaps_names[j]," tows in stock boundary across all years: ",  prop_survey_overlap)) +
             theme(legend.direction = "horizontal", legend.position = "bottom")
      }
      #save to list
      overlap_plots[[paste0(assessid,"_",tow_overlaps_names[j])]] <- overlap_plot
    }
    
    }else{next;
#      row <- data.table(matrix(ncol = 8, nrow = 1))
#      row[,1] <- assessid
#      row[,2] <- species
#      row[,3] <- FALSE
#      row[,4] <- row[,5] <- row[,6] <- row[,7] <- NA
#      row[,8] <- "no spatial overlap"
#      RAM_fishglob_overlap <- rbind(RAM_fishglob_overlap, row, use.names = F)
#
      
    }
  }else{next;
#    row <- data.table(matrix(ncol = 8, nrow = 1))
#    row[,1] <- assessid
#    row[,2] <- species
#    row[,3] <- FALSE
#    row[,4] <- row[,5] <- row[,6] <- row[,7] <- NA
#    row[,8] <- "no spp overlap"
#    RAM_fishglob_overlap <- rbind(RAM_fishglob_overlap, row, use.names = F)

  }
  print(paste0("stock # ",i,"/",length(shapefile_list)))
}

saveRDS(overlap_plots, file.path("figures","overlap_plots.Rds"))

########################################
#CREATE PDF FROM PLOTS


# Set up the PDF file
pdf(file.path("figures","overlap_plots.pdf"), width = 8.5, height = 11)  # Adjust width and height as needed

# Determine the number of plots, plots per page, and the number of pages
n_plots <- length(overlap_plots)
plots_per_page <- 4
num_pages <- ceiling(n_plots / plots_per_page)

# Loop through the pages and arrange the plots
for (page in 1:num_pages) {
  start_plot <- (page - 1) * plots_per_page + 1
  end_plot <- min(page * plots_per_page, n_plots)
  
  # Extract the subset of plots for the current page
  page_plots <- overlap_plots[start_plot:end_plot]
  
  # Arrange the plots in a 2x2 grid
  plot_grid(plotlist = page_plots, ncol = 2, nrow = 2)
}

# Close the PDF file
dev.off()

########################################

#read in private v public key

fg_public_private_key <- fread(file.path("data","fg_public_private_key.csv"))

#link in public private
RAM_fishglob_overlap <- fg_public_private_key[RAM_fishglob_overlap, on = "fg_survey_overlap"]

#does stock span multiple surveys?
RAM_fishglob_overlap[,count_FG_survey := uniqueN(fg_survey_overlap),.(RAM_stock)]
RAM_fishglob_overlap[,spans_multiple_FG_survey := ifelse(count_FG_survey>1,TRUE,FALSE)]

saveRDS(RAM_fishglob_overlap, file.path("data", "RAM_fishglob_overlap.Rds"))
fwrite(RAM_fishglob_overlap, file.path("data","RAM_fishglob_overlap.csv"))
RAM_fishglob_overlap <- readRDS(file.path("data", "RAM_fishglob_overlap.Rds"))

RAM_fishglob_overlap.public <- RAM_fishglob_overlap
#does stock span multiple surveys?
RAM_fishglob_overlap.public[,count_FG_survey := uniqueN(fg_survey_overlap),.(RAM_stock)]
RAM_fishglob_overlap.public[,spans_multiple_FG_survey := ifelse(count_FG_survey>1,TRUE,FALSE)]

#some stocks excluded because only private matches
RAM_fishglob_overlap.public[,reason_excluded:=ifelse(public == FALSE& is.na(reason_excluded), "Private data",reason_excluded)]
#hide years of private fishglob data
RAM_fishglob_overlap.public[,fg_survey_year:=ifelse(public == FALSE, NA,fg_survey_year)]
#hide # tows of private fishglob data
RAM_fishglob_overlap.public[,fg_total_haul_ids:=ifelse(public == FALSE, NA,fg_total_haul_ids)]
#hide percent overlap for private fishglob data
RAM_fishglob_overlap.public[,fg_stock_perc_haul_id_overlap:=ifelse(public == FALSE, NA,fg_stock_perc_haul_id_overlap)]

#reduce to unique values
RAM_fishglob_overlap.public <- unique(RAM_fishglob_overlap.public[, .(fg_survey_overlap, Survey_Name_Season, public, RAM_stock, species, fg_match, fg_survey_year,
                                                                      fg_total_haul_ids, fg_stock_perc_haul_id_overlap, reason_excluded, spans_multiple_FG_survey)])

saveRDS(RAM_fishglob_overlap.public, file.path("data", "RAM_fishglob_overlap.public.Rds"))
fwrite(RAM_fishglob_overlap.public, file.path("data","RAM_fishglob_overlap.public.csv"))

RAM_fishglob_overlap.public.only <- RAM_fishglob_overlap.public[public == TRUE,]

no_match_stocks <- RAM_fishglob_overlap[!is.na(reason_excluded),]
#in total, 339 out of 685 stocks have matches in all fishglob
#240 are excluded because of no species overlap, 106 excluded because of species match but no spatial overlap

nrow(unique(RAM_fishglob_overlap.public.only[,.(RAM_stock, species)]))

#in total, 256 out of 685 stocks have matches in public fishglob

match_stocks <- RAM_fishglob_overlap[is.na(reason_excluded),]

#of those matches, 8 have an annual spatial match of over 90% tows with at least one survey, (4 w/ public survey)
#of those matches, 90 have an annual spatial match of over 50% tows with at least one survey (78 w/ public survey)
#of those matches, 172 have an annual spatial match of over 30% tows with at least one survey (146 w/public survey)

match_stocks[,mean_overall_haul_overlap := 
                                 mean(fg_stock_perc_haul_id_overlap),.(fg_survey_overlap,RAM_stock)]

RAM_fishglob_overlap.public.only[,mean_overall_haul_overlap := 
                              mean(fg_stock_perc_haul_id_overlap),.(fg_survey_overlap,RAM_stock)]

match_stocks.a <- unique(match_stocks[,.(RAM_stock, species, fg_survey_overlap,Survey_Name_Season, mean_overall_haul_overlap,spans_multiple_FG_survey)])
match_stocks.public.a <- unique(RAM_fishglob_overlap.public.only[,.(RAM_stock, species, fg_survey_overlap,Survey_Name_Season, mean_overall_haul_overlap,spans_multiple_FG_survey)])

#which span multiple?
RAM_stock_cross_surveys <- unique(match_stocks.a[spans_multiple_FG_survey == TRUE,.(RAM_stock)]) #205 of 339

RAM_stock_cross_surveys.public <- unique(match_stocks.public.a[spans_multiple_FG_survey == TRUE,.(RAM_stock, Survey_Name_Season)])

nrow(match_stocks.a[mean_overall_haul_overlap >=90,])
nrow(match_stocks.a[mean_overall_haul_overlap >=50,])
nrow(match_stocks.a[mean_overall_haul_overlap >=30,])

#public
nrow(match_stocks.public.a[mean_overall_haul_overlap >=90,])
nrow(match_stocks.public.a[mean_overall_haul_overlap >=50,])
nrow(match_stocks.public.a[mean_overall_haul_overlap >=30,])

#which surveys match well with RAM stocks? (>50% on average)
unique(match_stocks.a[mean_overall_haul_overlap>50,Survey_Name_Season])

#[1] "Eastern Bering Sea"                  "Gulf of Alaska"                      "Canada West Coast Haida Gwaii"       "Aleutian Islands"                    "Norway Barents Sea"                 
#[6] "Canada S Gulf of St. Lawrence"       "Canada Scotian Shelf Fall"           "Canada Scotian Shelf Summer"         "Canada Scotian Shelf Spring"         "Canada N Gulf of St. Lawrence"      
#[11] "Canada Hectate Strait"               "Canada West Coast Vancouver Island"  "Canada Queen Charlotte Sound"        "Northern Ireland"                    "N Sea Q1"                           
#[16] "N Sea Q3"                            "Scotland Shelf Sea Q1"               "Scotland Shelf Sea Q4"               "South Africa"                        "Canada Newfoundland"                
#[21] "Southeast US Spring"                 "Southeast US Fall"                   "New Zealand Chatham Rise"            "New Zealand Sub-Antarctic"           "New Zealand East Coast South Island"
#[26] "Iceland"                             "Southeast US Summer"                 "Baltic Sea Q4"                       "Baltic Sea Q1"                       "English Channel"                    
#[31] "Irish Sea"                           "Portugal"                            "Rockall Plateau"       

#May be worth it to get South Africa, and New Zealand data for this project?
