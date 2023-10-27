
stockid <- fg_keep$stockid_ramldb %>% unique()

ramldb <- timeseries_values_views %>%
  filter (stockid %in% fg_keep$stockid_ramldb) %>%
  dplyr::select (stockid, TB, SSB, year) %>%
  mutate (year = year)

fg_keep_simp <- fg_keep %>%
  dplyr::select (stockid_ramldb, match_key, survey) %>%
  distinct() %>%
  filter (stockid_ramldb != "At_least_1_match")

stocks_OA <- stocks_hull %>%
  left_join(fg_keep_simp, by=c("match_key", 
                               "survey_i" = "survey")) %>%
  left_join(ramldb, by= c("stockid_ramldb" = "stockid",
                          "year")) %>%
  dplyr::select (accepted_name, stockid_ramldb, match_key, 
                 TB, SSB, CH_area, year, survey_i) %>%
  filter (!is.na(stockid_ramldb))

match_key <- stocks_OA$match_key %>% unique()

pdf ("results/ch_stock.pdf")
for (mk in match_key){
  df <- stocks_OA %>%
    filter (match_key == mk) %>%
    pivot_longer(cols = c(TB,SSB),
                 names_to = "Biomass_estimate",
                 values_to = "Biomass")
  
  surveyi <- df$survey_i %>% unique()
  
  for (s in surveyi) {
    df_s <-  df %>%
      filter (survey_i == s)
  }
  
  title <- paste (mk, s, sep=" / ")
  
  P <- ggplot (df_s, aes (x=Biomass,y=CH_area))+
    geom_point()+
    facet_wrap(~Biomass_estimate, scales="free")+
    theme_bw() +
    labs (y= "Convex Hull (Km2)") +
    ggtitle (title)
    
  print (P)
}
dev.off()