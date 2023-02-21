## Functions for individual project stats

### Plot samples on the map of Denmark
pstats <- function(project = NA, data = mfd_db){
  
  temp <- data %>% filter(project_id %in% project)
  out <- mapDK(detail = 'region', map.colour = "grey50", map.fill = "grey95") +
    geom_point(data = temp, 
               aes(x = longitude, y = latitude, group = NA), size = 1, alpha = 0.5) +
    theme(legend.position = c(0.8,0.8))
  
  return(out)
}

### Plot gegraphical coordinates on the map of Denmark
tstats <- function(project = NA, data = mfd_db){
  
  out <- data %>% filter(project_id %in% project) %>%
    mutate(gps_date_hab1 = ifelse(!is.na(mfd_hab1) & !is.na(latitude)& !is.na(sampling_date), 1, 0)) %>%
    group_by(project_id) %>%
    summarize(n_samples = n(),
              gps = sum(!is.na(latitude)),
              sampling_date = sum(!is.na(sampling_date)),
              mfd_areatype = sum(!is.na(mfd_areatype)),
              mfd_sampletype = sum(!is.na(mfd_sampletype)),
              mfd_hab1 = sum(!is.na(mfd_hab1)),
              mfd_hab2 = sum(!is.na(mfd_hab2)),
              mfd_hab3 = sum(!is.na(mfd_hab3)),
              gps_date_hab1 = sum(gps_date_hab1)) %>% 
    t() %>% 
    as.data.frame() %>%
    kable()
  
  return(out)
}

### Habitats per subproject count
hstats <- function(project = NA, data = mfd_db, cropt = 25){
  
  out <- data %>% filter(project_id %in% project) %>%
    count(mfd_areatype, mfd_sampletype,mfd_hab1,mfd_hab2, mfd_hab3) %>%
    mutate(mfd_hab1 = ifelse(nchar(mfd_hab1) > cropt, paste0(substr(mfd_hab1, 1, cropt),".."), mfd_hab1),
           mfd_hab2 = ifelse(nchar(mfd_hab2) > cropt, paste0(substr(mfd_hab2, 1, cropt),".."), mfd_hab2),
           mfd_hab3 = ifelse(nchar(mfd_hab3) > cropt, paste0(substr(mfd_hab3, 1, cropt),".."), mfd_hab3)) %>%
    kable()
  
  return(out)
}

### Ontology parser

add_ontology <- function(data = NA, ontology_path){
  
  mfd_ontology <- readxl::read_excel(ontology_path)
  
  test1 <- data %>%
    select(-mfd_hab1, -mfd_hab2, -mfd_hab3) %>%
    mutate(mfd_h1u = paste0(mfd_sampletype, mfd_areatype, paste0(str_sub(habitat_typenumber, 1,1),"000")),
           mfd_h2u = paste0(mfd_sampletype, mfd_areatype, paste0(str_sub(habitat_typenumber, 1,2),"00")),
           mfd_h3u = paste0(mfd_sampletype, mfd_areatype, habitat_typenumber))
  
  mfd_hab1 <- mfd_ontology %>% 
    count(mfd_sampletype, mfd_areatype,mfd_hab1_code, mfd_hab1) %>%
    mutate(mfd_h1u = paste0(mfd_sampletype, mfd_areatype,mfd_hab1_code))
  
  mfd_hab2 <- mfd_ontology %>% 
    count(mfd_sampletype, mfd_areatype,mfd_hab2_code, mfd_hab2) %>%
    mutate(mfd_h2u = paste0(mfd_sampletype, mfd_areatype,mfd_hab2_code))
  
  mfd_hab3 <- mfd_ontology %>% 
    count(mfd_sampletype, mfd_areatype,mfd_hab3_code, mfd_hab3) %>%
    mutate(mfd_h3u = paste0(mfd_sampletype, mfd_areatype,mfd_hab3_code))
  
  test2 <- test1 %>% left_join(mfd_hab1 %>% select(mfd_h1u, mfd_hab1), by = "mfd_h1u") %>%
    left_join(mfd_hab2 %>% select(mfd_h2u, mfd_hab2), by = "mfd_h2u") %>%
    left_join(mfd_hab3 %>% select(mfd_h3u, mfd_hab3), by = "mfd_h3u") %>%
    select(-mfd_h1u, -mfd_h2u, -mfd_h3u)
  
  return(test2)    
}

### Find missing info
missing_info <- function(project = NA, prj_table = mfd_projects){
  dd <- prj_table %>%
    filter(project_id==project)
  mitigation.table <- data.frame(info=colnames(dd), current=t(dd)) %>%
    mutate(status=if_else((current!=""&!is.na(current)), "complete", "incomplete"),
           mitigation=case_when(info%in%c("project_id", "description", "people", "responsible", "comment")&status=="incomplete"~paste0("Ask: ", dd[,"responsible"]),
                                .default="No action needed")) %>%
    select(info, status, mitigation) %>%
    `rownames<-`(c()) %>%
    kable()
  return(mitigation.table)
}

### Find missing metadata
missing_metadata <- function(project = NA, data_table = mfd_db){
  dd <- data_table %>%
    filter(project_id==project) %>%
    summarise_all(funs(sum(is.na(.))))
  mitigation.table <- data.frame(info=colnames(dd), current=t(dd))
  return(dd)
}

### Find coords out of map
#   Solution from https://datawanderings.com/2018/09/01/r-point-in-polygon-a-mathematical-cookie-cutter/ and https://www.linkedin.com/pulse/easy-maps-denmark-r-mikkel-freltoft-krogsholm/ using the regional map of Denmark from https://dawadocs.dataforsyningen.dk/

#url = "https://api.dataforsyningen.dk/landsdele?format=geojson"
#geofile = tempfile()
#download.file(url, geofile)
#regions <- rgdal::readOGR(geofile)# %>%

coords_out <- function(project = NA, regions_map = regions, data_table = mfd_db, id = "fieldsample_barcode", lat="latitude", lon="longitude"){
  dd <- data_table %>%
    filter(project_id==project) %>%
    select(all_of(c(id, lat, lon)))
  
  #dd <- rbind(dd, data.frame(fieldsample_barcode="MFDXXXX", latitude=40.5389, longitude=14.9729)) # Testing
  
  coordinates(dd) <- c(lon, lat)
  proj4string(dd) <- CRS("+proj=longlat +datum=WGS84")
  proj4string(dd) <- proj4string(regions_map)
  pointsinpoly <- over(dd, regions_map)
  to_return <- dd$fieldsample_barcode[is.na(pointsinpoly$navn)]
  
  if(length(to_return>0)){
    return(to_return)
  } else {
    return(F)
  }
  
}

### Find metadata out of expectations
sample_type.accepted <- c("biofilm", "sandfilter", "sediment", "sludge", "soil", "water", "water(dilute)") # List of accepted values "sample_type" variable
habitat_type.accepted <- c("agriculture", "built_environment", "city_other", "city_parks", "city_pond", "coast", "drinking_water", "fjords", "groundwater", "harbour", "lakes", "marine", "natural_soil", "polluted_soil", "rainwater_city", "rainwater_highway", "streams", "subterranean", "wastewater") # List of accepted values "habitat_type" variable

ooe_metadata <- function(project = NA, data_table = mfd_db){
  dd <- data_table %>%
    filter(project_id==project) %>%
    return(dd)
}

