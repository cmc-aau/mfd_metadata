#####
# Find metadata out of expectations (ooe)
#####

# Function to coords out of map
# Solution from https://datawanderings.com/2018/09/01/r-point-in-polygon-a-mathematical-cookie-cutter/ and https://www.linkedin.com/pulse/easy-maps-denmark-r-mikkel-freltoft-krogsholm/ using the regional map of Denmark from https://dawadocs.dataforsyningen.dk/
coords_out <- function(regions_map = NA, data_table = NA, id = "fieldsample_barcode", lat="latitude", lon="longitude"){
  data_table <- data_table[(!is.na(data_table$latitude)&!is.na(data_table$longitude)),]
  
  dd <- data_table %>%
    dplyr::select(all_of(c(id, lat, lon)))
  
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

missing_coords <-  function(missing_meta = NA){
  if(class(missing_meta)!="data.frame"){
    return(F)
  }
  samples_missing_coords <- missing_meta %>%
    filter(is.na(latitude) | is.na(longitude)) %>%
    select(fieldsample_barcode) %>%
    unlist()
  if(length(samples_missing_coords)==0){
    return(F)
  } else {
    return(samples_missing_coords)
  }
}

# Function to print missing coords results in the reportz
missing_coords_text <- function(data_table = NA, missing_meta = NA){
  print("here0")
  samples_missing_coords <- missing_coords(missing_meta = missing_meta) 
  N <- nrow(data_table)
    
  if(class(samples_missing_coords)!="character"){
    print("here1")
    asis_output("### Missing coordinates

No missing coordinates.")
  } else if(length(samples_missing_coords)==N){
    print("here2")
    asis_output("#### Missing coordinates

The coordinates (latitude and/or longitude) of all the samples are missing.")
    
  } else {
    print("here3")
    asis_output("#### Missing coordinates

The coordinates (latitude and/or longitude) of the following samples are missing:")
    print(samples_missing_coords %>% cat(sep=", "))
  }
}

# Function to print results in the report
out_coords_text <- function(data_table=NA, regions_map=NA, missing_meta = NA, id="fieldsample_barcode", lat="latitude", lon="longitude"){
  
  samples_missing_coords <- missing_coords(missing_meta = missing_meta)
  #print(samples_missing_coords)
  N <- base::nrow(data_table)
  
  if(class(samples_missing_coords)=="logical"){
    samples_missing_coords <- c()
  }
  
  if(length(samples_missing_coords)==N){
    asis_output("#### Coordinates check
  
All samples miss coordinates.
                ")
    #return(invisible(NULL))
  } else {
  
    out_of_map <- coords_out(regions_map=regions_map, data_table=data_table[!data_table$fieldsample_barcode%in%samples_missing_coords,], id=id, lat=lat, lon=lon)

    if(class(out_of_map)=="character"){
      asis_output(paste0("#### Coordinates check",
                         "\n\n",
                         "The following samples were reported from coordinates outside of Danish land:",
                         "\n\n",
                         paste0(out_of_map, collapse=", ")))
    } else {
      asis_output("#### Coordinates check
    
  No samples were reported from coordinates outside of Danish land.
                  ")
    }
  }
}

samples_on_map <- function(data_table = NA, samples_out = NA, id="fieldsample_barcode", lat="latitude", lon="longitude"){

  data_table <- data_table[(!is.na(data_table$latitude)&!is.na(data_table$longitude)),]
  
  if(nrow(data_table)==0){
    return(invisible(NULL))
  }
  
  if(class(samples_out)=="character"){
    data_table <- data_table %>%
      mutate(location=if_else(fieldsample_barcode %in% samples_out, "outside DK", "inside DK"),
             location=factor(location, levels=c("inside DK", "outside DK")))
  } else {
    data_table <- data_table %>%
      mutate(location="inside DK",
             location=factor(location, levels=c("inside DK", "outside DK")))
  }

  out <- mapDK(detail = 'region', map.colour = "grey50") +#, map.fill = "grey95") +
    geom_point(data = data_table, 
               aes(x = longitude, y = latitude, group = NA, color = location), size = 1, alpha = 1) +
    scale_color_manual(values = c(toupper("#ed9a26"), toupper("#29d6e6"))) +
    theme(legend.position = c(0.8,0.8))
  
  return(out)
}

