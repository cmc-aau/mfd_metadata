#####
# Full testing routine for the mfd subprojects
#####

###
# Util functions
###

# Subset to mfd database fro a single subproject
mfd_subset <- function(project=NA, data_table=mfd_db){
  data_table <- data_table %>%
    filter(project_id == project)
  
  return(data_table)
}

# Source blocks of functions by theme
source(paste0(wd, "/ooe_functions.R"))
source(paste0(wd, "/coords_functions.R"))


## Functions for individual project stats

### Plot samples on the map of Denmark
pstats <- function(data_table = NA){
  
  out <- mapDK(detail = 'region', map.colour = "grey50", map.fill = "grey95") +
    geom_point(data = data_table, 
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
    dplyr::select(-mfd_hab1, -mfd_hab2, -mfd_hab3) %>%
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
    dplyr::select(-mfd_h1u, -mfd_h2u, -mfd_h3u)
  
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
missing_metadata <- function(project=NA, data_table=mfd_db){
  dd <- data_table %>%
    filter(project_id==project) %>%
    filter(if_any(everything(.), is.na))
  if(nrow(dd)>0){
    return(dd) 
  } else {
    return(F)
  }
}

summarise_missing_metadata <- function(data_table = NA){
  dd <- data_table %>%
    summarise_all(funs(sum(is.na(.))))
  if(sum(dd)>0){
    return(dd)
  } else {
  #mitigation.table <- data.frame(info=colnames(dd), current=t(dd))
    return(F)
  }
}

summarise_metadata <- function(data_table = NA){
  N <- nrow(data_table)
  dd <- data_table %>%
    summarise_all(funs(sum(!is.na(.)))) %>%
    pivot_longer(cols=everything(), names_to="field", values_to="present") %>%
    mutate(`present [%]`=present/N*100, missing=N-present, `missing [%]`=missing/N*100)
  return(dd)
}

full_check <- function(project = NA, data_table = NA, missing_meta = NA, regions = NA){ # you can use cat
  
  cat(ooe_text(project = project, data_table = data_table),
  
  missing_coords_text(data_table = data_table, missing_meta = missing_meta),
  
  out_coords_text(data_table = data_table, regions = regions, missing_meta = missing_meta),
  sep="\n")
  
}
