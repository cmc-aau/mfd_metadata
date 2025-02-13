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
    mutate(mfd_h1u = paste0(mfd_sampletype, mfd_areatype, mfd_hab1_code))
  
  mfd_hab2 <- mfd_ontology %>% 
    count(mfd_sampletype, mfd_areatype,mfd_hab2_code, mfd_hab2) %>%
    mutate(mfd_h2u = paste0(mfd_sampletype, mfd_areatype, mfd_hab2_code))
  
  mfd_hab3 <- mfd_ontology %>% 
    count(mfd_sampletype, mfd_areatype,mfd_hab3_code, mfd_hab3) %>%
    mutate(mfd_h3u = paste0(mfd_sampletype, mfd_areatype, mfd_hab3_code))
  
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
missing_metadata <- function(data_table = NA, indices = NA){
  if(class(indices)!="character"){
    indices <- colnames(data_table)
  }
  
  dd <- data_table[, indices] %>%
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

summarise_metadata <- function(data_table = NA, indices = NA){
  if(class(indices)!="character"){
    indices <- colnames(data_table)
  }
  
  data_table <- data_table %>%
    select(indices)
  
  N <- nrow(data_table)
  dd <- data_table %>%
    summarise_all(funs(sum(!is.na(.)))) %>%
    pivot_longer(cols=everything(), names_to="field", values_to="present") %>%
    mutate(`present [%]`=present/N*100, missing=N-present, `missing [%]`=missing/N*100)
  return(dd)
}

print_incomplete_entries <- function(data_table = NA, missing_indices = NA,
                                     fname = NA, mapping = NA,
                                     minimal_indices = NA, project_name = NA,
                                     minimal_indices_explanation = NA){
  
  minimal_explanation <- openxlsx::read.xlsx(minimal_indices_explanation, 1)
  
  data_table <- data_table[] %>%
    mutate(across(everything(), ~trimws(.x)),
           habitat_typenumber=if_else(habitat_typenumber=="", NA, habitat_typenumber),
           sitename=if_else(sitename=="", NA, sitename))
  
  if(file.exists(fname)){
    file.remove(fname)
  }
  
  if(class(mapping)=="data.frame"){
    colnames(mapping) <- c("sample_id", "fieldsample_barcode")
    data_table <- full_join(data_table, mapping, by="fieldsample_barcode")
    minimal_indices <- c(minimal_indices, "sample_id")
  }
  
  missing_meta <- missing_metadata(data_table = data_table, indices = minimal_indices)
  
  missing_indices <- c(missing_meta$fieldsample_barcode)
  
  if(class(minimal_indices)=="character"){
    data_table <- data_table[, minimal_indices]
  }
  
  data_table <- data_table %>%
    mutate(project_name=project_name)
  
  order.namaes <- c(missing_indices, data_table$fieldsample_barcode[!data_table$fieldsample_barcode%in%missing_indices])
  data_table <- data_table[match(order.namaes, data_table$fieldsample_barcode),] %>%
    filter(!is.na(fieldsample_barcode))
  
    wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb, "variables")
    header_style <- openxlsx::createStyle(textDecoration = "Bold")
    openxlsx::writeData(wb, "variables", data_table, headerStyle = header_style)
    
    tmp <- data.frame(x=which(is.na(data_table))%%nrow(data_table), y=(which(is.na(data_table))-1)%/%nrow(data_table)+1)
    
    empty_cell_style <- openxlsx::createStyle(fgFill = toupper("#bce0a8"))
    check_cell_style <- openxlsx::createStyle(fgFill = toupper("#e38140"))
    
    check_cols <- which(colnames(data_table)%in%c("mfd_hab1", "mfd_hab2", "mfd_hab3"))
    
    for(i in 1:nrow(tmp)){
      
      if(tmp$y[i]%in%check_cols){
        openxlsx::addStyle(wb, sheet = "variables", style = check_cell_style,
                           rows = ifelse(tmp$x[i]==0, nrow(data_table), tmp$x[i])+1, cols = tmp$y[i])
      } else {
      openxlsx::addStyle(wb, sheet = "variables", style = empty_cell_style,
                         rows = ifelse(tmp$x[i]==0, nrow(data_table), tmp$x[i])+1, cols = tmp$y[i])
      }
      
    }
    
    openxlsx::addWorksheet(wb, "description")
    header_style <- openxlsx::createStyle(textDecoration = "Bold")
    openxlsx::writeData(wb, "description", minimal_explanation, headerStyle = header_style)
    
    openxlsx::saveWorkbook(wb, fname)
}

print_internal_map <- function(data_table = NA, fname = NA){
  
  if(file.exists(fname)){
    file.remove(fname)
  }
  
  data_table <- data_table %>%
    mutate(across(everything(), ~if_else(.x=="", NA, .x)))
  
  minimal_explanation <- data.frame(variable=c("project_id",
                                               "project_name",
                                               "description",
                                               "extended_metadata",
                                               "people",
                                               "responsible",
                                               "comment",
                                               "metadata_map"),
                                    description=c("Internal ID of the projects",
                                                  "Lay name of the project",
                                                  "Short paragraph describing the project",
                                                  "Content of the extended metadata associated to the project",
                                                  "Research patner(s)",
                                                  "Person responsible for the project",
                                                  "Comment to the project or action to be taken",
                                                  "Location of the metadata map for each project"))
  
  missing_meta <- missing_metadata(data_table = data_table, indices = colnames(data_table))
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "variables")
  header_style <- openxlsx::createStyle(textDecoration = "Bold")
  openxlsx::writeData(wb, "variables", data_table, headerStyle = header_style)
  
  tmp <- data.frame(x=which(is.na(data_table))%%nrow(data_table), y=(which(is.na(data_table))-1)%/%nrow(data_table)+1)
  
  empty_cell_style <- openxlsx::createStyle(fgFill = toupper("#bce0a8"))
  
  for(i in 1:nrow(tmp)){
    openxlsx::addStyle(wb, sheet = "variables", style = empty_cell_style,
                         rows = ifelse(tmp$x[i]==0, nrow(data_table), tmp$x[i])+1, cols = tmp$y[i])
  }
  
  openxlsx::addWorksheet(wb, "description")
  header_style <- openxlsx::createStyle(textDecoration = "Bold")
  openxlsx::writeData(wb, "description", minimal_explanation, headerStyle = header_style)
  
  openxlsx::saveWorkbook(wb, fname)
  
  
}

print_email <- function(project_name = NA, receivers = c(), requested_indices = c(), fname = NA, paragraph2 = NA){
  # print_email(fname=paste0(wd, "/test.txt"), receivers=c("Thomas Bygh Nymann Jensen <tbnj@bio.aau.dk>", "Francesco Delogu <frde@bio.aau.dk>"), requested_indices = c("pH), "temperature)
  # print_email(fname=paste0(wd, "/test.txt"), receivers=c("Thomas Bygh Nymann Jensen <tbnj@bio.aau.dk>"), requested_indices = c("temperature"))
  
  if(class(paragraph2)=="character"){
    paragraph2 <- paste0(paragraph2, "\n")
  } else {
    paragraph2 <- ""
  }
  
  receivers <- unlist(str_split(receivers, "; "))
  first_names <- unlist(lapply(str_split(unlist(str_split(receivers, " <"))[c(T,F)], " "), function(x) x[[1]]))
  
  if(length(requested_indices)>1){
    indices_string <- paste0(paste0(requested_indices[1:(length(requested_indices)-1)], collapse=", "), " and ", requested_indices[length(requested_indices)])
    index_number <- "s"
  } else {
    indices_string <- requested_indices[1]
    index_number <- ""
  }
  
  sink(file=fname)
  cat(paste(paste("To:", paste0(receivers, collapse=", "), sep=" "),
            paste("Object:", project_name, "data collab", sep=" "),
            "\n",
            paste0(paste("Dear", paste0(first_names, collapse=" and "), sep=" "), ",\n"),
            paste(paste0("I am writing you about the ", project_name, " data collaboration.",
                  " We were reviewing the data in order to proceed with the analysis and we realised that some entries are missing (i.e. from variable", index_number, ":"),
                  paste0(indices_string,
                  ").\n"),
                  sep=" "),
            paste("Could you please fill in the missing entires (if you are able to retrieve them) in the attached file \"table_to_fill.xlsx\"?",
            "The complete table should look like the extract \"example_table.xlsx\".", sep=" "),
            paste("In the \"table_to_fill.xlsx\" the first sheet (called variables) contains the data whilst the second one (called description) explains each variable and what kind of input is needed from your side.",
                  "The \"variables\" sheet includes the missing entries for which your input is needed (highlighted in green), as well as the missing entries that will be automatically filled by us (highlighted in orange) given your input on the variable \"habitat_typenumber\".",
                  "We included the ontology file (\"2022-11-28_mfd-habitat-ontology.xlsx\") to help in choosing the most descriptive habitat_typenumber.\n",
                  sep=" "),
            paragraph2,
            "Best regards,",
            "Per Halkjær Nielsen",
            sep="\n"))
  sink(file=NULL)
}

full_check <- function(project_name = NA, data_table = NA, missing_meta = NA, regions = NA){ # you can use cat
  
  cat(ooe_text(project = project, data_table = data_table),
  
  missing_coords_text(data_table = data_table, missing_meta = missing_meta),
  
  out_coords_text(data_table = data_table, regions = regions, missing_meta = missing_meta),
  sep="\n")
  
}
