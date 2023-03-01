#####
# Find metadata out of expectations (ooe)
#####

# Variables with accepted values, to be moved on the Markdown
sample_type.accepted <- c("biofilm", "sandfilter", "sediment", "sludge", "soil", "water", "water(dilute)") # List of accepted values "sample_type" variable
habitat_type.accepted <- c("agriculture", "built_environment", "city_other", "city_parks", "city_pond", "coast", "drinking_water", "fjords", "groundwater", "harbour", "lakes", "marine", "natural_soil", "polluted_soil", "rainwater_city", "rainwater_highway", "streams", "subterranean", "wastewater") # List of accepted values "habitat_type" variable

# Function to find ooe metadata
ooe_metadata <- function(project = NA, data_table = NA){
  df <- data_table %>%
    filter(project_id==project,
           !sample_type%in%sample_type.accepted,
           !habitat_type%in%habitat_type.accepted)
  if(nrow(df)==0){
    return(F)
  } else {
    return(df)
  }
}

# Function to print results in the report
ooe_text <- function(project = NA, data_table = NA){
  ooe_meta <- ooe_metadata(project = project, data_table = data_table)
  
  if(class(ooe_meta)=="data.frame"){
    asis_output("### Metadata entry out of expectations

The following samples contain unexpected entries:")
    print(ooe_meta)
  } else {
    asis_output("### Metadata entry out of expectations

No samples have unexpected entries.")
  }
}