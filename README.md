# mfd_metadata

## Intro
Many sources of samples contributed to the Microflora Danica collection. This work orgnises the received metadata from the various subprojects in a standardised way and prepares the (semi-)automated request for the missing information.

**Latest release of the MfD metadata: analysis/releases/2023-09-25_mfd_db.xlsx**

The final structure uses a "mfd_project" table listing the subprojects with summary info (e.g. description, people involved, etc.) and links to the location of the per-subproject "mfd_subproject" tables. Those tables list all data tables linked to the subproject. There are three types of tables associated to each subproject:
- main: info about the minimal metadata variables (in the mfd_report script this is governed by the variable "minimal_indices");
- mapping: sample-by-sample matching of the MfD ID with the naming scheme from the project partner (if present);
- table\<N\>: all the other tables containing furhter metadata (e.g. vegetation coverage, land usage, etc.). In this case the variable might not be intuitive and it is mandatory for each table to have two sheets. The first sheet (called "variables") contains the data proper, whilst the second one (called "description") contains a verbose description of the variables.

![alt_text](/data/images/mfd_files_organization.png)

![alt_text](/analysis/releases/entires_time.png)


## Repo strucutre

| Folder | Content |
| --- | --- |
| ├── analysis/                                        | Results of the analysis |
| │   ├── mails_01/                                    | Automatic generated email and material to send to project parnters |
| │   │   └── P\<NUMBER\>/                             | Project-specific folders |
| │   ├── metadata/                                    | Reformatted metadata for internal usage |
| │   │   └── P\<NUMBER\>/                             | Project-specific folders |
| │   └── releases/                                    | Folder with the stable releases of the MfD metadata|
| │       └── \<DATE\>_mfd_db.xlsx/                    | Date-stamped releases of the MfD metadata |
| ├── data/                                            | Data used to generate the results |
| │   └─── metadata/                                   | Metadata of the project |
| │       └── general/                                 | Metadata applying to whole mfd project |
| │           └── example_table.xlsx                   | Table to be sent to project partners as a "full table" |
| │           └── minimal_metadata_descriptors.xlsx    | Explanation of the variables to be requested |
| │       └── P\<NUMBER\>/                             | Project-specific folders |
| │       └── mfd_db.xlsx                              | Curated metadata in a single table |
| │       └── mfd_projects.xlsx                        | Curated information about the projects in a single table |
| ├── envs/                                            | Computing environments |
| │       └── mfd_metadata_R.yml                       | Environment with the packages needed to reproduce the analysis |
| └── scripts/                                         | Scripts used to analyse the data |
|         └── R_scripts/                               | R scripts |
|             ├── mfd_report.Rmd                       | Main notebook that generaltes all the results |
|             ├── subroject_template.Rmd               | Per-project R Markdown template |
|             ├── mfd_report_test_routine.R            | I/O and text printing functions for the template |
|             ├── coords_functions.R                   | Functions to work on the coordinates in the template |
|             └── ooe_functions.R                      | Functions to work on the out-of-expectation entries in the template |

## Workflow

In order to re-create the curation of the mfd metadata please follow the steps:

1. Clone this repo
2. Create the environment:
```
mamba env create -f envs/mfd_metadata_R.yml
```
3. Activate the environment
```
mamba activate mfd_metadata_R
```
4. Open R and knit the mfd_report.Rmd; on a terminal it looks like:
```
R # This opens the R terminal

rmarkdown::render("scripts/R_scripts/mfd_report.Rmd", "html_document")
``` 

To compare releases, after step 4 you will need to open R and knit the mfd_releases_comparison.Rmd. In a terminal it will be:
```
R # This opens the R terminal

rmarkdown::render("scripts/R_scripts/mfd_releases_comparison.Rmd", "html_document")
```

## Prerequisites

This repo uses mamba 1.1.0 and conda 22.11.1


