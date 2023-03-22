# mfd_metadata

## Intro
Metadata cleaning and report making for the Microflora Danica project.

## Repo strucutre

| Folder | Content |
| --- | --- |
| ├── analysis/                                        | Results of the analysis |
| │   ├── mails_01/                                    | Automatic generated email and material to send to project parnters |
| │   │   └── P\<NUMBER\>/                             | Project-specific folders |
| │   └── metadata/                                    | Reformatted metadata for internal usage |
| │       └── P\<NUMBER\>/                             | Project-specific folders |
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
|     └── R_scripts/                                   | R scripts |
|         ├── mfd_report.Rmd                           | Main notebook that generaltes all the results |
|         └── mfd_report_test_routine.R                | Per-project R Markdown template |

## Workflow

1. Clone this repo
2. Create the environment:
```
mamba env create -f /envs/mfd_metadata_R.yml
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

## Prerequisites

This repo uses mamba 1.1.0 and conda 22.11.1


