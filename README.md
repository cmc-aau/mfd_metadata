# mfd_metadata

## Intro
Many sources of samples contributed to the Microflora Danica collection. This work orgnises the received metadata from the various subprojects in a standardised way and prepares the (semi-)automated request for the missing information.

**Latest release of the MfD ontology: `data/ontology/latest_mfd-habitat-ontology.xlsx`**  
**Latest release of the MfD field metadata: `analysis/releases/latest_mfd_db.xlsx`**  
**Latest release of the MfD technical metadata: `data/metadata/general/latest_corrected_combined_metadata.csv`**  

Please note that the sample coordinates for projects P04_3, P04_5 and P06_3 are not reliable as noted in the column "coords_reliable" from the `<VERSION>_mfd_db.xlsx` file.
The accuracy of the other coordinates was assessed with the concordance between the broad terms of the habitat ontology and the land usage annotation of Denmark as provided in [Basemap 04](https://pages.github.com/). Therefore, the samples that do not match any pixel in a 20m radius from the one corresponding to the sample were screened manually, leading to the labelling as unreliable of 32 extra samples.

The variables in the first page of the metadata document `latest_mfd_db.xlsx` are described in the second page.

The release 2024-02-06_mfd_db.xlsx was used to generate the NCBI submission.

The original server used to store the MFD barcode data is not accessible anymore and the `mfd_report.Rmd` will not work anymore. Further curation of the metadata has been done with `mfd_report_revision.Rmd`.

![alt_text](/analysis/releases/entires_time.png)

## Repo structure (relevant subset)

| Folder | Content |
| --- | --- |
| ├── analysis/                                          | Results of the analysis |
| │   └── releases/                                      | Folder with the stable releases of the MFD metadata|
| │       └── \<DATE\>_mfd_db.xlsx                       | Date-stamped releases of the MFD metadata |
| ├── data/                                              | Data used to generate the results |
| │   └─── metadata/                                     | Metadata of the project |
| │       └── general/                                   | Metadata applying to whole MFD project |
| │           └── latest_corrected_combined_metadata.csv | Technical metadata table |
| │       └── ontology/                                  | Metadata applying to whole MFD project |
| │           └── \<DATE\>mfd-habitat-ontology.xlsx      | Date-stamped releases of the habitat ontology |
| ├── envs/                                              | Computing environments |
| │       └── mfd_metadata_R.yml                         | Environment with the packages needed to reproduce the analysis |
| └── scripts/                                           | Scripts used to analyse the data |
|         └── R_scripts/                                 | R scripts |
|             ├── mfd_report.Rmd                         | Main notebook that generates all the results |
|             ├── mfd_report_revision.Rmd                | Notebook that amends results for the paper revision |
|             ├── subroject_template.Rmd                 | Per-project R Markdown template |
|             ├── mfd_report_test_routine.R              | I/O and text printing functions for the template |
|             ├── coords_functions.R                     | Functions to work on the coordinates in the template |
|             └── ooe_functions.R                        | Functions to work on the out-of-expectation entries in the template |

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
4. Open R and knit the mfd_report_revision.Rmd; on a terminal it looks like:
```
R # This opens the R terminal

rmarkdown::render("scripts/R_scripts/mfd_report_revision.Rmd", "html_document")
``` 

To compare releases, after step 4 you will need to open R and knit the mfd_releases_comparison.Rmd. In a terminal it will be:
```
R # This opens the R terminal

rmarkdown::render("scripts/R_scripts/mfd_releases_comparison.Rmd", "html_document")
```

## Prerequisites

This repo uses mamba 1.1.0 and conda 22.11.1
