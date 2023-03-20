# mfd_metadata

## Intro
Metadata cleaning and report making for the Microflora Danica project.

## Repo strucutre

| Folder | Content |
| --- | --- |
| ├── analysis/                          | Results of the analysis |
| │   ├── mails_01                       | Automatic generated email and material to send to project parnter |
| │   │   └── P<NUMBER>                  | Project-specific folders |
| │   └── metadata                       | Reformatted metadata for internal usage |
| │       └── P<NUMBER>                  | Project-specific folders |
| ├── data                               | Data used to generate the results |
| │   ├── metadata                       | Metadata of the project |
| │       └── P<NUMBER>                  | Project-specific folders |
| └── scripts                            | Scripts used to analyse the data |
|     └── R_scripts                      | R scripts |
|         ├── mfd_report.Rmd             | Main notebook that generaltes all the results |
|         └── mfd_report_test_routine.R  | Per-project R Markdown template |








