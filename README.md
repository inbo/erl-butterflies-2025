<!-- badges: start -->
![Language: en-GB](https://img.shields.io/badge/language-en--GB-c04384)
[![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-brightgreen)](https://raw.githubusercontent.com/inbo/citeme/refs/heads/main/inst/licenses/cc_by_4_0.md)
[![Release](https://img.shields.io/github/release/inbo/erl-butterflies-2025.svg)](https://github.com/inbo/erl-butterflies-2025/releases)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

# Code and analyses for the 2025 European Red List of butterflies and comparison with the 2010 assessment

[Maes, Dirk![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-7947-3788)[^aut][^cre][^INBO];
[Warren, Martin S.](mailto:martin.warren%40bc-europe.eu)[^aut][^BtCE];
[Ellis, Sam](mailto:sam.ellis%40bc-europe.eu)[^aut][^BtCE];
[Langeraert, Ward![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-5900-8109)[^aut][^INBO];
[Verovnik, Rudi![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0002-5841-5925)[^aut][^UnoL];
[van Swaay, Chris A. M.![ORCID logo](https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png)](https://orcid.org/0000-0003-0927-2216)[^aut][^DtBC];
[Research Institute for Nature and Forest (INBO)](mailto:info%40inbo.be)[^cph][^fnd][^pbl]

[^aut]: author
[^BtCE]: Butterfly Conservation Europe
[^cph]: copyright holder
[^cre]: contact person
[^DtBC]: Dutch Butterfly Conservation
[^fnd]: funder
[^INBO]: Research Institute for Nature and Forest (INBO)
[^pbl]: publisher
[^UnoL]: University of Ljubljana

**keywords**:  European butterflies; Red List Index; extinction risk; conservation status; species traits

<!-- community: inbo -->

### Description
<!-- description: start -->
This repository contains reproducible analyses investigating changes in the conservation status of European butterflies by comparing the 2010 and 2025 European Red Lists.
It quantifies changes in extinction risk using the Red List Index (RLI) and examines how ecological, biogeographical, and life-history traits are associated with changes in species' threat status.
The analyses combine Red List assessments with species trait data to identify the groups of butterflies that are most vulnerable and to provide evidence for conservation policy and biodiversity monitoring across Europe.
<!-- description: end -->

### Execution steps

The analyses were developed and tested with **R 4.6.1**. Using the same R version is recommended to ensure reproducibility.

#### 1. Install the required packages

Install the required packages from CRAN:

```r
install.packages(c(
  "tidyverse",  # Data import, wrangling, and visualisation
  "zen4R",      # Download input data from Zenodo
  "targets",    # Manage and run the analysis pipeline
  "tarchetypes",# Additional target patterns used in the pipeline
  "boot"        # Bootstrap estimates and confidence intervals
))
```

The `effectclass` package is installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("inbo/effectclass") # Classify RLI changes based on their confidence intervals
```

#### 2. Open the project

Open `erl-butterflies-2025.Rproj` in RStudio.
The project should be run from its root directory so that all relative paths are resolved correctly.

#### 3. Run the analysis

The analyses are implemented as a [`targets`](https://books.ropensci.org/targets/) pipeline.
Run the complete pipeline with:

```r
targets::tar_make(
  script = "./source/targets/_targets.R",
  store = "./source/targets"
)
```

The pipeline automatically downloads the required input data from Zenodo (*link*).
Generated figures and tables are written to the `output` directory.

The `{targets}` pipeline keeps intermediate results in `source/targets/objects`.
Individual results can be retrieved without rerunning the complete analysis, for example:

```r
targets::tar_read(
  rli_change_boot_Elevation,
  store = "./source/targets"
)
```

To rerun the pipeline after changing the code, simply run `tar_make()` again.
`{targets}` will determine which targets need to be recomputed based on their dependencies.

#### 4. Output

After a successful pipeline run, the main results are available in:

```text
output/
├── figures/
└── tables/
```

The `source/targets/objects` directory contains cached intermediate results managed by `{targets}` and is used to avoid unnecessarily repeating computationally intensive steps.

### Repo structure

```bash
├── source
│   ├── targets                    ├ {targets} pipeline folder
│   └── R                          ├ helper functions for the pipeline
├── data                           ├ data folder automatically created during code execution
├── output                         ├ output folder automatically created during code execution
│   ├── figures                    │ 
│   └── tables                     │
│
├── erl-butterflies-2025.Rproj     ├ R project
├── README.md                      ├ project description
├── LICENSE.md                     ├ license
├── CITATION.cff                   ├ citation info
├── .zenodo.json                   ├ zenodo metadata
├── .gitignore                     ├ files to ignore
│
├── checklist.yml                  ├ options checklist package (https://github.com/inbo/checklist)
├── organisation.yml               ├ organisation settings checklist package
├── inst
│   └── en_gb.dic                  ├ dictionary with words that should not be checked by the checklist package
└── .github                        │ 
    ├── workflows                  │ 
    │   └── checklist_project.yml  ├ GitHub repo settings
    ├── CODE_OF_CONDUCT.md         │ 
    └── CONTRIBUTING.md            │
```
