<!-- badges: start -->
![Language: en-GB](https://img.shields.io/badge/language-en--GB-c04384)
[![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-brightgreen)](https://raw.githubusercontent.com/inbo/citeme/refs/heads/main/inst/licenses/cc_by_4_0.md)
[![Release](https://img.shields.io/github/release/inbo/erl-butterflies-2025.svg)](https://github.com/inbo/erl-butterflies-2025/releases)
[![Project Status: Concept - Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
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
This repository contains the reproducible analyses that investigates changes in the conservation status of European butterflies by comparing the 2010 and 2025 European Red Lists. It quantifies changes in extinction risk using the Red List Index (RLI) and examines how ecological, biogeographical, and life-history traits are associated with changes in species' threat status. The analyses combine Red List assessments with species trait data to identify the groups of butterflies that are most vulnerable and to provide evidence for conservation policy and biodiversity monitoring across Europe.
<!-- description: end -->

### Order of execution

Follow the steps below to run the scripts in a logical order.

> coming soon

### Repo structure

```bash
├── source                         ├ ...
├── data                           ├ ...
├── output                         ├ ...
├── media                          ├ ...
├── checklist.yml                  ├ options checklist package (https://github.com/inbo/checklist)
├── inst
│   └── en_gb.dic                  ├ dictionary with words that should not be checked by the checklist package
├── .github                        │ 
│   ├── workflows                  │ 
│   │   └── checklist_project.yml  ├ GitHub repo settings
│   ├── CODE_OF_CONDUCT.md         │ 
│   └── CONTRIBUTING.md            │
├── erl-butterflies-2025.Rproj     ├ R project
├── README.md                      ├ project description
├── LICENSE.md                     ├ licence
├── CITATION.cff                   ├ citation info
├── .zenodo.json                   ├ zenodo metadata
└── .gitignore                     ├ files to ignore
```
