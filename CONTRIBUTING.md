# Introduction

Welcome to the contributing guidelines for the MIMI project. This documentation will outline some basic principles and best practices to follow when making contributions to the project. 

Whilst every effort has been made to ensure that these guidelines are as exhaustive as possible, it's likely that not everything has been covered. Therefore if you do have any questions regarding anything that is not covered in this documentation, please contact one of us: <br>
[mohammedaheed.osman@wfp.org](mohammedaheed.osman@wfp.org) <br>
[gabriel.battcock@wfp.org](gabriel.battcock@wfp.org) <br>
[kevin.tang@wfp.org](keving.tang@wfp.org) <br>

### Contributing permissions

If you would like to contribute to the MIMI project, please reach out to one of us (contact details above) to discuss your proposeed contribution, and to arrange contributor access to the repository.

### Repository structure

data_challenge/
├─ README.md    <<<<<<<<<<<<<<<<<<<<<<<<<< YOU ARE HERE
├─ allData/                              * all data, in various formats
│  ├─ gp/
│  ├─ hospitalisation/
│  ├─ mortality/
│  ├─ swab/
│  ├─ vaccine/
│  └─ ...
├─ bibliography/
│  └─ UKHSA_bibliography.bib
├─ images/
│  └─ ...                                * full-resolution copy of all plots and infographic
├─ Meeting_notes
│  └─ ...
├─ Presentation
│  ├─ images/
│  ├─ Presentation_files/
│  ├─ Presentation.html
│  └─ Presentation.qmd                   * the presentation, with editable and executable code
├─ Presentation_files
│  └─ ...
├─ R_scripts
│  ├─ Season_data/
│  ├─ source/
│  ├─ corr.R
│  ├─ source_data_entry.R
│  └─ web_scraping_all_flu_subtypes.R
├─ report/
│  ├─ images/
│  ├─ UKHSA_report_files/
│  ├─ method_notes.R
│  ├─ UKHSA_report.html
│  ├─ UKHSA_report.pdf                   * the report rendered into pdf
│  └─ UKHSA_report.qmd                   * the report, with editable and executable code
├─ new_folder/
│  ├─ new_folder/
│  ├─ new_folder/
│  └─ new_folder/
├─ data_challenge.Rproj
└─ .gitignore


### Branches

The main branch in this repository should only contain finalised and clean code, therefore please do not commit your changes directly to the main branch!

Therefore please create a new branch for any code that is a work in progress, and name your branch according to the following naming convention: `author_keyword1_keyword2`

* `author`: Please include your initials so that it is clear who is responsible for this branch
* `keyword1`: A first keyword that relates to the project or analysis that you are working on
* `keyword2`: A second keyword that relates to the project or analysis that you are working on

For example, if Mohammed Osman is working on modelling fortification scenarios in Nigeria, he may choose to name his branch: `MO_fortification_nigeria`

If you would like to learn more about branches, please read the following [documentation](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches)

Once you are happy that the code in your branch is ready to be merged into the main branch, please submit a pull request.

### Submitting pull requests


### Merging pull requests

### Summary

Thank you for contributing to the MIMI project! 
