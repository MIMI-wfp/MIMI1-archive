# Introduction

Welcome to the contributing guidelines for the MIMI project. This documentation will outline some basic principles and best practices to follow when making contributions to the project. 

Whilst every effort has been made to ensure that these guidelines are as detailed as possible, it's likely that not everything has been covered. Therefore if you do have any questions regarding anything that is not covered in this documentation, please contact one of us: <br>
[mohammedaheed.osman@wfp.org](mohammedaheed.osman@wfp.org) <br>
[gabriel.battcock@wfp.org](gabriel.battcock@wfp.org) <br>
[kevin.tang@wfp.org](keving.tang@wfp.org) <br>

### Contributing permissions

If you would like to contribute to the MIMI project, please reach out to one of us (contact details above) to discuss your proposeed contribution, and to arrange contributor access to the repository.

### Repository structure

The MIMI repository has 3 main folders to categorise all projects (`data_rich`, `data_constrained` and `universal_functions`). Any new project should be created as a sub-folder within one of these categories:

* `data_rich` - Projects relating to MIMI data-rich contexts<br>
* `data_constrained` - Projects relating to MIMI data-constrained contexts<br>
* `universal_functions` - A place to store any functions that may be used across all projects, regardless of context

Below is a directory tree that outlines the structure of the MIMI repository:

```
MIMI/
├─ CONTRIBUTING.md    <<<<<<<<<<<<<<<<<<<<<<<<<< YOU ARE HERE
├─ data_rich/                              
│  ├─ all_base_models/
│  ├─ fortification_models/
│  ├─ data_requests/
│  ├─ India/
│  ├─ ethiopia/
│  ├─ individual_level_india/
│  ├─ dietary_assessment/
│  └─ ...
├─ data_constrained/
│  ├─ LSFF_indicators/
│  └─ ...
├─ universal_functions/
│  ├─ iron_full_probability/
│  └─ ...                                
├─ MIMI.Rproj
└─ .gitignore
```


### Branches

The main branch in this repository should only contain finalised and clean code, therefore please do not commit your changes directly to the main branch!

Therefore please create a new branch for any code that is a work in progress, and name your branch according to the following naming convention: `author_keyword1_keyword2`

* `author`: Please include your initials so that it is clear who is responsible for this branch
* `keyword1`: A first keyword that relates to the project or feature that you are working on
* `keyword2`: A second keyword that relates to the project or feature that you are working on

For example, if Mohammed Osman is working on modelling fortification scenarios in Nigeria, he may choose to name his branch: `MO_fortification_nigeria`

If you would like to learn more about branches, please read the following [documentation](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches)

Once you are happy that the code in your branch is ready to be merged into the main branch, please submit a pull request.

### Submitting pull requests


### Merging pull requests

### Summary

Thank you for contributing to the MIMI project! 
