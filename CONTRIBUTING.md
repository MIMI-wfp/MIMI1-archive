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

To start a pull request. Firstly "push" the branch that you have been working on. Now go to the MIMI GitHub repository, you should see your branch appear at the top of the page next to a green button labelled "Compare & pull request".

<p align="center">
<img width="700" alt="Screenshot 2024-02-19 at 15 11 33" src="https://github.com/kmtang/MIMI/assets/90572354/3790d37e-21ff-4126-aa3b-44609b028937">
</p>

Click this button, and you will be taken to a page where you can "open a pull request". Here you'll be asked to add a title and description for your pull request. You will also be given the opportunity to "Request a review". You should do this whenever you are making contributions to a project that somebody else is working on, by requesting a review from that person, they will be given an opportunity to review the changes and contributions that you have made before merging them into the main branch.

![Screenshot 2024-02-19 at 15 31 52](https://github.com/kmtang/MIMI/assets/90572354/458c1d2a-9b18-4fd5-87f5-dae44a0aa50d)

Once you are happy with everything, click "Create pull request"

### Merging pull requests

If you are the sole contributor to the project you have submitted the pull request for, you can go ahead and merge your own pull request (and resolve any merge conflicts that may have arisen). However, if you have requested a review, you will need to wait for the reviewer to approve the pull request and perform the merge.

If you are a reviewer, you will find your review requests in the "pull requests" tab in the repository. Click on the request, and click "add your review".

<img width="1238" alt="pull_request" src="https://github.com/kmtang/MIMI/assets/90572354/edb614d9-d047-4c6f-9716-f48311f3c01d">


You will then be taken to a page where you are able to review the changes made, if you are happy with the changes made, you can click "Approve" and "Submit review". Otherwise you can submit feedback or request changes from your collaborator if required.

![code_review](https://github.com/kmtang/MIMI/assets/90572354/dfc752dc-3745-4b27-b64b-4294023d5a40)

If any merge conflicts do arise, it is advised that you arrange a meeting or short video call with to resolve conflicts with your collaborator (unless the conflicts are due to inconsequential differences such as empty spaces or comments).

Once all changes are approved, you may now merge the pull request into the main branch!

![merge](https://github.com/kmtang/MIMI/assets/90572354/7b31efca-f708-48fb-a3a9-04dcc1bf20b4)


### Summary

Thank you for contributing to the MIMI project! 
