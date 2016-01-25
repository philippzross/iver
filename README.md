iver
=====

[Phil's](http://pzross.com) personal R functions

I don't currently see these functions as having any utility to anyone aside from myself so they are not officially submitted to CRAN but you should be able to install them using devtools.

## Installation

Installation should be straight forward. First, install the dependencies, then run the commands to install the package directly from Github.

**Dependencies:**

* [servr](https://github.com/yihui/servr)
* [knitcitations](https://github.com/cboettig/knitcitations)

**Commands:**

1. devtools::install_github("thephilross/iver")

## Creating New Knitr-Jekyll Lab Notebook

I prefer to manage my projects in RStudio and write as much of my code in R as possible as it offers great integration with Git and Github and a variety of tools to document your code, workflow, and results. Even if you program a lot in another language you can document your project easily using RStudio, rmarkdown, knitr, git, and github. To add to that, I use Jekyll, a static site generator written in Ruby, to statically generate HTML pages of my project to create an electronic lab notebook that I can serve locally and/or for free using [Github Pages](https://pages.github.com/) if I want to share it.

Starting a new project is simple:

1. Use RStudio to create a new project
2. Choose "Version Control" to checkout a project from a version control repository
3. Use the following URL to set up the notebook template: https://github.com/thephilross/nbtemplate
4. Open the shell and run `git remote remove origin` to remove the link between your new project and the template project repo
