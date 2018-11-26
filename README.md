MoffittFunctions
=============

A collection of Moffitt functions for annotation, statistical calculations, data manipulation, etc.

## Installation

First make sure you have SSH keys set up, then type:

```r
# If installing from GitHub
remotes::install_git("https://github.com/wfulp/MoffittFunctions", build_opts = NULL)

cred = git2r::cred_ssh_key(
    publickey = "MYPATH/.ssh/id_rsa.pub", 
    privatekey = "MYPATH/.ssh/id_rsa")

# If installing from Moffitt GitLab
devtools::install_git("git@gitlab.moffitt.usf.edu:ReproducibleResearch/MoffittFunctions.git", 
                      credentials = cred, build_opts = c("--no-resave-data"))

```
