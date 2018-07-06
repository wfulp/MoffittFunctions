If you are a performing a pull request on a package, you should be test, build, and load that version (from the working branch) of the package.

## Retrieving the package from the pull request branch

Switch to the branch with the pull request and the pull the most recent version:
```
git checkout [pr_branch]
git pull
```

This can also be done with the RStudio interface under the Git tab: toggle from the master to the pull request branch and then select Pull.

## Switch to the working directory of the package

There are many ways to do this:
- `setwd('path/to/package/')`
- RStudio shortcut: Ctrl + Shift + H
- RStudio drop-down menu: Session -> Set working directory -> ...
- Opening the .proj file if there is one

## Build the package

The following are two options using RStudio:
- ctrl-shift-B or cmd-shift-B
- Drop-down menu: Build -> Install and restart 

This will build the package and load it in the environment.

More advanced:
- 1. In R (builds documentation): `devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))`
- 2. At command line: `R CMD INSTALL --no-multiarch --with-keep.source VISCfunctions`

## Test the package

This runs `testthat` on the tests that were included in the package development. 

The following are three equivalent options:
- Using RStudio: ctrl-shift-T or cmd-shift-T
- Using RStudio drop-down menu: Build -> Test package 
- Using R: `devtools::test()`
