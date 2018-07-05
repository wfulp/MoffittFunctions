Moffitt code review guidelines
======================================================

This document goes over basic guidelines and rules for pull requests and code review. More information on the technical details for [pull requests are available here](https://gitlab.moffitt.usf.edu:8000/ReproducibleResearch/MoffittFunctions/blob/master/pull-request-information.md).

Currently, these guidelines are for contributions to packages (e.g., `MoffittFunctions`) or existing macros that are copied from previous projects. The framework for PT reports is still being developed.

# The programming workflow

Having a standardized work flow and good programming habits will make pull requests (PR) a better experence for everyone. If you are blocked or need help, ask someone!


### 1. Use branches

Use branches to edit code files in a repo before pushing it to the `master`. For existing macros that are not centralized, copy them from their most recent source, then commit and push them to your repository before editing them. If you edit them, do that on a [separate branch](https://gitlab.moffitt.usf.edu:8000/ReproducibleResearch/MoffittFunctions/blob/master/pull-request-information.md) and do a PR before pushing edits to the master.

### 2. Use readable code formatting (style).

It is not necessary that everyone adhere to a strict style guide, but some form of style guide should be followed. There are utilities for every programming language called *linters* that will check syntax for style. In R, there is a linter packaged called `lintr` available on CRAN. It works with `R`, `Rmd` and `Rnw` files.

It works in RStudio by running the following command (once you install the package):

```
lintr::lint("report.Rmd")
```

This should bring up a *Markers* tab with style warnings by line number. 

**RStudio will also automatically format your code for you: highlight a line of code and hit ctrl-shift-A.**

For more information on style recommendations used by `lintr`: [here is Hadley Wickham's style guide](http://r-pkgs.had.co.nz/style.html).

### 3. Use good coding habits. Use comments!

This pertains to styles that won't be caught by `lint`, but are covered in [ Hadley Wickham's style guide](http://r-pkgs.had.co.nz/style.html). There are many topics that could be covered here, but three major areas to focus on:

1. names: object names should convey information. Avoid names like `a`, `aa`, or `data`. Be careful not to overload, for example `data` is a function in base R.   
2. overwriting variables and data: new objects, variables, or subsets should get new names. 
3. comments: not every line requires a comment, but comments are invaluable to your reviewer. If you have a long `dplyr` chain or a multi-task `data.table` call, outline the steps. 

```
Comments should explain the why, not the what.
-Hadley
```

### 4. Make digestible pull requests.

There is a balance between a trivial and an overly burdensome PR. Consider linking a PR to a completed task or feature (e.g., finishing a function). If a function required multiple smaller functions, think about whether several PRs would solicit better review. A general rule of thumb is to keep your PR less than 300 lines. 

### 5. Contextualize your PR and use good commit messages.

Commit messages are very important, particularly if there were edits made to previous code. 

Useful commit messages:     
-REVISION: Add diagnostic plots for new model     
-BUGFIX: fix typo input parameters

Not useful commit messages:     
-updated     
-minor

Commit often. If you are using branches and pull requests, you do not have to worry about clogging up the master.

GitLab allows you to title and make a comment about a PR, use this to contextualize the request for the reviewer. This is particularly important if someone isn't not entirely familiar with the goal of a project.

### 6. Share that you have a PR to the group.

Use Slack or bring it up at stand up.

### 7. Once the PR is accepted, merge and continue your work.

