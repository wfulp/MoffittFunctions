MoffittFunctions
=============


MoffittFunctions is a collection of useful functions designed to assist in analysis and creation of professional reports. The current MoffittFunctions functions can be broken down to the following sections:

- Testing Functions
  - two_samp_bin_test
  - two_samp_cont_test
  - cor_test
- Fancy Output Functions
  - pretty_pvalues
  - pretty_pvalues_compareGroups
  - stat_paste
  - paste_tbl_grp
  - pretty_model_output
  - run_pretty_model_output
  - pretty_km_output
  - run_pretty_km_output
- Utility Functions
  - round_away_0
  - get_session_info
  - get_full_name
- Example Dataset
  - Bladder_Cancer


### Getting Started

```r
# Installing from GitHub
remotes::install_git("https://github.com/wfulp/MoffittFunctions")

# Loading MoffittFunctions and Example Dataset
library(MoffittFunctions)
data("Bladder_Cancer")
```

#### MoffittTemplates Package

The [MoffittTemplates](https://github.com/wfulp/MoffittTemplates) package makes extensive use of the `MoffittFunctions` package, and is a great way get started making professional statistical reports.

