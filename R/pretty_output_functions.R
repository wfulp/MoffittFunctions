#' Pasting Together Information for Two Groups
#'
#' Paste together information, often statistics, from two groups. There are two predefined combinations: mean(sd) and median[min,max], but user may also paste any single measure together.
#'
#'
#' @param data input dataset. User must use consistent naming throughout, \strong{with an underscore} to separate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be two columns with column names that exactly match the input for \code{first_name} and \code{second_name} (i.e. 'Group1' and 'Group2'), which are used to form the \code{Comparison} variable.
#' @param vars_to_paste vector of names of common measures to paste together. Can be the predefined 'median_min_max' or 'mean_sd', or any variable as long as they have matching columns for each group (i.e. Group1_MyMeasure and Group2_MyMeasure). Multiple measures can be requested. Default: "all" will run 'median_min_max' and 'mean_sd', as well as any pairs of columns in the proper format.
#' @param first_name name of first group (string before '_') . Default is 'Group1'.
#' @param second_name name of second group (string before '_'). Default is 'Group2'.
#' @param sep_val value to be pasted between the two measures. Default is ' vs. '.
#' @param na_str_out the character to replace missing values with.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". Will be used to determine the character to be pasted between the group names (\code{Comparison} variable).  Specifying "two.sided" will use the \code{sep_val} input.
#' @param digits integer indicating the number of decimal places to round to before pasting for numeric variables. Default is 0.
#' @param trailing_zeros logical indicating if trailing zeros should be included (i.e. 0.100 instead of 0.1). Note if set to TRUE output is a character vector.
#' @param keep_all logical indicating if all remaining, unpasted variables in \code{data} should be returned with the pasted variables. Default TRUE.
#' @param verbose a logical variable indicating if warnings and messages should be displayed. Default FALSE.
#' @details
#'
#' User must use consistant naming throughout, with a underscore to separate the group names from the measures (i.e. \code{Group1_mean} and \code{Group2_mean}). There also must be columns defining the group names (i.e. \code{Group1} and \code{Group2}), which are used to form the \code{Comparison} variable.
#'
#' \code{alternative} included as a parameter so the direction can easily be seen in one-sided test. If "two.sided" is selected the value to be pasted between the two group names will be set to \code{sep_val}, where "greater" will use " > " and "less" with use " < " as the pasting value.
#'
#'
#' @return data.frame with all the pasted values requested. Each name will have '_comparison' at the end of the names (i.e. mean_comparison, median_comparison, ...)
#' @examples
#'
#' # Same examples on data.table
#' library(data.table)
#' data(exampleData_BAMA)
#'
#' descriptive_stats_by_group <- exampleData_BAMA[, .(
#'      Group1 = unique(group[group == 1]), Group2 = unique(group[group == 2]),
#'      Group1_n = length(magnitude[group == 1]), Group2_n = length(magnitude[group == 2]),
#'      Group1_mean = mean(magnitude[group == 1]), Group2_mean = mean(magnitude[group == 2]),
#'      Group1_sd = sd(magnitude[group == 1]), Group2_sd = sd(magnitude[group == 2]),
#'      Group1_median = median(magnitude[group == 1]), Group2_median = median(magnitude[group == 2]),
#'      Group1_min = min(magnitude[group == 1]), Group2_min = min(magnitude[group == 2]),
#'      Group1_max = max(magnitude[group == 1]), Group2_max = max(magnitude[group == 2])
#' ), by = .(visitno,antigen)]
#'
#' paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = 'all', 
#'    first_name = 'Group1', second_name = 'Group2', 
#'    sep_val = " vs. ", digits = 0, keep_all = TRUE)
#'
#' paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = c("mean", "median_min_max"), 
#'    alternative= "less", keep_all = FALSE)
#'
#' paste_tbl_grp(data = descriptive_stats_by_group, vars_to_paste = 'all', 
#'    first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", 
#'    alternative = 'less', digits = 5, keep_all = FALSE)
#'
#'
#' # Same example with tidyverse (dplyr+tidyr) with some custom functions
#'
#' library(dplyr)
#' library(tidyr)
#'
#'q95_fun = function(x) quantile(x, 0.95)
#'N = function(x) length(x)
#'
#'exampleData_BAMA %>%
#'  mutate(group = paste0("Group", group)) %>%
#'  group_by(group, visitno, antigen) %>%
#'  summarise_at("magnitude", funs(N, mean, sd, median, min, max, q95_fun)) %>%
#'  gather(variable, value, -(group:antigen)) %>% # these three chains create a wide dataset
#'  unite(temp, group, variable) %>%
#'  spread(temp, value) %>%
#'  mutate(Group1 = "Group 1", Group2 = "Group 2") %>%
#'  paste_tbl_grp()
#'
#' @import data.table
#' @export


paste_tbl_grp <- function(data, vars_to_paste = 'all', first_name = 'Group1', second_name = 'Group2', sep_val = " vs. ", na_str_out = "---", alternative = c("two.sided", "less", "greater"), digits = 0, trailing_zeros = TRUE, keep_all = TRUE, verbose = FALSE){

  #####Checking variables being used

  data_here <- as.data.frame(data)
  alternative <- match.arg(alternative)
  .check_numeric_input(digits, lower_bound = 0, scalar = TRUE)
  if (first_name == second_name) stop('"first_name" and "second_name" must be different')
  if (sum(first_name == names(data_here)) != 1) stop('Expecting one column named "', first_name , '" in input dataset, but there are ', sum(first_name == names(data_here)), ' present')
  if (sum(second_name == names(data_here)) != 1) stop('Expecting one column named "', second_name , '" in input dataset, but there are ', sum(second_name == names(data_here)), ' present')
  if (length(vars_to_paste) != 1 & any(vars_to_paste == 'all')) {
    vars_to_paste = 'all'
    if (verbose) message('Since "all" specified other entries of vars_to_paste ignored.')
  }

  # Defining vars when vars_to_paste set to 'all'
    if (length(vars_to_paste) == 1 & any(vars_to_paste == 'all')) {
      #Need to address if one group name a subset of another
      temp_group1_names <- names(data_here)[(substr(names(data_here), 0, nchar(first_name)) == first_name) &
                                              (nchar(first_name) > nchar(second_name) | substr(names(data_here), 0, nchar(second_name)) != second_name)]
      temp_group2_names <- names(data_here)[(substr(names(data_here), 0, nchar(second_name)) == second_name) &
                                              (nchar(second_name) > nchar(first_name) | substr(names(data_here), 0, nchar(first_name)) != first_name)]
      temp_group1_measures <- gsub(paste0(first_name,'_'), '', temp_group1_names, fixed = TRUE)
      temp_group2_measures <- gsub(paste0(second_name,'_'), '', temp_group2_names, fixed = TRUE)
      vars_to_paste_here <- unique(intersect(temp_group1_measures, temp_group2_measures))

      # Adding speacial cases
      if (sum(vars_to_paste_here %in% c('median','min','max')) == 3) vars_to_paste_here <- c(vars_to_paste_here, 'median_min_max')
      if (sum(vars_to_paste_here %in% c('mean','sd')) == 2) vars_to_paste_here <- c(vars_to_paste_here, 'mean_sd')

      if (verbose) message('The following measures will be combined: ', paste0(vars_to_paste_here, collapse = ', '))
    } else {
      vars_to_paste_here <- unique(vars_to_paste)
    }
    # Giving a message if nothing to return
    if (any(vars_to_paste == 'all') & length(vars_to_paste_here) == 0) {
      if (verbose) message('"all" specified, but no matching columns to paste')
      if (keep_all) return(data) else return(NULL)
    }

  # Need to define which variables to check. Special considerations for the predefined values
  vars_to_check <- vars_to_paste_here[!vars_to_paste_here %in% c('median_min_max','mean_sd')]
  if (any(vars_to_paste_here == 'median_min_max')) vars_to_check <- unique(c(vars_to_check, 'median', 'min', 'max'))
  if (any(vars_to_paste_here == 'mean_sd')) vars_to_check <- unique(c(vars_to_check, 'mean', 'sd'))

  # Need to check the group1 and group2 version of each variable being pasted
  group1_vars_to_check <- paste0(first_name, '_', vars_to_check)
  group2_vars_to_check <- paste0(second_name, '_', vars_to_check)
  for (i in 1:length(vars_to_check)) {
    if (sum(group1_vars_to_check[i] == names(data_here)) != 1) stop('Expecting one column named "', group1_vars_to_check[i] , '" in input dataset, but there are ', sum(group1_vars_to_check[i] == names(data_here)), ' present')
    temp_group1_var <- data_here[, group1_vars_to_check[i] == names(data_here)]
    if (is.numeric(temp_group1_var) & any(!is.na(temp_group1_var))) .check_numeric_input(temp_group1_var)

    if (sum(group2_vars_to_check[i] == names(data_here)) != 1) stop('Expecting one column named "', group2_vars_to_check[i] , '" in input dataset, but there are ', sum(group2_vars_to_check[i] == names(data_here)), ' present')
    temp_group2_var <- data_here[, group2_vars_to_check[i] == names(data_here)]
    if (is.numeric(temp_group2_var) & any(!is.na(temp_group2_var))) .check_numeric_input(temp_group2_var)
  }

  ##### Pasting variables

  # Comparison variable
  comparison_sep <- switch(alternative,
                            two.sided = sep_val,
                            less = ' < ',
                            greater = ' > ')

  comparison_var <- paste0(data_here[, first_name == names(data_here)], comparison_sep, data_here[, second_name == names(data_here)])

  # Other variables
  pasted_results <- list()
  for (i in 1:length(vars_to_paste_here)) {
    if (vars_to_paste_here[i] == 'median_min_max') {
      pasted_results[[i]] <- paste0(
        stat_paste(stat1 = data_here[, paste0(first_name, '_median')],
                   stat2 = data_here[, paste0(first_name, '_min')],
                   stat3 = data_here[, paste0(first_name, '_max')],
                   digits = digits, bound_char = '[', sep = ', ', na_str_out = na_str_out, trailing_zeros = trailing_zeros
        ),
        sep_val,
        stat_paste(stat1 = data_here[, paste0(second_name, '_median')],
                   stat2 = data_here[, paste0(second_name, '_min')],
                   stat3 = data_here[, paste0(second_name, '_max')],
                   digits = digits, bound_char = '[', sep = ', ', na_str_out = na_str_out, trailing_zeros = trailing_zeros
        )
      )
    } else if (vars_to_paste_here[i] == 'mean_sd') {
      pasted_results[[i]] <-  paste0(
        stat_paste(stat1 = data_here[, paste0(first_name, '_mean')],
                   stat2 = data_here[, paste0(first_name, '_sd')],
                   digits = digits, bound_char = '(', na_str_out = na_str_out, trailing_zeros = trailing_zeros
        ),
        sep_val,
        stat_paste(stat1 = data_here[, paste0(second_name, '_mean')],
                   stat2 = data_here[, paste0(second_name, '_sd')],
                   digits = digits, bound_char = '(', na_str_out = na_str_out, trailing_zeros = trailing_zeros
        )
      )
    } else {
      first_var_here <- data_here[, paste0(first_name, '_', vars_to_paste_here[i])]
      second_var_here <- data_here[, paste0(second_name, '_', vars_to_paste_here[i])]
      both_var_here <- c(first_var_here, second_var_here)

      # Want to set digits to 0 if an integer
      if (is.numeric(both_var_here)) {
        if (any((both_var_here %% 1) != 0)) digits_here = digits else digits_here = 0
      } else digits_here = digits

      pasted_results[[i]] <- paste0(ifelse(is.na(first_var_here), na_str_out, .round_if_numeric(first_var_here, digits = digits_here, trailing_zeros = trailing_zeros)),
                                    sep_val,
                                    ifelse(is.na(second_var_here), na_str_out, .round_if_numeric(second_var_here, digits = digits_here, trailing_zeros = trailing_zeros))
      )
    }
  }
  names(pasted_results) <- paste0(vars_to_paste_here, '_comparison')

  pasted_results <- data.frame('Comparison' = comparison_var,pasted_results, stringsAsFactors = FALSE)

  # Returning all data if desired
  if (keep_all) {
    index_to_keep <- !names(data_here) %in% c(first_name, second_name, group1_vars_to_check, group2_vars_to_check)
    data.frame(data_here[, index_to_keep, drop = FALSE], pasted_results, stringsAsFactors = FALSE)
  } else {
    pasted_results
  }

}




#' Rounds and combines up to three numbers into table friendly presentation
#'
#' Takes in up to 3 numeric values, rounds each to a specified digit amount (if numeric), and then combines them accordingly.
#'
#' @param stat1 first statistic to be pasted.
#' @param stat2 second statistic to be pasted (optional).
#' @param stat3 third statistic to be pasted (optional).
#' @param digits positive integer of length 1 between 0 (default) and 14, giving the amount of digits to round to.
#' @param trailing_zeros logical indicating if trailing zeros should included (i.e. 0.100 instead of 0.1). Note is set to TRUE output is a character vector
#' @param bound_char the character to be used between stat1 and stat2/stat3. Available options are '(' (default), '[', '\{', and '|'.
#' @param sep the string to be used between stat2 and stat3. The default is ', '.
#' @param na_str_out the character to replace missing values with.
#' @return string of combined values
#'
#' @details
#'
#' One value provided - returns a rounded value or the missing character.
#' Two values - returns stat1 (stat2). e.g., mean (sd)
#' Three values - returns stat1 (stat2, stat3). e.g., estimate (95\% lower, 95\% upper) or median [min, max]
#'
#' Currently the method does work with string variables, but of course rounding would not be relevant for strings.
#'
#'
#' @examples
#'
#' stat_paste(5.109293)
#' stat_paste(NA)
#' stat_paste(5.109293, 2.145)
#' stat_paste(5.109293, 1.7645, 8.0345)
#' stat_paste(NA, NA, NA)
#' stat_paste(5.109, "p < 0.001", digits = 3)
#' stat_paste(c(rep(5,5),NA),c(1:5,NA),c(1,NA,2,NA,3,NA),bound_char = '[')
#'
#' library(data.table)
#' data(testData_BAMA)
#' testData_BAMA [, .(
#'   median_min_max = stat_paste(
#'      median(magnitude, na.rm = TRUE),
#'      min(magnitude, na.rm = TRUE),
#'      max(magnitude, na.rm = TRUE)
#'      )), by = .(antigen, visit, group)]
#'
#' @export
stat_paste = function(stat1, stat2 = NULL, stat3 = NULL, digits = 0, trailing_zeros = TRUE, bound_char = c('(','[','{','|'), sep = ', ', na_str_out = "---"){
  bound_char <- match.arg(bound_char)
  end_bound_char <-   switch(bound_char,
                      `(` = ')',
                      `[` = ']',
                      `{` = '}',
                      `|` = '|'
  )

  stat1_pasted_obj <-  ifelse(is.na(stat1), na_str_out, as.character(.round_if_numeric(stat1, digits = digits, trailing_zeros = trailing_zeros)))
  if (is.null(stat2)) {
    pasted_output <- stat1_pasted_obj
  } else {
    stat2_pasted_obj <-  ifelse(is.na(stat2), na_str_out, as.character(.round_if_numeric(stat2, digits = digits, trailing_zeros = trailing_zeros)))
    if (is.null(stat3)) {
      pasted_output <- ifelse(is.na(stat1) & is.na(stat2), na_str_out, paste0(stat1_pasted_obj, " ", bound_char, stat2_pasted_obj, end_bound_char))
    } else {
      stat3_pasted_obj <-  ifelse(is.na(stat3), na_str_out, as.character(.round_if_numeric(stat3, digits = digits, trailing_zeros = trailing_zeros)))
      pasted_output <- ifelse(is.na(stat1) & is.na(stat2) & is.na(stat3), na_str_out, paste0(stat1_pasted_obj, " ", bound_char, stat2_pasted_obj, sep, stat3_pasted_obj, end_bound_char))
    }
  }
  pasted_output
}


#' Round and format a vector of p-values
#'
#' pretty_pvalues() takes a vector of p-values, rounds them to a specified digit amount,
#' allows options for emphasizing p-values < the defined significance level, and returns a character for missing.
#'
#' @param pvalues numeric vector of raw p-values to be formatted
#' @param digits number of digits to round to; values with zeros past this number of digits are truncated
#' @param bold TRUE or FALSE: set to TRUE to bold p-values < the defined significance level
#' @param italic TRUE or FALSE: set to TRUE to italicize p-values < the defined significance level
#' @param background highlight color for p-values < the defined significance level. Default = NULL (no highlighting)
#' @param sig_alpha the defined significance level. Default = 0.05
#' @param missing_char character string that will replace missing values from the p-value vector. Default = "---"
#' @param include_p TRUE or FALSE: set to TRUE to print "p = " before each p-value
#' @param trailing_zeros TRUE or FALSE: default = TRUE, p-values are formatted with trailing zeros to the defined number of digits (i.e. 0.100 instead of 0.1 if digits = 3)
#'
#' @return Vector of transformed p-values for table output
#'
#' @details
#'
#' With this function, there are two things to be noted:
#' Since the p-value vector formatting uses \code{cell_spec}, which generates raw HTML or LaTeX code, make sure you remember to put \code{escape = FALSE} into your kable code when generating your table. At the same time, you will need to escape special symbols manually.
#' Additionally, \code{cell_spec} needs a way to know whether you want HTML or LaTeX output. You can specify it locally in the function or globally using \code{options(knitr.table.format = "latex")}. If you don't provide anything, this function will output as HTML by default.
#'
#' @examples
#'
#' pvalue_example = c(1, 0.06, 0.0005, NA, 1e-6)
#'
#' pretty_pvalues(pvalue_example, background = "pink")
#'
#' pretty_pvalues(pvalue_example, digits = 4, missing_char = "missing", bold = TRUE)
#'
#' # How to use pretty_pvalues in reports
#' raw_pvals <- c(0.00000007, .05000001, NaN, NA, 0.783)
#' pretty_pvals <- pretty_pvalues(raw_pvals , digits = 3, background = "green", italic = TRUE, bold = TRUE)
#' kableExtra::kable(pretty_pvals , format = "latex", escape = FALSE, col.names = c("P-values"))
#'
#' @export


pretty_pvalues = function(pvalues, digits = 3, bold = FALSE, italic = FALSE, background = NULL, sig_alpha = 0.05, missing_char = '---', include_p = FALSE, trailing_zeros = TRUE){

  .check_numeric_input(pvalues, lower_bound = 0, upper_bound = 1)
  .check_numeric_input(sig_alpha, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  .check_numeric_input(digits, lower_bound = 1, upper_bound = 14, scalar = TRUE, whole_num = TRUE)

  #Need to set options for no scientific notation, but set back to user preference on exit
  op <- options()
  options(scipen = 10)
  on.exit(options(op))

  lower_cutoff = 10^(-digits)

  ## relevant p-value indices for specific assignments
  missing_p = which(is.na(pvalues))
  below_cutoff_p = which(pvalues < lower_cutoff)
  sig_p = which(pvalues < sig_alpha)

  if (trailing_zeros) pvalues_new = round_away_0(pvalues, trailing_zeros = TRUE, digits = digits) else pvalues_new = as.character(round_away_0(pvalues, trailing_zeros = F, digits))

  ## manipulate and assign pvalues as characters to output pvalue vector
  pvalues_new[missing_p] = missing_char
  pvalues_new[below_cutoff_p] = paste0("<", lower_cutoff)

  # the letter 'p' in front of values
  if (include_p) pvalues_new <- ifelse(pvalues_new < lower_cutoff, paste0('p',  pvalues_new), paste0('p=',  pvalues_new))

  # formatting
  if (bold == TRUE | italic == TRUE | !is.null(background)) pvalues_new[sig_p] = kableExtra::cell_spec(pvalues_new[sig_p], format = "latex", bold = bold, italic = italic, background = background, escape = FALSE)

  pvalues_new
}






#' Fancy Table Output of KM (survfit) Fit
#' 
#' This function takes a Kaplan-Meier model fit object (from survival::survfit) and calculate survival estimates at a specified time, and Median Survival Estimates. This can be performed on an overall KM fit or a fit including a categorical variable (strata).
#'
#' @param fit survfit object (with or without single strata variable)
#' @param time_est numerical vector of time estimates. If NULL (default) no time estimates are calculated
#' @param group_name strata variable name. If NULL and strata exists then using variable
#' @param title_name title to use
#' @param surv_est_prefix prefix to use in survival estimate names. Default is Time (i.e. Time:5, Time:10,...)
#' @param surv_est_digits number of digits to round p values for survival estimates for specified times
#' @param median_est_digits number of digits to round p values for Median Survival Estimates
#' @param latex_output will this table go into a latex output (making special charaters latex friendly)
#' 
#' @details 
#' Currently works with multiple strata in the fit (i.e. \code{survfit(Surv(time, event) ~ x1 + x2)}), although level and \code{Group} column names may be off.
#' 
#' @return
#' A tibble with: \code{Name} (if provided), \code{Group} (if strata variable in fit), \code{Level} (if strata variable in fit), \code{Median Estimate}, \code{Time:X} (Survival estimates for each time provided, if any). In no strata variable tibble is one row, otherwise nrows = number of strata levels.
#' 
#' @examples
#' 
#' # Basic linear model example
#' set.seed(542542522)
#' ybin <- sample(0:1, 100, replace = TRUE)
#' ybin2 <- sample(0:1, 100, replace = TRUE)
#' y <- rexp(100,.1)
#' x1 <- factor(sample(LETTERS[1:2],100,replace = TRUE))
#' x2 <- factor(sample(letters[1:4],100,replace = TRUE))
#' my_fit <- survival::survfit(survival::Surv(y, ybin) ~ 1)
#' my_fit2 <- survival::survfit(survival::Surv(y, ybin) ~ x1)
#' my_fit3 <- survival::survfit(survival::Surv(y, ybin) ~ x2)
#' my_fit_y2 <- survival::survfit(survival::Surv(y, ybin2) ~ 1)
#' 
#' pretty_km_output(fit = my_fit3, time_est = c(5,10), title_name = 'Overall Fit')
#' 
#' library(dplyr) 
#' km_info <- bind_rows(
#'   pretty_km_output(fit = my_fit, time_est = c(5,10), 
#'         group_name = 'Overall', title_name = 'Overall Survival---ybin'),
#'   pretty_km_output(fit = my_fit2, time_est = c(5,10), 
#'         group_name = NULL, title_name = 'Overall Survival---ybin'),
#'   pretty_km_output(fit = my_fit3, time_est = c(5,10), 
#'         group_name = 'x2', title_name = 'Overall Survival---ybin'),
#'   pretty_km_output(fit = my_fit_y2, time_est = c(5,10), 
#'         group_name = 'Overall', title_name = 'Overall Survival---ybin2'),
#' ) %>% select(Name, Group, Level, everything())
#' 
#' library(kableExtra)
#' kable(km_info, escape = F, longtable = F, booktabs = TRUE, linesep = '', 
#'      caption = 'Survival Percentage Estimates at 5 and 10 Years') %>% 
#'   collapse_rows(1:2, row_group_label_position = 'stack', headers_to_remove = 1:2) %>% 
#'   
#'   # Real World Examples
#'   data(Bladder_Cancer)
#'   surv_obj <- survival::Surv(Bladder_Cancer$Survival_Months, Bladder_Cancer$Vital_Status == 'Dead')   
#'   downstage_fit <- survival::survfit(surv_obj ~ PT0N0, data = Bladder_Cancer)
#'   
#'   pretty_km_output(fit = downstage_fit, time_est = c(24, 60), 
#'        surv_est_prefix = 'Month', surv_est_digits = 3)
#'   
#' 
#' @importFrom  dplyr %>%
#' @importFrom tibble tibble
#' 
#' @export

pretty_km_output <- function(fit, time_est = NULL, group_name = NULL, title_name = NULL, surv_est_prefix = 'Time', surv_est_digits = 2, median_est_digits = 1, latex_output =FALSE){
  # Input Checking
  if (!is.null(time_est)) {
    # Want to make sure time_est > 0
    .check_numeric_input(time_est, lower_bound = 1e-50)
    if (any(time_est > max(fit$time, na.rm = TRUE))) 
      stop("At least one value of 'time_est' estimate is higher than the largest time_est value in 'fit' object.")
  } else {
    # if no time_est requested, setting time_est to 0 and then removing later, to make coding much easier.
    time_est = 0
  }
  .check_numeric_input(surv_est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(median_est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  
  # If group name not specified but using strata, will use var name
  if (is.null(group_name) && !is.null(fit$strata)) 
    group_name <- gsub('_', ' ', substr(names(fit$strata)[1], 1, regexpr('=', names(fit$strata)[1]) - 1))
  
  # Getting specific time_est estimates
  tmp_summary <- summary(fit, time = time_est, extend = TRUE)
  tmp_surv_est <-  stat_paste(tmp_summary$surv, tmp_summary$lower, tmp_summary$upper, 
                              digits = surv_est_digits, trailing_zeros = TRUE,  bound_char = '(', na_str_out = 'N.E.')
  if (latex_output) tmp_surv_est <- gsub('\\%','\\\\%', tmp_surv_est)

  if (!is.null(fit$strata)) {
    tmp_strata_levels <- substr(tmp_summary$strata, regexpr('=', tmp_summary$strata) + 1, nchar(as.vector(tmp_summary$strata)))
    tmp_all_levels <- tibble::tibble(Level = substr(names(fit$strata), regexpr('=',  names(fit$strata)) + 1, nchar( names(fit$strata))))
    tmp_med_info <- stat_paste(tmp_summary$table[,'median'], tmp_summary$table[,paste0(fit$conf.int,'LCL')], tmp_summary$table[,paste0(fit$conf.int,'UCL')], 
                               digits = median_est_digits, trailing_zeros = TRUE,  bound_char = '(', na_str_out = 'N.E.')
    n_events <- tmp_summary$table[,'events']
  } else {
    tmp_strata_levels <- NULL
    tmp_med_info <-  stat_paste(tmp_summary$table['median'], tmp_summary$table[paste0(fit$conf.int,'LCL')], tmp_summary$table[paste0(fit$conf.int,'UCL')], 
                                digits = median_est_digits, trailing_zeros = TRUE,  bound_char = '(', na_str_out = 'N.E.')
    n_events <- tmp_summary$table['events']
  }
  tmp_surv_est_info_long <- dplyr::bind_cols(Level = tmp_strata_levels, 
                                     Time = tmp_summary$time, Est = tmp_surv_est)
  names(tmp_surv_est_info_long)[ names(tmp_surv_est_info_long) == 'Time'] = surv_est_prefix

  tmp_surv_est_info <- tmp_surv_est_info_long %>% tidyr::spread_(surv_est_prefix, 'Est', sep = ':') 
  
  # Need to merge in time est with levels, in case entire strata missing. Then replace missing with N.E.
  if (!is.null(fit$strata))  tmp_surv_est_info <- dplyr::full_join(tmp_all_levels, tmp_surv_est_info, by = 'Level') 
  tmp_surv_est_info <- tmp_surv_est_info %>% replace(., is.na(.), 'N.E.')
  
  tmp_med_info <- gsub('NA', 'N.E.', tmp_med_info)
  
  # Stripping some unnecessary attributes
  attr(tmp_med_info, 'names') <- NULL
  attr(n_events, 'names') <- NULL
  
  tmp_output <- dplyr::bind_cols(Group = rep(group_name,length(tmp_med_info)),
                          N = fit$n, `N Events` = n_events,
                          tmp_surv_est_info,
                          `Median Estimate` = tmp_med_info) %>% 
    # putting time variables at end
    dplyr::select(-dplyr::contains(paste0(surv_est_prefix, ':')), dplyr::everything(), dplyr::contains(paste0(surv_est_prefix, ':')))
  
  # Dropping if time = 0 (i.e. no time given)
  if (all(time_est == 0))  tmp_output <- tmp_output %>% dplyr::select(-dplyr::contains(surv_est_prefix))
  
  # Reorder if strata
  if (!is.null(fit$strata))  tmp_output <- tmp_output %>% dplyr::select(Group, Level, dplyr::everything())
  
  # Adding Title in front, if given
  if (!is.null(title_name)) dplyr::bind_cols(Name = rep(title_name,length(tmp_med_info)), tmp_output) else tmp_output
}





#' Wrapper for KM Model Output, with Log-Rank p value
#' 
#' This function takes a dataset, along with variables names for time and event status for KM fit, and possibly strata
#'
#' @param strata_in name of strata variable, or NA (default) if no strata desired
#' @param model_data dataset that contains \code{strata_in}, \code{time_in}, and \code{event_in} variables
#' @param time_in name of time variable component of outcome measure
#' @param event_in name of T/F event stauts or expression resulting in T/F scalor (i.e. "Vital_Status == 'Dead'") for the name of event variable component of outcome measure. TRUE represents event (i.e. Death)
#' @param time_est numerical vector of time estimates. If NULL (default) no time estimates are calculated
#' @param group_name strata variable name. If NULL and strata exists then using variable
#' @param title_name title to use
#' @param conf_level the confidence level required (default is 0.95).
#' @param surv_est_prefix prefix to use in survival estimate names. Default is Time (i.e. Time:5, Time:10,...)
#' @param surv_est_digits number of digits to round p values for survival estimates for specified times
#' @param median_est_digits number of digits to round p values for Median Survival Estimates
#' @param p_digits number of digits to round p values for Log-Rank p value
#' @param latex_output will this table go into a latex output (making special charaters latex friendly)
#' @param sig_alpha the defined significance level. Default = 0.05
#' @param background background color of significant values, or no highlighting if NULL. Default is "yellow"
#' @param ... other params to pass to \code{pretty_pvalues} (i.e. \code{bold} or \code{italic})
#
#'  
#' @return
#' A tibble with: \code{Name} (if provided), \code{Group} (if strata variable in fit), \code{Level} (if strata variable in fit), \code{Time:X} (Survival estimates for each time provided), \code{Median Estimate}. In no strata variable tibble is one row, otherwise nrows = number of strata levels.
#' 
#' @examples
#' 
#' # Basic linear model example
#' set.seed(542542522)
#' ybin <- sample(0:1, 100, replace = TRUE)
#' ybin2 <- sample(0:1, 100, replace = TRUE)
#' y <- rexp(100,.1)
#' x1 <- factor(sample(LETTERS[1:2],100,replace = TRUE))
#' x2 <- factor(sample(letters[1:4],100,replace = TRUE))
#' my_data <- data.frame(y, ybin,ybin2, x1, x2)
#' Hmisc::label(my_data$x1) <- "X1 Variable"
#' 
#'  # Single runs 
#' run_pretty_km_output(strata_in = 'x1', model_data = my_data, 
#'      time_in = 'y', event_in = 'ybin == 1', time_est = NULL)
#' run_pretty_km_output(strata_in = 'x1', model_data = my_data, 
#'      time_in = 'y', event_in = 'ybin == 1', time_est = c(5,10))
#' 
#' # Multiple runs for different variables
#' library(dplyr) 
#' vars_to_run = c(NA, 'x1', 'x2')
#' purrr::map_dfr(vars_to_run, run_pretty_km_output, model_data = my_data,
#'      time_in = 'y', event_in = 'ybin == 1', time_est = NULL) %>% 
#'    select(Group, Level, everything())
#'    
#' km_info <- purrr::map_dfr(vars_to_run, run_pretty_km_output, model_data = my_data, time_in = 'y', 
#'      event_in = 'ybin == 1', time_est = c(5,10), surv_est_prefix = 'Year', 
#'      title_name = 'Overall Survival') %>% 
#'    select(Group, Level, everything())
#'    
#' km_info2 <- purrr::map_dfr(vars_to_run, run_pretty_km_output, model_data = my_data, time_in = 'y', 
#'      event_in = 'ybin2 == 1', time_est = c(5,10), surv_est_prefix = 'Year', 
#'      title_name = 'Cancer Specific Survival') %>% 
#'    select(Group, Level, everything())
#' 
#' library(kableExtra)
#' options(knitr.kable.NA = '')
#' kable(bind_rows(km_info, km_info2), escape = F, longtable = F, booktabs = TRUE, linesep = '', 
#'      caption = 'Survival Percentage Estimates at 5 and 10 Years') %>%
#'   collapse_rows(c(1:2), row_group_label_position = 'stack', headers_to_remove = 1:2) 
#' 
#' 
#'   # Real World Example
#'   data(Bladder_Cancer)
#'   
#'   vars_to_run = c(NA, 'Gender', 'Clinical_Stage_Grouped', 'PT0N0', 'Any_Downstaging')
#'   
#'   purrr::map_dfr(vars_to_run, run_pretty_km_output, model_data = Bladder_Cancer, 
#'        time_in = 'Survival_Months', event_in = 'Vital_Status == "Dead"', time_est = c(24,60), 
#'        surv_est_prefix = 'Month', p_digits=5) %>% 
#'    select(Group, Level, everything())
#'   
#' @importFrom  Hmisc label
#' 
#' @export
#' 
run_pretty_km_output <- function(strata_in = NA, model_data, time_in, event_in, time_est = NULL, group_name = NULL, title_name = NULL, conf_level = .95, surv_est_prefix = 'Time', surv_est_digits = 2, median_est_digits = 1, p_digits = 4, latex_output = FALSE, sig_alpha = 0.05, background = 'yellow', ...) {
  if (length(strata_in) != 1) stop('"strata_in" must be length of 1')
  .check_numeric_input(surv_est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(median_est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(p_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(sig_alpha, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  .check_numeric_input(conf_level, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  if (all(time_in != colnames(model_data)))
    stop('"time_in" must be in the "model_data" dataset')

  if (is.na(strata_in)) {
    if (is.null(group_name)) group_name <- 'Overall'
    tmp_formula <- as.formula(paste("survival::Surv(",time_in,",",event_in,") ~ 1"))
  } else {
    if (!all(na.omit(strata_in) %in% colnames(model_data)))
      stop('All variables used in the "strata_in" must be in the "model_data" dataset')
    if (is.null(group_name)) {
      #Using label if possible
      if (Hmisc::label(model_data[,strata_in]) != '') 
        group_name <- Hmisc::label(model_data[,strata_in]) 
      else group_name <- gsub('_', ' ', strata_in)
    }
    tmp_formula <- as.formula(paste("survival::Surv(",time_in,",",event_in,") ~ ", strata_in))
    tmp_pval <-  pchisq(survival::survdiff(formula = tmp_formula, data = model_data)$chi, 
                        length(survival::survdiff(formula = tmp_formula, data = model_data)$n) - 1, 
                        lower.tail = FALSE)
    if (latex_output) {
      tmp_p_info <- pretty_pvalues(tmp_pval, digits = p_digits, sig_alpha = sig_alpha, trailing_zeros = TRUE, background = background, ...)
    } else {
      tmp_p_info <- pretty_pvalues(tmp_pval, digits = p_digits, trailing_zeros = TRUE)
    }
  }
  
  tmp_fit <- survival::survfit(tmp_formula, model_data, conf.int = conf_level)
  tmp_km_output <- pretty_km_output(fit = tmp_fit, time_est = time_est, group_name = group_name, title_name = title_name, surv_est_prefix = surv_est_prefix, surv_est_digits = surv_est_digits, median_est_digits = median_est_digits, latex_output = latex_output)
  
  if (is.na(strata_in)) 
    tmp_km_output else 
      dplyr::bind_cols(tmp_km_output, `Log-Rank P` = c(tmp_p_info, rep('', nrow(tmp_km_output) - 1)))
  
 }