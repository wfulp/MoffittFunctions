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
#' pretty_pvals <- pretty_pvalues(raw_pvals , digits = 3, 
#'    background = "green", italic = TRUE, bold = TRUE)
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




#' Fancy Table Output of Linear, Logistic, and Cox Models
#' 
#' pretty_model_output() takes a Linear, Logistic, and Cox model fit object and calculate estimates, odds ratios, or hazard ratios, respectively, with confidence intervals. P values are also produced. For categorical variables with 3+ levels overall Type 3 p values are calculated, in addition to p values comparing to the first level (reference).
#'
#' @param fit lm, glm, or coxph fit (currently only tested on logistic glm fit)
#' @param model_data data.frame or tibble  used to create model fits. Used for capturing variable labels, if they exist
#' @param title_name title to use (will be repeated in first column)
#' @param conf_level the confidence level required (default is 0.95).
#' @param overall_p_test_stat "Wald" (default) or "LR"; the test.statistic to pass through to the test.statistic param in car::Anova. Ignored for lm fits.
#' @param est_digits number of digits to round OR or HR to (default is 3)
#' @param p_digits number of digits to round p values (default is 4)
#' @param latex_output will this table go into a latex output (making special charaters latex friendly)
#' @param sig_alpha the defined significance level for highlighting. Default = 0.05 (Only used if latex_output = TRUE)
#' @param background background color of significant values, or no highlighting if NULL. Default is "yellow" (Only used if latex_output = TRUE)
#' @param ... other params to pass to \code{pretty_pvalues} (i.e. \code{bold} or \code{italic}) (Only used if latex_output = TRUE)
#' 
#' @details 
#' 
#' Model type is determined by \code{fit} class, and also family if glm class. If the class is glm and  binomial or quasibinomial family, then the output is designed for a Logistic model (i.e. Odd Ratios), if the class is coxph the output is designed for a Cox model (i.e. Harzard Ratios), otherwise the output is designed for a linear model or other model where normal coefficient estimates are displayed.
#'
#' @return
#' 
#' A tibble with: \code{Name} (if provided), \code{Variable}, \code{Level}, \code{Est/OR/HR (95\% CI)}, \code{P Value} (for categorical variables comparing to reference), \code{Overall P Value} (for categorical variables with 3+ levels). 
#'   
#' @examples
#' 
#' # Basic linear model example
#' set.seed(542542522)
#' ybin <- sample(0:1, 100, replace = TRUE)
#' y <- rexp(100,.1)
#' x1 <- rnorm(100)
#' x2 <- y + rnorm(100)
#' x3 <- factor(sample(letters[1:4],100,replace = TRUE))
#' my_data <- data.frame(y, ybin, x1, x2, x3)
#' 
#' # Linear Regression
#' my_fit <- lm(y ~ x1 + x2 + x3, data = my_data)
#' pretty_model_output(fit = my_fit, model_data = my_data)
#' 
#' # Logistic Regression
#' my_fit <- glm(ybin ~ x1 + x2 + x3, data = my_data, family = binomial(link = "logit"))
#' pretty_model_output(fit = my_fit, model_data = my_data)
#' 
#' # Coxph Regression
#' my_fit <- survival::coxph(survival::Surv(y, ybin) ~ x1 + x2 + x3, data = my_data)
#' my_pretty_model_output <- pretty_model_output(fit = my_fit, model_data = my_data)
#' 
#' # Printing of Fancy table in HTML
#' library(dplyr)
#' kableExtra::kable(my_pretty_model_output, 'html', caption = 'My Table') %>% 
#'    kableExtra::collapse_rows(c(1:2), row_group_label_position = 'stack')
#'   
#' # Real World Examples
#' data(Bladder_Cancer)
#' surv_obj <- survival::Surv(Bladder_Cancer$Survival_Months, Bladder_Cancer$Vital_Status == 'Dead')  
#' my_fit <- survival::coxph(surv_obj ~ Gender + Clinical_Stage_Grouped + PT0N0, data = Bladder_Cancer)
#' my_output <- pretty_model_output(fit = my_fit, model_data = Bladder_Cancer)
#' kableExtra::kable(my_output, 'html') %>% 
#'     kableExtra::collapse_rows(c(1:2), row_group_label_position = 'stack')
#'   
#' @import car
#' @importFrom Hmisc label label<-
#' 
#' @export


pretty_model_output <- function(fit, model_data, overall_p_test_stat = c('Wald', 'LR'), title_name = NULL, conf_level = 0.95, est_digits = 3, p_digits = 4, latex_output =FALSE, sig_alpha = 0.05, background = 'yellow', ...) {
  overall_p_test_stat <- match.arg(overall_p_test_stat)
  .check_numeric_input(est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(p_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(sig_alpha, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  .check_numeric_input(conf_level, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  
  if (any(class(fit) == 'glm') && fit$family$family %in% c('binomial', 'quasibinomial')) {
    # Logistic Regression
    exp_output <- TRUE
    est_name <- 'OR'
  } else if (any(class(fit) == 'coxph')) {
    # Coxph Regression
    exp_output <- TRUE
    est_name <- 'HR'
  } else {
    # Not Logistic Regression or Coxph
    exp_output <- FALSE
    est_name <- 'Est'
  } 

  #Using Variable labels for output, is no label using variable name
  var_names <- all.vars(fit$terms)[-1]
  if (!all(var_names %in% colnames(model_data)))
    stop('All variables used in the "fit" must be in the "model_data" dataset')
  var_labels <- Hmisc::label(model_data)[var_names]
  if (any(var_labels == ''))
    var_labels[var_labels == ''] <- gsub('_', ' ', var_names[var_labels == ''])
  
  
  neat_fit = fit %>% broom::tidy(conf.int = TRUE, exponentiate = exp_output, conf.level = conf_level) 
  
  if (latex_output) {
    # P value highlighting if using for pdf output (latex)
    neat_fit$p.label = pretty_pvalues(neat_fit$p.value, digits = p_digits, trailing_zeros = TRUE, sig_alpha = sig_alpha, background = background, ...) 
  } else {
    neat_fit$p.label = pretty_pvalues(neat_fit$p.value, digits = p_digits, trailing_zeros = TRUE)
  }
  
  neat_fit <- neat_fit %>%
    dplyr::select(variable = term, est = estimate, p.label, p.value, conf.low, conf.high) %>%
    dplyr::filter(variable != "(Intercept)")
  
  
  if (length(fit$xlevels) > 0) {
    all_levels = fit$xlevels %>% tibble::enframe() %>% tidyr::unnest() %>%
      dplyr::mutate(variable = paste0(name, value))
    
    neat_fit <- dplyr::full_join(all_levels, neat_fit, by = "variable") %>%
      dplyr::mutate(
        name = ifelse(is.na(name), variable, name),
        value = ifelse(is.na(value), '', value)
      )
  } else {
    neat_fit <- neat_fit %>%
      dplyr::mutate(
        name = variable,
        value = ''
      )
  }
  
  neat_fit <- neat_fit %>%
    dplyr::mutate(
      est.label = ifelse(is.na(est), "1.0 (Reference)",
                         stat_paste(est, conf.low, conf.high, digits = est_digits, trailing_zeros = TRUE)),
      p.label = ifelse(is.na(p.label), ifelse(latex_output, '---', '-'), p.label)
    ) %>%
    dplyr::select(name, Level = value, Est_CI = est.label, `P Value` = p.label) %>%
    dplyr::arrange(factor(name, levels = var_names)) %>% 
    dplyr::rename(!!paste0(est_name, paste0(' (', round_away_0(conf_level, 2) * 100, ifelse(latex_output, '\\', '')), '% CI)') := Est_CI)
  
  # Dropping extra variable names (for overall p merging)
  neat_fit <- neat_fit %>%
   dplyr::mutate(name_sub = ifelse(duplicated(name), '', name),
          Variable = var_labels[match(name,var_names)])
  
  ## Type III Overall variable tests
  
  # Getting which vars we need overall tests for
  overall_vars_needed <- neat_fit %>% dplyr::group_by(name) %>% dplyr::summarise(run_var = n() > 2)
  
  if (any(overall_vars_needed$run_var)) {
    type3_tests <- dplyr::full_join(broom::tidy(car::Anova(fit, type = 'III', test.statistic = overall_p_test_stat)),
                                    overall_vars_needed, by = c('term' = 'name'))
    
    if (latex_output) {
      # P value highlighting if using for pdf output (latex)
      type3_tests$overall.p.label = pretty_pvalues(type3_tests$p.value, digits = p_digits, 
                                                   trailing_zeros = TRUE, sig_alpha = sig_alpha, background = background, ...) 
    } else {
      type3_tests$overall.p.label = pretty_pvalues(type3_tests$p.value, digits = p_digits, trailing_zeros = TRUE)
    }
    
    type3_tests <- type3_tests %>% 
      dplyr::filter(term != "(Intercept)" & run_var) %>%
      dplyr::select(variable = term, `Overall P Value` = overall.p.label)
    
    neat_fit <- dplyr::full_join(neat_fit, type3_tests, by = c("name_sub" = "variable")) %>%
      dplyr::mutate(`Overall P Value` = ifelse(is.na(`Overall P Value`), '', `Overall P Value`))
  } else {
    neat_fit <- neat_fit %>% dplyr::mutate(`Overall P Value` = '')
  }
  
  neat_fit <- neat_fit %>% dplyr::select(Variable, Level, dplyr::contains('CI'), dplyr::contains('P Value'))
 
  # Adding Title in front, if given
  if (!is.null(title_name)) dplyr::bind_cols(Name = rep(title_name,nrow(neat_fit)), neat_fit) else neat_fit
  
}




#' Wrapper for Pretty Model Output
#' 
#' Wrapper for pretty_model_output(). This function takes a dataset, along with variables names for x (could be multiple), y, and possibly event status, for model fit.
#'
#' @param x_in name of x variables in model (can be vector of x names)
#' @param model_data data.frame or tibble that contains \code{x_in}, \code{time_in}, and \code{event_in} variables
#' @param y_in name of outcome measure for logistic and linear model, or name of time component in cox model
#' @param event_in name of event status variable. Shouled be left NULL for logistic and linear models. If \code{event_level} = NULL then this must be the name of a F/T or 0/1 variable, where F or 0 are considered the censored level, respectively.
#' @param event_level outcome variable event level for logistic model, and event status level for cox model.
#' @param title_name title to use (will be repeated in first column)
#' @param fail_if_warning Should program stop and give useful message if there is a warning message when running model (Default is TRUE)
#' @param conf_level the confidence level required (default is 0.95).
#' @param overall_p_test_stat "Wald" (default) or "LR"; the test.statistic to pass through to the test.statistic param in car::Anova. Ignored for lm fits.
#' @param est_digits number of digits to round OR or HR to (default is 3)
#' @param p_digits number of digits to round p values (default is 4)
#' @param latex_output will this table go into a latex output (making special charaters latex friendly)
#' @param sig_alpha the defined significance level for highlighting. Default = 0.05 (Only used if latex_output = TRUE)
#' @param background background color of significant values, or no highlighting if NULL. Default is "yellow" (Only used if latex_output = TRUE)
#' @param verbose a logical variable indicating if warnings and messages should be displayed. Default FALSE.
#' @param ... other params to pass to \code{pretty_pvalues} (i.e. \code{bold} or \code{italic})
#
#' 
#' @details 
#' \code{x_in} can be single variable name, or vector of variables to include in the model. All variables must be present in the \code{model_data} dataset.
#' 
#' \code{fail_if_warning} variable default to TRUE because most warnings should be addressed, such as the "Loglik converged before variable XX; beta may be infinite" warning.
#' 
#' @return
#' 
#' A tibble with: \code{Name} (if provided), \code{Variable}, \code{Level}, \code{Est/OR/HR (95\% CI)}, \code{P Value} (for categorical variables comparing to reference), \code{Overall P Value} (for categorical variables with 3+ levels), \code{n/n (event)}. 
#' 
#' @examples
#' 
#' # Basic linear model example
#' set.seed(542542522)
#' ybin <- sample(0:1, 100, replace = TRUE)
#' ybin2 <- sample(c('Male','Female'), 100, replace = TRUE)
#' ybin3 <- sample(c('Dead','Alive'), 100, replace = TRUE)
#' y <- rexp(100,.1)
#' x1 <- factor(sample(LETTERS[1:2],100,replace = TRUE))
#' x2 <- factor(sample(letters[1:4],100,replace = TRUE))
#' my_data <- data.frame(y, ybin, ybin2, ybin3, x1, x2)
#' Hmisc::label(my_data$x1) <- "X1 Variable"
#' 
#'  # Single runs 
#' run_pretty_model_output(x_in = 'x1', model_data = my_data, y_in = 'y', event_in = 'ybin')
#' run_pretty_model_output(x_in = 'x1', model_data = my_data, y_in = 'y', 
#'      event_in = 'ybin3', event_level = 'Dead')
#' run_pretty_model_output(x_in = c('x1','x2'), model_data = my_data, y_in = 'y', event_in = 'ybin')
#' run_pretty_model_output(x_in = 'x2', model_data = my_data, y_in = 'ybin', event_in = NULL, verbose = T)
#' run_pretty_model_output(x_in = 'x2', model_data = my_data, y_in = 'y', event_in = NULL)
#' 
#' # Multiple runs for different variables
#' library(dplyr) 
#' vars_to_run = c('x1', 'x2')
#' cox_models <- purrr::map_dfr(vars_to_run, run_pretty_model_output, model_data = my_data, 
#'      y_in = 'y', event_in = 'ybin')
#' 
#' kableExtra::kable(cox_models, 'html', caption = 'My Table') %>% 
#'   kableExtra::collapse_rows(c(1:2), row_group_label_position = 'stack', headers_to_remove = 1:2)
#' 
#' # Real World Example
#' data(Bladder_Cancer)
#' vars_to_run = c('Gender', 'Clinical_Stage_Grouped', 'PT0N0', 'Any_Downstaging')
#' 
#' univariate_output <- purrr::map_dfr(vars_to_run, run_pretty_model_output, model_data = Bladder_Cancer, 
#'       y_in = 'Survival_Months', event_in = 'Vital_Status', event_level = 'Dead')
#' kableExtra::kable(univariate_output, 'html') %>% 
#'       kableExtra::collapse_rows(c(1:2), row_group_label_position = 'stack', headers_to_remove = 1:2)
#' 
#' multivariable_output <- run_pretty_model_output(vars_to_run, model_data = Bladder_Cancer, 
#'       y_in = 'Survival_Months', event_in = 'Vital_Status', event_level = 'Dead')
#' kableExtra::kable(multivariable_output, 'html') %>% 
#'       kableExtra::collapse_rows(c(1:2), row_group_label_position = 'stack', headers_to_remove = 1:2)
#' 
#' 
#' @importFrom  Hmisc label
#' 
#' @export
#' 

run_pretty_model_output <- function(x_in, model_data, y_in, event_in = NULL, event_level = NULL, title_name = NULL, fail_if_warning = TRUE, conf_level = 0.95, overall_p_test_stat = c('Wald', 'LR'), est_digits = 3, p_digits = 4, latex_output = FALSE, sig_alpha = 0.05, background = 'yellow', verbose = FALSE, ...) {
  overall_p_test_stat <- match.arg(overall_p_test_stat)
  .check_numeric_input(est_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(p_digits, lower_bound = 1, upper_bound = 14, whole_num = TRUE, scalar = TRUE)
  .check_numeric_input(sig_alpha, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  .check_numeric_input(conf_level, lower_bound = 0, upper_bound = 1, scalar = TRUE)
  if (!all(x_in %in% colnames(model_data)))
    stop('All "x_in" (',paste0(x_in, collapse = ', '), ') must be in the "model_data" dataset')
  if (length(y_in) != 1) stop('"y_in" must be length of 1')
  if (all(y_in != colnames(model_data))) 
    stop('"y_in" (',y_in, ') not in the "model_data" dataset')
  if (length(unique(model_data[,y_in, drop = TRUE])) <= 1)
    stop('"y_in" (',y_in, ') must have more than one unique value')
  
  x_in_paste = paste0(x_in, collapse = ' + ')
  
  if (is.null(event_in)) {
    tmp_formula <- as.formula(paste(y_in, " ~ ", x_in_paste))
    if (length(unique(model_data[,y_in, drop = TRUE])) == 2) {
      # Logistic Model
      # making y_in a factor
      if (!is.null(event_level)) {
        if (all(unique(model_data[, y_in, drop = TRUE]) != event_level))
          stop('"event_level" (',event_level, ') not present in "y_in" (',y_in, ')')
        model_data[, y_in] <- factor(model_data[, y_in, drop = TRUE] == event_level)
      } else {
        model_data[, y_in] <- factor(model_data[, y_in, drop = TRUE])
        if (verbose) 
          message('Since no "event_level" specified setting "',levels(model_data[, y_in, drop = TRUE])[2], '" as outcome event level in logistic model')
      }
        if (nlevels(model_data[, y_in, drop = TRUE]) != 2) 
          stop('"y_in" (',y_in, ') must have two levels for logistic model')
      tmp_fit <- tryCatch(expr =  glm(tmp_formula, data = model_data, family = binomial(link = "logit")), 
                          error = function(c) stop('Logistic model with "',deparse(tmp_formula), '" formula has error(s) running'))
      if (fail_if_warning) {
        tmp_confint <- tryCatch(expr = suppressMessages(confint(tmp_fit)), 
                            error = function(c) stop('Logistic model with "',deparse(tmp_formula), '" formula has error(s) calculating CI(s)'), 
                            warning = function(c) stop('Logistic model with "',deparse(tmp_formula), '" formula has Inf CI(s); most likely a model error'))
      }
    } else {
      # Linear Model
      tmp_fit <- tryCatch(expr =  lm(tmp_formula, data = model_data), 
                          error = function(c) stop('Linear model with "',deparse(tmp_formula), '" formula has error(s) calculating CI(s)'))
      if (fail_if_warning) {
        tmp_confint <- tryCatch(expr = suppressMessages(confint(tmp_fit)), 
                                error = function(c) stop('Linear model with "',deparse(tmp_formula), '" formula has error(s) calculating CI(s)'), 
                                warning = function(c) stop('Linear model with "',deparse(tmp_formula), '" formula has Inf CI(s); most likely a model error'))
      }
    }
    n_info <- paste0('n=',nrow(tmp_fit$model))
  } else {
    # Cox Model
    if (all(event_in != colnames(model_data))) 
      stop('"event_in" (',event_in, ') not in the "model_data" dataset')
    if (length(unique(model_data[,event_in, drop = T])) > 2)
      stop('"event_in" (',event_in, ') must have only two levels')
    
    if (!is.null(event_level)) {
      if (all(unique(model_data[, event_in, drop = TRUE]) != event_level))
        stop('"event_level" (',event_level, ') not present in "event_in" (',event_in, ')')
      model_data[,event_in] <- model_data[,event_in, drop = T] == event_level
    } 
    event_levels <- unique(model_data[,event_in, drop = T])
    if (all(event_levels != TRUE))
      stop('"event_in" (',event_in, ') must have at least one event')
    

    tmp_formula <- as.formula(paste("survival::Surv(",y_in,",",event_in,") ~ ", x_in_paste))
    if (fail_if_warning) {
      tmp_fit <- tryCatch(expr = survival::coxph(tmp_formula, data = model_data), 
                          error = function(c) stop('Cox model with "',deparse(tmp_formula), '" formula has error(s) running'), 
                          warning = function(c) stop('Cox model with "',deparse(tmp_formula), '" formula has warnings(s) running'))
    } else {
      tmp_fit <- tryCatch(expr = survival::coxph(tmp_formula, data = model_data), 
                          error = function(c) stop('Cox model with "',deparse(tmp_formula), '" formula has error(s) running'))
      
    }
    n_info <- paste0('n=',tmp_fit$n,' (',tmp_fit$nevent,')')
  }
  
  tmp_output <- pretty_model_output(fit = tmp_fit, model_data = model_data, title_name = title_name, conf_level = conf_level, overall_p_test_stat = overall_p_test_stat, est_digits = est_digits, p_digits = p_digits, latex_output = latex_output, sig_alpha = sig_alpha, background = background, ...)
  tmp_output <- dplyr::bind_cols(tmp_output, n =  c(n_info, rep("", nrow(tmp_output) - 1)))
  
  if (!is.null(event_in)) names(tmp_output)[names(tmp_output) == 'n'] <- 'n (events)'
  
  tmp_output
}
