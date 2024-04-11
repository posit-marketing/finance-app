# Packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(ggplot2)

# Data --------------------------------------------------------------------

con <-
  DBI::dbConnect(odbc::databricks(),
                 httpPath = "/sql/1.0/warehouses/300bd24ba12adf8e")

lendingclub_dat <-
  dplyr::tbl(con,
             dbplyr::in_catalog("hive_metastore", "default", "lendingclub")) |>
  collect()

lendingclub_dat_clean <-
  lendingclub_dat |>
  mutate(
    # Convert relevant columns to numeric
    across(c( "loan_amnt", "annual_inc_joint", "annual_inc", "installment", "tot_cur_bal", "inq_fi", "inq_last_12m", "num_tl_120dpd_2m", "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m", "bc_util", "max_bal_bc", "open_acc_6m", "num_rev_accts", "bc_open_to_buy", "percent_bc_gt_75", "total_bal_il", "total_il_high_credit_limit", "total_cu_tl", "total_bal_ex_mort"), as.numeric),
    # Missing values for these columns seem most appropriate to fill with zero
    across(c("inq_fi", "inq_last_12m", "num_tl_120dpd_2m", "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m"), ~ replace_na(., 0)),
    # Missing values for these columns seem most appropriate to fill with mean values
    across(c("bc_util", "max_bal_bc", "open_acc_6m", "num_rev_accts", "bc_open_to_buy", "percent_bc_gt_75", "total_bal_il", "total_il_high_credit_limit", "total_cu_tl"), ~ replace_na(., mean(.))),
    # Missing values for these columns seem most appropriate to fill with the column max
    across(c("mo_sin_old_il_acct", "mths_since_last_delinq", "mths_since_last_major_derog", "mths_since_last_record", "mths_since_rcnt_il", "mths_since_recent_bc", "mths_since_recent_bc_dlq", "mths_since_recent_inq", "mths_since_recent_revol_delinq"), ~ replace_na(., max(.))),
    earliest_cr_line = lubridate::parse_date_time2(paste("01", earliest_cr_line, sep =
                                                           "-"),
                                                   "dmy",
                                                   cutoff_2000 = 50L), 
    age_earliest_cr = lubridate::interval(as.Date(earliest_cr_line), as.Date(lubridate::today())) %/% lubridate::days(1),
    # Calculate a loan to income statistic
    loan_to_income = case_when(
      application_type == "Individual" ~ loan_amnt / annual_inc,
      .default = loan_amnt / annual_inc_joint
    ),
    #  Calculate the percentage of monthly income the installment payment represents
    installment_pct_inc = case_when(
      application_type == "Individual" ~ installment / (annual_inc / 12),
      .default = installment / (annual_inc_joint / 12)
    ),
    # Calculate the percentage of monthly income the installment payment represents
    adjusted_dti = case_when(
      application_type == "Individual" ~ (loan_amnt + tot_cur_bal) / (annual_inc),
      .default = (loan_amnt + tot_cur_bal) / (annual_inc_joint)
    ),
    #  Calculate utilization on installment accounts excluding mortgage balance
    il_util_ex_mort = case_when(
      total_il_high_credit_limit > 0 ~ total_bal_ex_mort / total_il_high_credit_limit,
      .default = 0
    ),
    # Fill debt to income joint with individual debt to income where missing
    dti_joint = coalesce(dti_joint, dti),
    # Fill annual income joint with individual annual income where missing
    annual_inc_joint = coalesce(annual_inc_joint, annual_inc),
    # Remove percent sign
    int_rate = as.numeric(stringr::str_remove(int_rate, "%")),
    revol_util = as.numeric(stringr::str_remove(revol_util, "%"))
    )

# Model -------------------------------------------------------------------

lendingclub_dat_clean <-
  lendingclub_dat_clean |> 
  mutate(across(where(is.character), .fns = as.factor)) |>
  select(where(~ sum(!is.na(.x)) > 0))

lendingclub_dat_clean |>
  select(where(is.factor)) %>%
  select(where( ~ nlevels(.) < 2))

applicant_numeric <- c('annual_inc','dti','age_earliest_cr','loan_amnt', 'installment')
applicant_text <- c('emp_title','title')
applicant_categorical <- c('application_type', 'emp_length', 'home_ownership', 'addr_state', 'term')
credit_numeric <- c('acc_open_past_24mths','avg_cur_bal','bc_open_to_buy','bc_util','delinq_2yrs','delinq_amnt','open_acc','pub_rec','revol_util','revol_bal','tot_coll_amt','tot_cur_bal','total_acc','total_rev_hi_lim','num_accts_ever_120_pd','num_actv_bc_tl','num_actv_rev_tl','num_bc_sats','num_bc_tl','num_il_tl', 'num_rev_tl_bal_gt_0','pct_tl_nvr_dlq','percent_bc_gt_75','tot_hi_cred_lim','total_bal_ex_mort','total_bc_limit','total_il_high_credit_limit','total_rev_hi_lim','all_util', 'loan_to_income', 'installment_pct_inc','il_util','il_util_ex_mort','total_bal_il','total_cu_tl')
NUMERIC_VARS_QB_20 <- c('inq_last_6mths','mo_sin_old_il_acct', 'mo_sin_old_rev_tl_op', 'mo_sin_old_rev_tl_op', 'mo_sin_rcnt_tl', 'mort_acc','num_op_rev_tl','num_rev_accts','num_sats','pub_rec','pub_rec_bankruptcies','tax_liens', 'all_util', 'loan_to_income')
NUMERIC_VARS_QB_5 <- c('num_tl_120dpd_2m','num_tl_30dpd')
NUMERIC_VARS_QB_10 <- c('revol_util','mths_since_last_delinq','mths_since_last_major_derog','mths_since_last_record','mths_since_rcnt_il','mths_since_recent_bc','mths_since_recent_bc_dlq','mths_since_recent_inq','mths_since_recent_revol_delinq', 'num_tl_90g_dpd_24m','num_tl_op_past_12m')
NUMERIC_VARS_QB_50 <- c('installment','bc_open_to_buy','loan_amnt','total_bc_limit','percent_bc_gt_75')

all_vars <- c(applicant_numeric, applicant_text, applicant_categorical, credit_numeric, NUMERIC_VARS_QB_20, NUMERIC_VARS_QB_5, NUMERIC_VARS_QB_10, NUMERIC_VARS_QB_50 )
lendingclub_dat_cols <- lendingclub_dat_clean |> select(int_rate, all_of(all_vars))

set.seed(55)
train_test_split <- initial_split(lendingclub_dat_cols)

lend_train <- training(train_test_split)
lend_test <- testing(train_test_split)

rec_obj <- recipe(int_rate ~ ., data = lend_train) |>
  step_normalize(applicant_numeric, credit_numeric) |>
  step_discretize(NUMERIC_VARS_QB_20, num_breaks = 20) |>
  step_discretize(NUMERIC_VARS_QB_5, num_breaks = 5) |>
  step_discretize(NUMERIC_VARS_QB_10, num_breaks = 10) |>
  step_discretize(NUMERIC_VARS_QB_50, num_breaks = 50)

lend_linear <- linear_reg()

lend_wflow <- 
  workflow() |>
  add_model(lend_linear) |>
  add_recipe(rec_obj)

tic()
lend_fit <-
  lend_wflow |>
  fit(data = lend_train)
toc()

