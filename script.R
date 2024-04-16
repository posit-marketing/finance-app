# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

# Data --------------------------------------------------------------------

con <-
  DBI::dbConnect(odbc::databricks(), httpPath = "/sql/1.0/warehouses/300bd24ba12adf8e")

lendingclub_dat <- 
  dplyr::tbl(con, dbplyr::in_catalog("hive_metastore", "default", "lendingclub")) |> 
  mutate(
    # Convert these columns into numeric
    across(c(starts_with("annual"), starts_with("dti"), starts_with("inq"),  starts_with("mo"), starts_with("mths"), starts_with("num"), starts_with("open"), starts_with("percent"), starts_with("pct"), starts_with("revol"), starts_with("tot"), "all_util", "il_util", "tax_liens",  "loan_amnt", "installment", "pub_rec_bankruptcies", "num_tl_120dpd_2m", "bc_util", "max_bal_bc", "bc_open_to_buy", "acc_open_past_24mths", "avg_cur_bal", "delinq_2yrs", "pub_rec"), ~ as.numeric(.)),
    # Calculate a loan to income statistic
    loan_to_income = case_when(
      application_type == "Individual" ~ loan_amnt / annual_inc,
      .default = loan_amnt / annual_inc_joint
    ),
    # Calculate a loan to income statistic
    loan_to_income = case_when(
      application_type == "Individual" ~ loan_amnt / annual_inc,
      .default = loan_amnt / annual_inc_joint
    ),
    # Calculate the percentage of monthly income the installment payment represents
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
    annual_inc_joint = coalesce(annual_inc_joint, annual_inc)) |> 
  collect()

lendingclub_dat_clean <-
  lendingclub_dat |>
  mutate(
    # Missing values for these columns seem most appropriate to fill with zero
    across(c("inq_fi", "dti", "all_util", "percent_bc_gt_75", "il_util", "avg_cur_bal","all_util", "il_util", "inq_last_6mths", "inq_last_12m", "num_tl_120dpd_2m", "open_il_12m", "open_il_24m", "open_rv_12m", "open_rv_24m"), ~ replace_na(., 0)),
    # Missing values for these columns seem most appropriate to fill with the column max
    across(c("mo_sin_old_il_acct", "mths_since_last_major_derog", "mths_since_last_delinq", "mths_since_recent_bc", "mths_since_last_record", "mths_since_rcnt_il", "mths_since_recent_bc", "mths_since_recent_bc_dlq", "mths_since_recent_inq", "mths_since_recent_revol_delinq", "mths_since_recent_revol_delinq"),  ~ replace_na(., max(., na.rm = TRUE))),
    # Remove percent sign
    int_rate = as.numeric(stringr::str_remove(int_rate, "%")),
    # Remove percent sign
    revol_util = as.numeric(stringr::str_remove(revol_util, "%")),
    # Create variable for earliest line of credit
    earliest_cr_line = lubridate::parse_date_time2(paste("01", earliest_cr_line, sep = "-"), "dmy", cutoff_2000 = 50L),
    # Calculate time since earliest line of credit
    age_earliest_cr = lubridate::interval(as.Date(earliest_cr_line), as.Date(lubridate::today())) %/% lubridate::days(1),
    # Convert characters to factors
    across(where(is.character), .fns = as.factor))

applicant_numeric     <- c("annual_inc","dti","age_earliest_cr","loan_amnt", "installment")
applicant_text        <- c("emp_title","title")
applicant_categorical <- c("application_type", "emp_length", "term")
credit_numeric        <- c("acc_open_past_24mths","avg_cur_bal","bc_open_to_buy","bc_util","delinq_2yrs","open_acc","pub_rec","revol_bal","tot_coll_amt","tot_cur_bal","total_acc","total_rev_hi_lim","num_accts_ever_120_pd","num_actv_bc_tl","num_actv_rev_tl","num_bc_sats","num_bc_tl","num_il_tl", "num_rev_tl_bal_gt_0","pct_tl_nvr_dlq","percent_bc_gt_75","tot_hi_cred_lim","total_bal_ex_mort","total_bc_limit","total_il_high_credit_limit","total_rev_hi_lim","all_util", "loan_to_income", "installment_pct_inc","il_util","il_util_ex_mort","total_bal_il","total_cu_tl")
NUMERIC_VARS_QB_20    <- c("inq_last_6mths","mo_sin_old_il_acct", "mo_sin_old_rev_tl_op", "mo_sin_old_rev_tl_op", "mo_sin_rcnt_tl", "mort_acc","num_op_rev_tl","num_rev_accts","num_sats","pub_rec","pub_rec_bankruptcies","tax_liens", "all_util", "loan_to_income")
NUMERIC_VARS_QB_5     <- c("num_tl_120dpd_2m")
NUMERIC_VARS_QB_10    <- c("mths_since_last_delinq","mths_since_last_major_derog","mths_since_last_record","mths_since_rcnt_il","mths_since_recent_bc","mths_since_recent_bc_dlq","mths_since_recent_inq","mths_since_recent_revol_delinq", "num_tl_90g_dpd_24m","num_tl_op_past_12m")
NUMERIC_VARS_QB_50    <- c("installment","bc_open_to_buy","loan_amnt","total_bc_limit","percent_bc_gt_75")
mean_impute_vals <- c("bc_util", "num_rev_accts", "bc_open_to_buy", "percent_bc_gt_75", "total_bal_il", "total_il_high_credit_limit", "total_cu_tl")

# Model -------------------------------------------------------------------

# lendingclub_dat_clean <-
#   lendingclub_dat_clean |> 
#   mutate(across(where(is.character), .fns = as.factor)) |>
#   select(where(~ sum(!is.na(.x)) > 0))

all_vars <- c(applicant_numeric, applicant_categorical, credit_numeric, NUMERIC_VARS_QB_20, NUMERIC_VARS_QB_5, NUMERIC_VARS_QB_10, NUMERIC_VARS_QB_50 )
lendingclub_dat_cols <- lendingclub_dat_clean |> select(int_rate, all_of(all_vars)) |> filter(!is.na(int_rate))

# Make sure there are no NAs
colSums(is.na(lendingclub_dat_cols))

# Make sure there are no columns with a single factor
lendingclub_dat_cols |>
  select(where(is.factor)) %>%
  select(where( ~ nlevels(.) < 2))

set.seed(1234)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(yardstick)
train_test_split <- initial_split(lendingclub_dat_cols)

lend_train <- training(train_test_split)
lend_test <- testing(train_test_split)

rec_obj <- recipe(int_rate ~ ., data = lend_train) |>
  step_normalize(applicant_numeric, credit_numeric) |>
  step_impute_mean(mean_impute_vals) |> 
  step_other()

# Review data
prep(rec_obj, lend_train) %>% bake(new_data = NULL)

# lend_linear <- linear_reg()

# lend_linear_wflow <- 
#   workflow() |>
#   add_model(lend_linear) |>
#   add_recipe(rec_obj)

# lend_linear_fit <-
#   lend_linear_wflow |>
#   fit(data = lend_train)
# 
# predict(lend_linear_fit, lend_test)
# 
# rsq(lend_ranger_results, truth = int_rate, estimate = .pred)
# 
# lend_ranger_results <-
#   bind_cols(predict(lend_ranger_fit, lend_train)) |>
#   bind_cols(lend_train |>
#               select(int_rate))

lend_rand <- rand_forest(mode = "regression") |>
  set_engine("ranger",
             importance = "permutation")
# 
# lend_ranger_wflow <- 
#   workflow() |>
#   add_model(lend_rand) |>
#   add_recipe(rec_obj)

# lend_ranger_fit <-
#   lend_ranger_wflow |>
#   fit(data = lend_train)

# predict(lend_ranger_fit, lend_test)
# 
# lend_ranger_results <-
#   bind_cols(predict(lend_ranger_fit, lend_train)) |>
#   bind_cols(lend_train |>
#               select(int_rate))

# ggplot(lend_ranger_results, aes(x = int_rate, y = .pred)) +
#   geom_point(color = "#FA8128",
#              alpha = 0.5) +
#   geom_smooth(method = "lm",
#               color = "#1B909E") +
#   labs(y = "Predicted Interest Rate", x = "Actual") +
#   coord_obs_pred() +
#   theme_minimal()
# 
# rsq(lend_ranger_results, truth = int_rate, estimate = .pred)

# vip:::vi(lend_ranger_fit) |> 
#   arrange(desc(Importance))

# prep(rec_obj, lend_train) %>% bake(newdata = NULL)

imp_var <- c("term", "installment_pct_inc", "bc_open_to_buy", "installment", "loan_to_income")

lendingclub_dat_cols_select <- 
  lendingclub_dat_cols |> 
  select(int_rate, all_of(imp_var))

train_test_split_select <- initial_split(lendingclub_dat_cols_select)

lend_train_select <- training(train_test_split_select)
lend_test_select <- testing(train_test_split_select)

rec_obj_select <- recipe(int_rate ~ ., data = lend_train_select) |>
  step_normalize(c("installment", "installment_pct_inc", "loan_to_income")) |>
  step_impute_mean("bc_open_to_buy") |> 
  step_other()

lend_wflow_select <- 
  workflow() |>
  add_model(lend_rand) |>
  add_recipe(rec_obj_select)

lend_fit_select <-
  lend_wflow_select |>
  fit(data = lend_train_select)

lend_results_select <-
  bind_cols(predict(lend_fit_select, lend_train_select)) |>
  bind_cols(lend_train_select |>
              select(int_rate))

rsq(lend_results_select, truth = int_rate, estimate = .pred)

library(vetiver)

v <- vetiver_model(lend_fit_select, "lend_fit")

board <-
  pins::board_connect(auth = "manual",
                      server = "https://pub.palm.ptd.posit.it/",
                      key = "vDr7oUh6vusooYyhwWAVOnJgxsjwD37G")

library(pins)

board %>% vetiver_pin_write(v)

library(plumber)

pr() %>%
  vetiver_api(v)

pins::pin_write(board = board,
                x = v,
                name = "isabella.velasquez/lending_model")

vetiver_deploy_rsconnect(
  board = board,
  name = "isabella.velasquez/lend_fit",
  predict_args = list(debug = TRUE)
)
≈