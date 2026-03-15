packages <- c(
  "tidyverse","janitor","caret","pROC","rpart","rpart.plot",
  "ranger","glmnet","broom","forcats"
)

to_install <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(caret)
  library(pROC)
  library(rpart)
  library(rpart.plot)
  library(ranger)
  library(glmnet)
  library(broom)
  library(forcats)
})

set.seed(42)

# -----------------------------
# 1) Load data
# -----------------------------
# If telco_raw already exists, keep it. Otherwise read from CSV.
if(!exists("telco_raw")){
  telco_raw <- readr::read_csv("Telco_customers.csv", show_col_types = FALSE)
}

telco_raw <- telco_raw %>% clean_names()

cat("\nData shape:", nrow(telco_raw), "rows x", ncol(telco_raw), "cols\n")

# -----------------------------
# 2) Helper functions
# -----------------------------
safe_num <- function(x){
  if(is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, "[\\$,]", "")
  x <- na_if(str_trim(x), "")
  suppressWarnings(as.numeric(x))
}

to_yes_no <- function(x){
  if(is.numeric(x)) return(ifelse(is.na(x), NA_character_, ifelse(x==1,"Yes","No")))
  if(is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x,"Yes","No")))
  x <- as.character(x)
  case_when(
    is.na(x) ~ NA_character_,
    str_to_lower(x) %in% c("1","yes","y","true","t") ~ "Yes",
    str_to_lower(x) %in% c("0","no","n","false","f") ~ "No",
    TRUE ~ x
  )
}

mode_impute <- function(x){
  if(all(is.na(x))) return(x)
  ux <- x[!is.na(x)]
  m <- names(sort(table(ux), decreasing = TRUE))[1]
  x[is.na(x)] <- m
  x
}

# -----------------------------
# 3) Cleaning + FIXES
# -----------------------------
telco <- telco_raw %>%
  mutate(
    # numeric coercions
    churn_label = safe_num(churn_label),
    churn_flag  = safe_num(churn_flag),
    satisfaction_score = safe_num(satisfaction_score),
    cltv = safe_num(cltv),
    tenure_months = safe_num(tenure_months),
    monthly_charge = safe_num(monthly_charge),
    total_charges = safe_num(total_charges),
    age = safe_num(age),
    under_30 = safe_num(under_30),
    senior_citizen = safe_num(senior_citizen),
    married = safe_num(married),
    dependents = safe_num(dependents),
    num_dependents = safe_num(num_dependents),
    phone_service = safe_num(phone_service),
    internet_service = safe_num(internet_service),
    streaming_tv = safe_num(streaming_tv),
    streaming_movies = safe_num(streaming_movies),
    
    # Tableau-safe ZIP
    zip_code = as.character(zip_code),
    
    # keep status as character
    customer_status = as.character(customer_status)
  )

# --- Fix churn target (customer_status has: Churned / Stayed / Joined) ---
telco <- telco %>%
  mutate(
    churn_flag_fixed = case_when(
      str_to_lower(customer_status) == "churned" ~ 1,
      str_to_lower(customer_status) %in% c("stayed","joined") ~ 0,
      TRUE ~ NA_real_
    ),
    churn = factor(ifelse(churn_flag_fixed == 1, "Churned", "Not Churned"),
                   levels = c("Not Churned","Churned"))
  )

cat("\nChurn distribution (fixed):\n")
print(table(telco$customer_status, useNA="ifany"))
print(table(telco$churn, useNA="ifany"))

# --- internet_service mapping based on your actual data: only 0/1 ---
telco <- telco %>%
  mutate(
    internet_service_type = case_when(
      internet_service == 1 ~ "Yes",
      internet_service == 0 ~ "No",
      TRUE ~ "Unknown"
    )
  )

# --- Add Tableau-friendly Yes/No versions of 0/1 fields (keep numeric too) ---
bin_cols <- c("phone_service","streaming_tv","streaming_movies",
              "under_30","senior_citizen","married","dependents")
for(col in bin_cols){
  telco[[paste0(col,"_yn")]] <- to_yes_no(telco[[col]])
}

# --- Repair total_charges if missing/invalid (optional but helpful) ---
telco <- telco %>%
  mutate(
    total_charges = ifelse(is.na(total_charges) | total_charges < 0,
                           monthly_charge * tenure_months,
                           total_charges)
  )

# --- Impute missing values lightly (Tableau-friendly) ---
num_cols <- telco %>% select(where(is.numeric)) %>% names()
chr_cols <- telco %>% select(where(is.character)) %>% names()

telco <- telco %>%
  mutate(across(all_of(num_cols), ~ ifelse(is.na(.x), median(.x, na.rm=TRUE), .x))) %>%
  mutate(across(all_of(chr_cols), ~ mode_impute(.x)))

# --- Feature engineering ---
telco <- telco %>%
  mutate(
    tenure_band = case_when(
      tenure_months < 6  ~ "0–5",
      tenure_months < 12 ~ "6–11",
      tenure_months < 24 ~ "12–23",
      tenure_months < 48 ~ "24–47",
      TRUE ~ "48+"
    ),
    monthly_charge_band = case_when(
      monthly_charge < 35 ~ "<35",
      monthly_charge < 70 ~ "35–69",
      monthly_charge < 100 ~ "70–99",
      TRUE ~ "100+"
    ),
    annual_revenue = monthly_charge * 12
  )

# -----------------------------
# 4) Tableau Exports
# -----------------------------
if(!exists("telco_clean_tableau")){
  telco_clean_tableau <- read_csv("telco_clean_tableau.csv")
}

telco_clean_tableau$tenure_band <- gsub("â€“", "-", telco_clean_tableau$tenure_band)

write_csv(telco_clean_tableau, "telco_clean_tableau.csv")

# Executive summary
executive_summary <- telco %>%
  summarise(
    total_customers = n(),
    churned = sum(churn_flag_fixed==1),
    churn_rate = churned/total_customers,
    joined = sum(str_to_lower(customer_status)=="joined"),
    joined_rate = joined/total_customers,
    avg_tenure_months = mean(tenure_months),
    avg_monthly_charge = mean(monthly_charge),
    total_annual_revenue = sum(annual_revenue),
    annual_revenue_at_risk = sum(annual_revenue * churn_flag_fixed)
  )
write_csv(executive_summary, "executive_summary.csv")

# KPI table (segment-ready for Tableau)
churn_kpis_tableau <- telco %>%
  group_by(internet_service_type, tenure_band, monthly_charge_band, state) %>%
  summarise(
    customers = n(),
    churned = sum(churn_flag_fixed==1),
    churn_rate = churned/customers,
    joined = sum(str_to_lower(customer_status)=="joined"),
    joined_rate = joined/customers,
    avg_tenure = mean(tenure_months),
    avg_monthly_charge = mean(monthly_charge),
    annual_revenue_total = sum(annual_revenue),
    annual_revenue_at_risk = sum(annual_revenue * churn_flag_fixed),
    .groups="drop"
  )

churn_kpis_tableau$tenure_band <- gsub("â€“", "-", churn_kpis_tableau$tenure_band)
churn_kpis_tableau$monthly_charge_band <- gsub("â€“", "-", churn_kpis_tableau$monthly_charge_band)

write_csv(churn_kpis_tableau, "churn_kpis_tableau.csv")

# Revenue risk by tenure
revenue_risk_tenure <- telco %>%
  group_by(tenure_band) %>%
  summarise(
    customers = n(),
    churn_rate = mean(churn_flag_fixed),
    annual_revenue_total = sum(annual_revenue),
    annual_revenue_at_risk = sum(annual_revenue * churn_flag_fixed),
    .groups="drop"
  )
write_csv(revenue_risk_tenure, "revenue_risk_tenure.csv")

# -----------------------------
# 5) Modeling (Leakage-safe)
# -----------------------------
# Recommendation: exclude "Joined" for a pure churn-vs-stayed model
telco_model <- telco %>% filter(str_to_lower(customer_status) != "joined")

# Build modeling dataset (drop leakage + IDs + super granular)
model_df <- telco_model %>%
  mutate(target = factor(ifelse(churn_flag_fixed==1,"Churned","Stayed"),
                         levels=c("Stayed","Churned"))) %>%
  select(
    -customer_id,
    -customer_status,   # leakage
    -churn_label,       # leakage
    -churn_flag,        # broken/leakage-ish
    -churn_flag_fixed,  # target
    -churn              # label version of target
  ) %>%
  select(-city, -zip_code)  # reduce overfitting

# Split train/test
idx <- createDataPartition(model_df$target, p=0.8, list=FALSE)
train <- model_df[idx,]
test  <- model_df[-idx,]

train_y <- train$target
test_y  <- test$target
train_x <- train %>% select(-target)
test_x  <- test %>% select(-target)

# Characters -> factors
train_x <- train_x %>% mutate(across(where(is.character), as.factor))
test_x  <- test_x  %>% mutate(across(where(is.character), as.factor))

# Remove near-zero variance
nzv <- nearZeroVar(train_x)
if(length(nzv)>0){
  train_x <- train_x[, -nzv, drop=FALSE]
  test_x  <- test_x[,  -nzv, drop=FALSE]
}

# Align factor levels and handle unseen levels as "Missing"
factor_cols <- names(which(sapply(train_x, is.factor)))
for(nm in factor_cols){
  test_x[[nm]] <- factor(test_x[[nm]], levels=levels(train_x[[nm]]))
  test_x[[nm]] <- fct_na_value_to_level(test_x[[nm]],  level="Missing")
  train_x[[nm]]<- fct_na_value_to_level(train_x[[nm]], level="Missing")
}

# Dummy encode
dmy <- dummyVars(~ ., data=train_x, fullRank=TRUE)
train_mat <- predict(dmy, newdata=train_x) %>% as.data.frame()
test_mat  <- predict(dmy, newdata=test_x) %>% as.data.frame()

cat("\nNA check (should be 0):\n")
print(sum(is.na(train_mat)))
print(sum(is.na(test_mat)))

# -----------------------------
# 6) Models
# -----------------------------
# 6.1 Penalized Logistic Regression (LASSO)
x_train <- as.matrix(train_mat)
y_train <- ifelse(train_y=="Churned",1,0)
x_test  <- as.matrix(test_mat)

set.seed(42)
cvfit <- cv.glmnet(x_train, y_train, family="binomial", alpha=1)

prob_lasso <- as.numeric(predict(cvfit, newx=x_test, s="lambda.min", type="response"))
pred_lasso <- factor(ifelse(prob_lasso>=0.5,"Churned","Stayed"), levels=levels(test_y))

# 6.2 Decision Tree
tree_fit <- rpart(train_y ~ ., data=train_mat %>% mutate(train_y=train_y), method="class")
tree_prob <- predict(tree_fit, newdata=test_mat, type="prob")[,"Churned"]
tree_pred <- factor(ifelse(tree_prob>=0.5,"Churned","Stayed"), levels=levels(test_y))

# 6.3 Random Forest
rf_fit <- ranger(
  x = train_mat,
  y = train_y,
  probability = TRUE,
  num.trees = 500,
  seed = 42
)

rf_prob <- predict(rf_fit, data = test_mat)$predictions[, "Churned"]
rf_pred <- factor(ifelse(rf_prob >= 0.5, "Churned", "Stayed"), levels = levels(test_y))

# -----------------------------
# 7) Evaluation + Exports
# -----------------------------
eval_model <- function(actual, pred, prob){
  cm <- confusionMatrix(pred, actual, positive="Churned")
  auc_val <- as.numeric(auc(roc(actual, prob, levels=c("Stayed","Churned"))))
  tibble(
    accuracy = cm$overall["Accuracy"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    f1 = cm$byClass["F1"],
    auc = auc_val
  )
}

model_comparison <- bind_rows(
  eval_model(test_y, pred_lasso, prob_lasso) %>% mutate(model="Penalized Logistic (LASSO)"),
  eval_model(test_y, tree_pred, tree_prob)   %>% mutate(model="Decision Tree"),
  eval_model(test_y, rf_pred, rf_prob)       %>% mutate(model="Random Forest")
) %>% select(model, everything())

write_csv(model_comparison, "model_comparison.csv")

# Export LASSO coefficients (drivers) — robust version
coef_df <- coef(cvfit, s = "lambda.min") %>%
  as.matrix() %>%
  as.data.frame()

# rename the only column to "coef"
colnames(coef_df) <- "coef"

coef_df <- coef_df %>%
  rownames_to_column("term") %>%
  filter(term != "(Intercept)", coef != 0) %>%
  mutate(abs_coef = abs(coef)) %>%
  arrange(desc(abs_coef))

write_csv(coef_df, "model_coefficients.csv")

# Optional: save RF importance (nice for a Tableau/slide)
rf_fit <- ranger(
  x = train_mat,
  y = train_y,
  probability = TRUE,
  num.trees = 500,
  importance = "impurity",
  seed = 42
)

# Optional: save RF importance (nice for Tableau/slide) — robust version
rf_imp <- data.frame(
  feature = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance),
  row.names = NULL
) %>%
  arrange(desc(importance)) %>%
  slice(1:20)

write_csv(rf_imp, "rf_feature_importance_top20.csv")

list.files(pattern = "csv$")

model_comparison

library(readr)
library(dplyr)
telco_clean_tableau <- read_csv("telco_clean_tableau.csv")

library(readr)
library(dplyr)

files <- c(
  "telco_clean_tableau.csv",
  "churn_kpis_tableau.csv",
  "revenue_risk_tenure.csv",
  "executive_summary.csv"
)

for (f in files) {
  
  # read with correct encoding
  temp <- read.csv(f, fileEncoding = "latin1", stringsAsFactors = FALSE)
  
  # fix dash issue globally
  temp[] <- lapply(temp, function(x) {
    if (is.character(x)) gsub("â€“", "-", x) else x
  })
  
  write.csv(temp, f, row.names = FALSE)
}

cat("✅ ALL FILES FIXED FOR REAL\n")



files <- c(
  "churn_kpis_tableau.csv"
)

for (f in files) {
  
  # read with correct encoding
  temp <- read.csv(f, fileEncoding = "latin1", stringsAsFactors = FALSE)
  
  # fix dash issue globally
  temp[] <- lapply(temp, function(x) {
    if (is.character(x)) gsub("ÃƒÂ¢Ã‚Â€Ã‚Â“", "-", x) else x
  })
  
  write.csv(temp, f, row.names = FALSE)
}

cat("✅ ALL FILES FIXED FOR REAL\n")
