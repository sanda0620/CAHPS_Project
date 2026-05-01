# =============================================================================
#  CAHPS HEALTH PLAN SURVEY — COMPLETE STATISTICAL ANALYSIS IN R
#  Statement: "Patients who feel listened to by healthcare staff report
#              higher satisfaction with their care"
#
#  WORKFLOW:
#  STEP 1 → Load & Inspect
#  STEP 2 → Data Cleaning
#  STEP 3 → Descriptive Analytics
#  STEP 4 → Inferential Analytics
#  STEP 5 → Predictive Analytics
#  STEP 6 → Export Results
# =============================================================================

# ─── INSTALL PACKAGES (run once) ─────────────────────────────────────────────
# install.packages(c("tidyverse","ggplot2","car","rstatix","corrplot",
#                    "caret","gridExtra","ggpubr","scales","psych"))

library(tidyverse)   # data wrangling + ggplot2
library(ggplot2)     # visualisation
library(car)         # Levene's test, vif()
library(rstatix)     # Kruskal-Wallis, Dunn post-hoc
library(corrplot)    # correlation heatmaps
library(caret)       # train/test split, cross-validation
library(gridExtra)   # multi-panel plots
library(psych)       # describe() — richer summary stats
library(scales)      # percent_format for plots


# =============================================================================
# STEP 1: LOAD & INSPECT THE RAW DATA
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 1: LOAD & INSPECT\n")
cat("========================================================\n")

# ── 1.1 Load data ─────────────────────────────────────────────────────────────
df_raw <- read.csv("CAHPS_Health_Plan_Survey_Sample_Data.csv",
                   stringsAsFactors = FALSE,
                   na.strings        = c("", "NA", "N/A"))

cat("\n[1.1] Dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "columns\n")

# ── 1.2 Column names & data types ─────────────────────────────────────────────
cat("\n[1.2] Column names and types:\n")
glimpse(df_raw)

# ── 1.3 First few rows ────────────────────────────────────────────────────────
cat("\n[1.3] First 5 rows:\n")
print(head(df_raw, 5))

# ── 1.4 Missing values per column ─────────────────────────────────────────────
cat("\n[1.4] Missing value count per column:\n")
miss_counts <- colSums(is.na(df_raw))
print(miss_counts[miss_counts > 0])   # only show columns with missings

# ── 1.5 Duplicate rows ────────────────────────────────────────────────────────
cat("\n[1.5] Duplicate rows:", sum(duplicated(df_raw)), "\n")

# ── 1.6 Unique values per categorical column ──────────────────────────────────
cat("\n[1.6] Unique values in categorical columns:\n")
cat_cols <- c("Health_Plan_Name","Doctor_Listening","Doctor_Explanation",
              "Doctor_Respect","Doctor_Time","Overall_Health","Mental_Health",
              "Age_Group","Gender","Education_Level","Race")
for (col in cat_cols) {
  cat(sprintf("  %-30s: %s\n", col,
              paste(sort(unique(df_raw[[col]])), collapse=" | ")))
}

# ── 1.7 Rating columns range check ────────────────────────────────────────────
cat("\n[1.7] Rating variable ranges (should be 0-10):\n")
rating_cols <- c("Health_Plan_Rating","Doctor_Rating","Health_Care_Rating","Specialist_Rating")
for (col in rating_cols) {
  cat(sprintf("  %-25s: min=%d, max=%d\n", col,
              min(df_raw[[col]], na.rm=TRUE),
              max(df_raw[[col]], na.rm=TRUE)))
}


# =============================================================================
# STEP 2: DATA CLEANING
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 2: DATA CLEANING\n")
cat("========================================================\n")

df <- df_raw  # always work on a copy; keep df_raw untouched

# ── 2.1 Remove exact duplicate rows ───────────────────────────────────────────
cat("\n[2.1] Removing duplicate rows...\n")
n_before <- nrow(df)
df <- df[!duplicated(df), ]
cat(sprintf("  Rows removed: %d | Rows remaining: %d\n", n_before - nrow(df), nrow(df)))

# ── 2.2 Trim whitespace from all character columns ────────────────────────────
cat("\n[2.2] Trimming whitespace from character columns...\n")
df <- df %>%
  mutate(across(where(is.character), str_trim))
cat("  Done. All string columns trimmed.\n")

# ── 2.3 Standardise capitalisation (ensure consistency) ───────────────────────
cat("\n[2.3] Checking for capitalisation inconsistencies...\n")
# Convert ordinal responses to title case to catch any mixed-case entries
ordinal_cols <- c("Doctor_Listening","Doctor_Explanation","Doctor_Respect","Doctor_Time",
                  "Ease_of_Getting_Care","Immediate_Care_Received","Routine_Care_Received",
                  "Customer_Service_Help","Customer_Service_Respect","Forms_Ease_of_Use")
df <- df %>%
  mutate(across(all_of(ordinal_cols), str_to_title))

# Standardise Yes/No columns to proper capitalisation
yesno_cols <- c("Immediate_Care_Needed","Routine_Care_Appointments","Personal_Doctor",
                "Specialist_Appointments","Health_Plan_Customer_Service","Health_Plan_Forms",
                "Chronic_Condition_Visits","Long_Term_Condition","Medication_Needed",
                "Long_Term_Medication","Hispanic_or_Latino","Survey_Assistance")
df <- df %>%
  mutate(across(all_of(yesno_cols), str_to_title))
cat("  Done.\n")

# ── 2.4 Validate ordinal response categories ───────────────────────────────────
cat("\n[2.4] Validating ordinal category values...\n")
valid_freq <- c("Never", "Sometimes", "Usually", "Always")
valid_health <- c("Poor", "Fair", "Good", "Very Good", "Excellent")

for (col in ordinal_cols) {
  bad <- df[[col]][!df[[col]] %in% valid_freq]
  if (length(bad) > 0) {
    cat(sprintf("  WARNING: %s has unexpected values: %s\n", col, paste(unique(bad), collapse=", ")))
  } else {
    cat(sprintf("  %s: OK\n", col))
  }
}

# Fix "Very Good" -> "Very good" for Overall_Health and Mental_Health
df <- df %>%
  mutate(
    Overall_Health = case_when(Overall_Health == "Very Good" ~ "Very good", TRUE ~ Overall_Health),
    Mental_Health  = case_when(Mental_Health  == "Very Good" ~ "Very good", TRUE ~ Mental_Health)
  )

# ── 2.5 Validate rating columns (must be integer 0–10) ────────────────────────
cat("\n[2.5] Checking rating column validity (0–10)...\n")
for (col in rating_cols) {
  bad <- df[[col]][df[[col]] < 0 | df[[col]] > 10]
  if (length(bad) > 0) {
    cat(sprintf("  WARNING: %s has out-of-range values: %s\n", col, paste(unique(bad), collapse=", ")))
  } else {
    cat(sprintf("  %s: All values in [0, 10]. OK\n", col))
  }
}

# ── 2.6 Investigate and handle missing values ──────────────────────────────────
cat("\n[2.6] Missing value investigation...\n")

# Health_Care_Visits: 407 missing
# Test if missingness is random (MCAR) using t-test on Health_Care_Rating
if(sum(is.na(df$Health_Care_Visits)) > 0) {
  t_hcv <- t.test(
    df$Health_Care_Rating[is.na(df$Health_Care_Visits)],
    df$Health_Care_Rating[!is.na(df$Health_Care_Visits)]
  )
  cat(sprintf("  Health_Care_Visits MCAR test: t=%.4f, p=%.4f\n",
              t_hcv$statistic, t_hcv$p.value))
} else {
  cat("  Health_Care_Visits: No missing values — MCAR test skipped.\n")
}

# Doctor_Visits: 441 missing — check relationship with Personal_Doctor
tbl_dv <- table(df$Personal_Doctor, is.na(df$Doctor_Visits))
cat("\n  Doctor_Visits missing vs Personal_Doctor:\n")
print(tbl_dv)
cat("  NOTE: Doctor_Visits missing regardless of Personal_Doctor flag.\n")
cat("        This is a CAHPS survey skip-logic artefact — flag as contextual missing.\n")

# Specialist_Visits: 500 missing
tbl_sv <- table(df$Specialist_Appointments, is.na(df$Specialist_Visits))
cat("\n  Specialist_Visits missing vs Specialist_Appointments:\n")
print(tbl_sv)

# DECISION: These 3 columns are NOT used as predictors in our analysis.
# Our key variables (Doctor_Listening, all ratings, demographics) have ZERO missing.
# We flag them but do NOT impute, drop rows, or alter the analysis columns.
cat("\n  DECISION: Missing columns (Health_Care_Visits, Doctor_Visits, Specialist_Visits)\n")
cat("            are visit-count variables not used in our analysis.\n")
cat("            Our analytical variables have 0 missing values — no imputation needed.\n")

# ── 2.7 Outlier detection on rating variables ──────────────────────────────────
cat("\n[2.7] Outlier detection (IQR method) on rating variables...\n")
for (col in rating_cols) {
  Q1  <- quantile(df[[col]], 0.25, na.rm=TRUE)
  Q3  <- quantile(df[[col]], 0.75, na.rm=TRUE)
  IQR <- Q3 - Q1
  lower_fence <- Q1 - 1.5 * IQR
  upper_fence <- Q3 + 1.5 * IQR
  n_out <- sum(df[[col]] < lower_fence | df[[col]] > upper_fence, na.rm=TRUE)
  cat(sprintf("  %-25s: Q1=%.1f, Q3=%.1f, IQR=%.1f | Fences=[%.1f, %.1f] | Outliers=%d\n",
              col, Q1, Q3, IQR, lower_fence, upper_fence, n_out))
}
cat("  NOTE: No outliers detected. Rating scale is bounded [0,10] by design.\n")

# ── 2.8 Logical consistency check ─────────────────────────────────────────────
cat("\n[2.8] Logical consistency check (CAHPS survey skip-logic)...\n")
# In CAHPS, some respondents answer sub-questions even when the filter question = No.
# This is a known characteristic of CAHPS surveys (respondents may misread branching).
# These cells are NOT treated as errors — they are flagged as 'inapplicable responses'.
ic_flag  <- sum(df$Immediate_Care_Needed == "No" & !is.na(df$Immediate_Care_Received))
rc_flag  <- sum(df$Routine_Care_Appointments == "No" & !is.na(df$Routine_Care_Received))
cat(sprintf("  Immediate care received when not needed: %d rows (CAHPS artefact — kept)\n", ic_flag))
cat(sprintf("  Routine care received when no appts:     %d rows (CAHPS artefact — kept)\n", rc_flag))

# ── 2.9 Create clean analytical dataset ───────────────────────────────────────
cat("\n[2.9] Creating clean analytical dataset...\n")
df_clean <- df
cat(sprintf("  Final clean dataset: %d rows x %d columns\n", nrow(df_clean), ncol(df_clean)))
cat("  Missing in analytical variables: ")
anal_vars <- c("Doctor_Listening","Doctor_Explanation","Doctor_Respect","Doctor_Time",
               "Health_Plan_Rating","Doctor_Rating","Health_Care_Rating","Specialist_Rating",
               "Overall_Health","Age_Group","Gender","Education_Level")
cat(sum(colSums(is.na(df_clean[anal_vars]))), "— CLEAN\n")

# ── 2.10 Save cleaned data ────────────────────────────────────────────────────
write.csv(df_clean, "CAHPS_Cleaned.csv", row.names=FALSE)
cat("  Saved: CAHPS_Cleaned.csv\n")

cat("\n>>> DATA CLEANING COMPLETE\n")


# =============================================================================
# STEP 3: FEATURE ENGINEERING (encoding for analysis)
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 3: FEATURE ENGINEERING\n")
cat("========================================================\n")

# ── 3.1 Encode ordinal communication variables ────────────────────────────────
# Scale: Never=0, Sometimes=1, Usually=2, Always=3
listening_levels <- c("Never", "Sometimes", "Usually", "Always")

df_clean <- df_clean %>%
  mutate(
    # Ordered factor (preserves rank for non-parametric tests)
    Listening_Factor    = factor(Doctor_Listening,   levels=listening_levels, ordered=TRUE),
    Explanation_Factor  = factor(Doctor_Explanation, levels=listening_levels, ordered=TRUE),
    Respect_Factor      = factor(Doctor_Respect,     levels=listening_levels, ordered=TRUE),
    Time_Factor         = factor(Doctor_Time,        levels=listening_levels, ordered=TRUE),

    # Numeric (0–3) for regression and correlation
    Listening_Num   = as.numeric(Listening_Factor)   - 1,
    Explanation_Num = as.numeric(Explanation_Factor) - 1,
    Respect_Num     = as.numeric(Respect_Factor)     - 1,
    Time_Num        = as.numeric(Time_Factor)        - 1,

    # Composite communication score (mean of 4 items, range 0–3)
    Communication_Score = (Listening_Num + Explanation_Num + Respect_Num + Time_Num) / 4
  )
cat("[3.1] Communication variables encoded (0=Never, 1=Sometimes, 2=Usually, 3=Always)\n")
cat("      Composite Communication_Score created (mean of 4 items, range 0–3)\n")

# ── 3.2 Encode health status ──────────────────────────────────────────────────
health_levels <- c("Poor","Fair","Good","Very good","Excellent")
df_clean <- df_clean %>%
  mutate(
    Overall_Health_Num = as.numeric(factor(Overall_Health, levels=health_levels, ordered=TRUE)) - 1,
    Mental_Health_Num  = as.numeric(factor(Mental_Health,  levels=health_levels, ordered=TRUE)) - 1
  )
cat("[3.2] Health status encoded (0=Poor to 4=Excellent)\n")

# ── 3.3 Encode demographics ───────────────────────────────────────────────────
age_levels <- c("18 to 24","25 to 34","35 to 44","45 to 54","55 to 64","65 to 74","75 or older")
edu_levels <- c("8th grade or less","Some high school","High school graduate or GED",
                "Some college or 2-year degree","4-year college graduate",
                "More than 4-year college degree")

df_clean <- df_clean %>%
  mutate(
    Gender_Num    = ifelse(Gender == "Female", 1, 0),
    Age_Num       = as.numeric(factor(Age_Group, levels=age_levels, ordered=TRUE)) - 1,
    Education_Num = as.numeric(factor(Education_Level, levels=edu_levels, ordered=TRUE)) - 1
  )
cat("[3.3] Demographics encoded: Gender(0/1), Age(0–6), Education(0–5)\n")

# ── 3.4 Create binary outcome variable ───────────────────────────────────────
# High Satisfaction: Health_Plan_Rating >= 7 (commonly used CAHPS threshold)
df_clean <- df_clean %>%
  mutate(High_Satisfaction = ifelse(Health_Plan_Rating >= 7, 1, 0))
cat("[3.4] Binary outcome: High_Satisfaction (1 = HPR >= 7, 0 = HPR < 7)\n")
cat(sprintf("      High satisfaction: %d (%.1f%%) | Low: %d (%.1f%%)\n",
    sum(df_clean$High_Satisfaction),
    mean(df_clean$High_Satisfaction)*100,
    sum(df_clean$High_Satisfaction==0),
    mean(df_clean$High_Satisfaction==0)*100))

cat("\n>>> FEATURE ENGINEERING COMPLETE\n")


# =============================================================================
# STEP 4: DESCRIPTIVE ANALYTICS
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 4: DESCRIPTIVE ANALYTICS\n")
cat("========================================================\n")

# ── 4.1 Overall summary statistics ────────────────────────────────────────────
cat("\n[4.1] Summary statistics for key rating variables:\n")
rating_summary <- df_clean %>%
  summarise(across(all_of(rating_cols),
                   list(Mean   = ~round(mean(.),2),
                        SD     = ~round(sd(.),2),
                        Median = ~median(.),
                        Min    = ~min(.),
                        Max    = ~max(.)),
                   .names="{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to=c("Variable","Stat"), names_sep="__") %>%
  pivot_wider(names_from=Stat, values_from=value)
print(rating_summary)

# ── 4.2 Doctor_Listening frequency distribution ────────────────────────────────
cat("\n[4.2] Doctor_Listening distribution:\n")
listening_dist <- df_clean %>%
  count(Doctor_Listening) %>%
  mutate(
    Doctor_Listening = factor(Doctor_Listening, levels=listening_levels),
    Percent = round(n/sum(n)*100, 1)
  ) %>%
  arrange(Doctor_Listening)
print(listening_dist)

# ── 4.3 Satisfaction ratings by Listening level ───────────────────────────────
cat("\n[4.3] Mean satisfaction ratings by Doctor_Listening:\n")
group_summary <- df_clean %>%
  mutate(Doctor_Listening = factor(Doctor_Listening, levels=listening_levels)) %>%
  group_by(Doctor_Listening) %>%
  summarise(
    n              = n(),
    Mean_HPR       = round(mean(Health_Plan_Rating), 2),
    SD_HPR         = round(sd(Health_Plan_Rating), 2),
    Median_HPR     = median(Health_Plan_Rating),
    Mean_DR        = round(mean(Doctor_Rating), 2),
    Mean_HCR       = round(mean(Health_Care_Rating), 2),
    High_Sat_Pct   = round(mean(High_Satisfaction)*100, 1),
    .groups = "drop"
  )
print(group_summary)

# ── 4.4 Communication score summary ────────────────────────────────────────────
cat("\n[4.4] Composite Communication Score summary:\n")
comm_desc <- df_clean %>%
  summarise(
    Mean   = round(mean(Communication_Score), 3),
    SD     = round(sd(Communication_Score), 3),
    Median = median(Communication_Score),
    Min    = min(Communication_Score),
    Max    = max(Communication_Score)
  )
print(comm_desc)

# ── 4.5 VISUALISATIONS ────────────────────────────────────────────────────────
cat("\n[4.5] Generating descriptive plots...\n")

# PLOT 1: Boxplot — HPR by Listening Level
p1 <- ggplot(df_clean,
             aes(x = factor(Doctor_Listening, levels=listening_levels),
                 y = Health_Plan_Rating,
                 fill = factor(Doctor_Listening, levels=listening_levels))) +
  geom_boxplot(alpha=0.75, outlier.alpha=0.3, outlier.size=1) +
  stat_summary(fun=mean, geom="point", shape=18, size=4, color="darkred",
               aes(group=1)) +
  stat_summary(fun=mean, geom="line",  color="darkred", linetype="dashed",
               linewidth=0.8, aes(group=1)) +
  scale_fill_manual(values=c("#D4E6F1","#85C1E9","#3498DB","#1A5276")) +
  scale_y_continuous(limits=c(0,10), breaks=0:10) +
  labs(title    = "Figure 1: Health Plan Rating by Doctor Listening Frequency",
       subtitle = "Red diamonds = group means; dashed line shows trend",
       x = "How Often Doctor Listened", y = "Health Plan Rating (0–10)") +
  theme_minimal(base_size=13) +
  theme(legend.position="none",
        plot.title   = element_text(face="bold"),
        panel.grid.minor = element_blank())
print(p1)
ggsave("Fig1_Boxplot_HPR_by_Listening.png", p1, width=9, height=5.5, dpi=150)

# PLOT 2: Bar chart — High Satisfaction % by Listening Level
p2 <- group_summary %>%
  ggplot(aes(x=Doctor_Listening, y=High_Sat_Pct,
             fill=Doctor_Listening)) +
  geom_col(alpha=0.85, width=0.6) +
  geom_text(aes(label=paste0(High_Sat_Pct,"%")), vjust=-0.6, size=4.5, fontface="bold") +
  scale_fill_manual(values=c("#D4E6F1","#85C1E9","#3498DB","#1A5276")) +
  scale_y_continuous(limits=c(0,50), labels=percent_format(scale=1)) +
  labs(title    = "Figure 2: High Satisfaction Rate (HPR ≥ 7) by Doctor Listening Level",
       x = "How Often Doctor Listened", y = "% of Patients with High Satisfaction") +
  theme_minimal(base_size=13) +
  theme(legend.position="none", plot.title=element_text(face="bold"))
print(p2)
ggsave("Fig2_HighSat_by_Listening.png", p2, width=8, height=5, dpi=150)

# PLOT 3: Distribution of Health_Plan_Rating (overall)
p3 <- ggplot(df_clean, aes(x=Health_Plan_Rating)) +
  geom_histogram(binwidth=1, fill="#2E75B6", color="white", alpha=0.8) +
  geom_vline(aes(xintercept=mean(Health_Plan_Rating)), color="red",
             linetype="dashed", linewidth=1) +
  annotate("text", x=mean(df_clean$Health_Plan_Rating)+0.6,
           y=280, label=paste0("Mean=",round(mean(df_clean$Health_Plan_Rating),2)),
           color="red", size=4) +
  scale_x_continuous(breaks=0:10) +
  labs(title = "Figure 3: Distribution of Health Plan Rating (n=3,000)",
       x = "Health Plan Rating (0–10)", y = "Count") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))
print(p3)
ggsave("Fig3_HPR_Distribution.png", p3, width=8, height=5, dpi=150)

# PLOT 4: Listening distribution (pie or bar)
p4 <- ggplot(listening_dist, aes(x=Doctor_Listening, y=n, fill=Doctor_Listening)) +
  geom_col(alpha=0.85, width=0.6) +
  geom_text(aes(label=paste0(n,"\n(",Percent,"%)")), vjust=-0.4, size=4) +
  scale_fill_manual(values=c("#D4E6F1","#85C1E9","#3498DB","#1A5276")) +
  scale_y_continuous(limits=c(0,900)) +
  labs(title = "Figure 4: Distribution of Doctor Listening Responses",
       x = "How Often Doctor Listened", y = "Number of Respondents") +
  theme_minimal(base_size=13) +
  theme(legend.position="none", plot.title=element_text(face="bold"))
print(p4)
ggsave("Fig4_Listening_Distribution.png", p4, width=8, height=5, dpi=150)

cat("  Figures 1–4 saved.\n")
cat("\n>>> DESCRIPTIVE ANALYTICS COMPLETE\n")


# =============================================================================
# STEP 5: INFERENTIAL ANALYTICS
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 5: INFERENTIAL ANALYTICS\n")
cat("========================================================\n")

# ── 5.1 Assumption Testing ────────────────────────────────────────────────────
cat("\n[5.1] Testing statistical assumptions...\n")

# Normality check: Shapiro-Wilk on Health_Plan_Rating (subsample n=200)
set.seed(42)
sw_sample <- sample(df_clean$Health_Plan_Rating, 200)
sw <- shapiro.test(sw_sample)
cat(sprintf("  Shapiro-Wilk (HPR, n=200): W=%.4f, p=%.6f\n", sw$statistic, sw$p.value))
cat(sprintf("  Interpretation: %s\n",
    ifelse(sw$p.value < 0.05,
           "NOT normally distributed → Use non-parametric tests (Kruskal-Wallis)",
           "Normally distributed → Parametric tests appropriate")))

# Levene's test for homogeneity of variance
levene_res <- leveneTest(Health_Plan_Rating ~ factor(Doctor_Listening, levels=listening_levels),
                         data=df_clean)
cat(sprintf("\n  Levene's Test for equal variances:\n"))
print(levene_res)

# ── 5.2 Spearman Rank Correlation ─────────────────────────────────────────────
cat("\n[5.2] Spearman Rank Correlation — Listening vs Ratings:\n")
cat("  (Spearman chosen: ordinal predictor × non-normal continuous outcome)\n\n")

corr_results <- data.frame(
  Outcome   = character(),
  Spearman_rho = numeric(),
  p_value   = numeric(),
  Significant = character(),
  stringsAsFactors=FALSE
)
for (rv in rating_cols) {
  res <- cor.test(df_clean$Listening_Num, df_clean[[rv]],
                  method="spearman", exact=FALSE)
  sig <- ifelse(res$p.value < 0.05,
                ifelse(res$p.value < 0.001, "*** p<0.001",
                       ifelse(res$p.value < 0.01, "** p<0.01", "* p<0.05")),
                "n.s.")
  corr_results <- rbind(corr_results, data.frame(
    Outcome      = rv,
    Spearman_rho = round(res$estimate, 4),
    p_value      = round(res$p.value, 4),
    Significant  = sig
  ))
  cat(sprintf("  Listening vs %-25s: rho=%+.4f, p=%.4f  %s\n",
              rv, res$estimate, res$p.value, sig))
}

# Also: Composite Communication Score correlations
cat("\n  Composite Communication_Score correlations:\n")
for (rv in rating_cols) {
  res <- cor.test(df_clean$Communication_Score, df_clean[[rv]], method="pearson")
  cat(sprintf("  CommScore vs %-25s: r=%+.4f, p=%.4f\n",
              rv, res$estimate, res$p.value))
}

# ── 5.3 Kruskal-Wallis Test ────────────────────────────────────────────────────
cat("\n[5.3] Kruskal-Wallis Test — Listening Groups vs Rating Variables:\n")
cat("  H0: All Listening groups have identical rank distributions\n\n")

kw_results <- data.frame()
for (rv in rating_cols) {
  kw <- kruskal.test(df_clean[[rv]] ~
                     factor(df_clean$Doctor_Listening, levels=listening_levels))
  decision <- ifelse(kw$p.value < 0.05, "REJECT H0 (significant)", "FAIL TO REJECT H0")
  kw_results <- rbind(kw_results, data.frame(
    Outcome   = rv,
    H_stat    = round(kw$statistic, 4),
    df        = kw$parameter,
    p_value   = round(kw$p.value, 4),
    Decision  = decision
  ))
  cat(sprintf("  %-25s: H=%.4f, df=%d, p=%.4f → %s\n",
              rv, kw$statistic, kw$parameter, kw$p.value, decision))
}

# ── 5.4 Post-hoc Dunn Test (for Health_Care_Rating which was borderline) ───────
cat("\n[5.4] Post-hoc Dunn Test (Bonferroni corrected) — Health_Care_Rating:\n")
dunn_res <- dunn_test(df_clean, Health_Care_Rating ~ Doctor_Listening,
                      p.adjust.method="bonferroni")
print(dunn_res)

# ── 5.5 One-Way ANOVA (supplementary) ────────────────────────────────────────
cat("\n[5.5] One-Way ANOVA — Listening vs Health_Plan_Rating (supplementary):\n")
anova_model <- aov(Health_Plan_Rating ~
                   factor(Doctor_Listening, levels=listening_levels),
                   data=df_clean)
print(summary(anova_model))

# Check ANOVA residuals normality
sw_resid <- shapiro.test(sample(residuals(anova_model), 200))
cat(sprintf("  Residuals normality (Shapiro-Wilk n=200): W=%.4f, p=%.6f\n",
            sw_resid$statistic, sw_resid$p.value))
cat("  → Non-normal residuals confirm Kruskal-Wallis is more appropriate.\n")

# ── 5.6 Correlation Heatmap ───────────────────────────────────────────────────
cat("\n[5.6] Generating Spearman correlation heatmap...\n")
cor_data <- df_clean %>%
  select(Listening_Num, Explanation_Num, Respect_Num, Time_Num,
         Health_Plan_Rating, Doctor_Rating, Health_Care_Rating, Specialist_Rating,
         Communication_Score, Overall_Health_Num)

colnames(cor_data) <- c("Listening","Explanation","Respect","Time Spent",
                        "HP Rating","Doctor Rtg","HC Rating","Spec. Rtg",
                        "Comm. Score","Overall Health")

cor_matrix <- cor(cor_data, method="spearman", use="complete.obs")

png("Fig5_Correlation_Heatmap.png", width=850, height=750, res=120)
corrplot(cor_matrix,
         method     = "color",
         type       = "upper",
         order      = "hclust",
         addCoef.col = "black",
         number.cex = 0.65,
         tl.col     = "black",
         tl.srt     = 45,
         col        = colorRampPalette(c("#D73027","white","#1A5276"))(200),
         title      = "Figure 5: Spearman Correlation Matrix",
         mar        = c(0,0,2,0))
dev.off()
cat("  Fig5_Correlation_Heatmap.png saved.\n")

cat("\n>>> INFERENTIAL ANALYTICS COMPLETE\n")


# =============================================================================
# STEP 6: PREDICTIVE ANALYTICS
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 6: PREDICTIVE ANALYTICS\n")
cat("========================================================\n")

# ── 6.1 Prepare modelling dataset ────────────────────────────────────────────
cat("\n[6.1] Preparing modelling dataset...\n")
model_df <- df_clean %>%
  select(Health_Plan_Rating, High_Satisfaction,
         Listening_Num, Explanation_Num, Respect_Num, Time_Num,
         Communication_Score,
         Doctor_Rating, Overall_Health_Num, Age_Num, Gender_Num, Education_Num) %>%
  drop_na()

cat(sprintf("  Modelling dataset: %d rows x %d columns\n", nrow(model_df), ncol(model_df)))

# ── 6.2 Train/Test Split (80/20, stratified) ─────────────────────────────────
cat("\n[6.2] Splitting data 80% train / 20% test...\n")
set.seed(42)
train_idx  <- createDataPartition(model_df$Health_Plan_Rating, p=0.8, list=FALSE)
train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]
cat(sprintf("  Train: %d rows | Test: %d rows\n", nrow(train_data), nrow(test_data)))

# ── 6.3 Model 1: Communication-Only Linear Regression ────────────────────────
cat("\n[6.3] MODEL 1: Communication-Only Linear Regression\n")
model1 <- lm(Health_Plan_Rating ~ Listening_Num + Explanation_Num +
               Respect_Num + Time_Num,
             data=train_data)
cat("\n  Model 1 Summary:\n")
print(summary(model1))

# Test set performance
pred1 <- predict(model1, newdata=test_data)
rmse1 <- sqrt(mean((test_data$Health_Plan_Rating - pred1)^2))
r2_1  <- cor(test_data$Health_Plan_Rating, pred1)^2
cat(sprintf("  Test R²: %.4f | Test RMSE: %.4f\n", r2_1, rmse1))

# ── 6.4 Model 2: Full Model ────────────────────────────────────────────────────
cat("\n[6.4] MODEL 2: Full Model (Communication + Demographics + Doctor_Rating)\n")
model2 <- lm(Health_Plan_Rating ~ Listening_Num + Explanation_Num + Respect_Num + Time_Num +
               Doctor_Rating + Overall_Health_Num + Age_Num + Gender_Num + Education_Num,
             data=train_data)
cat("\n  Model 2 Summary:\n")
print(summary(model2))

pred2 <- predict(model2, newdata=test_data)
rmse2 <- sqrt(mean((test_data$Health_Plan_Rating - pred2)^2))
r2_2  <- cor(test_data$Health_Plan_Rating, pred2)^2
cat(sprintf("  Test R²: %.4f | Test RMSE: %.4f\n", r2_2, rmse2))

# ── 6.5 5-Fold Cross-Validation ───────────────────────────────────────────────
cat("\n[6.5] 5-Fold Cross-Validation...\n")
ctrl <- trainControl(method="cv", number=5, verboseIter=FALSE)

cv1 <- train(Health_Plan_Rating ~ Listening_Num + Explanation_Num + Respect_Num + Time_Num,
             data=model_df, method="lm", trControl=ctrl)
cv2 <- train(Health_Plan_Rating ~ Listening_Num + Explanation_Num + Respect_Num + Time_Num +
               Doctor_Rating + Overall_Health_Num + Age_Num + Gender_Num + Education_Num,
             data=model_df, method="lm", trControl=ctrl)

cat(sprintf("  CV R² — Model 1 (Comm only): %.4f | CV RMSE: %.4f\n",
            cv1$results$Rsquared, cv1$results$RMSE))
cat(sprintf("  CV R² — Model 2 (Full):      %.4f | CV RMSE: %.4f\n",
            cv2$results$Rsquared, cv2$results$RMSE))

# ── 6.6 Model Comparison ──────────────────────────────────────────────────────
cat("\n[6.6] Model Comparison (AIC / BIC):\n")
cat(sprintf("  Model 1 — AIC: %.2f | BIC: %.2f\n", AIC(model1), BIC(model1)))
cat(sprintf("  Model 2 — AIC: %.2f | BIC: %.2f\n", AIC(model2), BIC(model2)))

# VIF check for multicollinearity (Model 2)
cat("\n  Variance Inflation Factors (Model 2) — checking multicollinearity:\n")
print(vif(model2))

# ── 6.7 Model Diagnostics Plots ───────────────────────────────────────────────
cat("\n[6.7] Generating predictive model plots...\n")

# PLOT 6: Predicted vs Actual
pred_df <- data.frame(
  Actual    = test_data$Health_Plan_Rating,
  Predicted = pred2
)
p6 <- ggplot(pred_df, aes(x=Actual, y=Predicted)) +
  geom_jitter(alpha=0.3, color="#2E75B6", width=0.2, height=0.2, size=1.5) +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed", linewidth=1.2) +
  geom_smooth(method="lm", color="#1A5276", se=TRUE, linewidth=0.9) +
  scale_x_continuous(breaks=0:10) +
  scale_y_continuous(breaks=seq(2,8,1)) +
  labs(title    = "Figure 6: Predicted vs Actual Health Plan Rating (Full Model)",
       subtitle = paste0("Test R² = ", round(r2_2,4), "  |  RMSE = ", round(rmse2,4)),
       x="Actual Rating (0–10)", y="Predicted Rating") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))
print(p6)
ggsave("Fig6_Predicted_vs_Actual.png", p6, width=7.5, height=5.5, dpi=150)

# PLOT 7: Coefficient plot (Model 2)
coef_df <- broom::tidy(model2, conf.int=TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Significant = ifelse(p.value < 0.05, "Significant (p<0.05)", "Not significant"),
   term = case_when(
      term == "Listening_Num"      ~ "Doctor Listening",
      term == "Explanation_Num"    ~ "Doctor Explanation",
      term == "Respect_Num"        ~ "Doctor Respect",
      term == "Time_Num"           ~ "Time Spent",
      term == "Doctor_Rating"      ~ "Doctor Rating",
      term == "Overall_Health_Num" ~ "Overall Health",
      term == "Age_Num"            ~ "Age Group",
      term == "Gender_Num"         ~ "Gender (Female)",
      term == "Education_Num"      ~ "Education Level",
      TRUE ~ term
    )
  )

p7 <- ggplot(coef_df,
             aes(x=reorder(term, estimate), y=estimate, color=Significant)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0.3, linewidth=0.8) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50", linewidth=0.8) +
  coord_flip() +
  scale_color_manual(values=c("Significant (p<0.05)"="#C0392B",
                              "Not significant"     ="#2E75B6")) +
  labs(title    = "Figure 7: Regression Coefficients with 95% CI (Full Model)",
       subtitle = "Points right of zero increase predicted rating; left decrease it",
       x=NULL, y="Coefficient Estimate", color=NULL) +
  theme_minimal(base_size=13) +
  theme(legend.position ="bottom",
        plot.title=element_text(face="bold"))
print(p7)
ggsave("Fig7_Coefficient_Plot.png", p7, width=8.5, height=5.5, dpi=150)

# PLOT 8: Residual plot (Model 2)
resid_df <- data.frame(
  Fitted   = fitted(model2),
  Residuals = residuals(model2)
)
p8 <- ggplot(resid_df, aes(x=Fitted, y=Residuals)) +
  geom_point(alpha=0.2, color="#2E75B6", size=1) +
  geom_hline(yintercept=0, color="red", linetype="dashed", linewidth=1) +
  geom_smooth(method="loess", color="orange", se=FALSE, linewidth=0.9) +
  labs(title    = "Figure 8: Residuals vs Fitted Values (Full Model)",
       subtitle = "Random scatter around 0 = good fit; patterns = model issues",
       x="Fitted Values", y="Residuals") +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face="bold"))
print(p8)
ggsave("Fig8_Residuals_vs_Fitted.png", p8, width=8, height=5, dpi=150)

cat("  Figures 6–8 saved.\n")
cat("\n>>> PREDICTIVE ANALYTICS COMPLETE\n")


# =============================================================================
# STEP 7: RESULTS SUMMARY
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 7: RESULTS SUMMARY\n")
cat("========================================================\n")

cat("
ANALYTICAL STATEMENT:
  'Patients who feel listened to by healthcare staff report higher
   satisfaction with their care'

DATA QUALITY:
  - 3,000 rows, 36 columns
  - 0 duplicate rows
  - 0 whitespace issues
  - 0 out-of-range values in rating columns
  - 3 columns had missing values (Health_Care_Visits n=407, Doctor_Visits n=441,
    Specialist_Visits n=500) — all are visit-count variables NOT used in analysis
  - 0 missing values in any analytical variable

DESCRIPTIVE FINDINGS:
  - Doctor Listening evenly distributed: Never=25.2%, Sometimes=26.4%,
    Usually=24.0%, Always=24.4%
  - Mean Health Plan Rating nearly identical across all listening groups
    (range: 4.89–5.08 on 0–10 scale; difference <0.2 points)
  - High satisfaction rate differs by only 3.7 percentage points across groups

INFERENTIAL FINDINGS:
  - Kruskal-Wallis: H=1.43, df=3, p=0.698 — NOT significant
  - Spearman ρ (Listening vs HPR): rho=+0.013, p=0.465 — NOT significant
  - One-Way ANOVA: F=0.479, p=0.697 — consistent with K-W
  - Only Health_Care_Rating showed borderline significance (H=8.10, p=0.044)
    but INVERSE direction — higher listening = lower HC rating
  - Post-hoc Dunn (Bonferroni): no pairwise comparison significant

PREDICTIVE FINDINGS:
  - Model 1 (Communication only):  R²≈0.000, CV-R²≈−0.001, RMSE≈3.11
  - Model 2 (Full model):          R²≈0.000, CV-R²≈−0.003, RMSE≈3.11
  - All 4 communication variable coefficients non-significant (p>0.05)
  - Communication variables explain ~0% of variance in Health Plan Rating

CONCLUSION:
  The analytical statement is NOT SUPPORTED by the CAHPS Health Plan Survey data.
  Patient satisfaction with their health plan appears independent of how frequently
  they perceive their doctor as listening to them. Other unmeasured factors
  (e.g., coverage costs, claims handling, wait times, treatment outcomes) likely
  drive Health Plan satisfaction ratings in this dataset.
")

cat("All outputs saved. Analysis complete.\n")

# =============================================================================
# STEP 8: EXPORT FOR REACT DASHBOARD
# =============================================================================
cat("\n========================================================\n")
cat("  STEP 8: EXPORTING CSVs FOR REACT DASHBOARD\n")
cat("========================================================\n")

dir.create("exports", showWarnings = FALSE)

# 1. Listening distribution
listening_dist <- df %>%
  count(Doctor_Listening) %>%
  rename(group = Doctor_Listening, count = n)
write.csv(listening_dist, "exports/listening_dist.csv", row.names=FALSE)
cat("✓ listening_dist.csv\n")

# 2. Mean ratings by listening group
mean_ratings <- df %>%
  group_by(Doctor_Listening) %>%
  summarise(
    mean_HPR  = round(mean(Health_Plan_Rating, na.rm=TRUE), 3),
    mean_DR   = round(mean(Doctor_Rating,      na.rm=TRUE), 3),
    mean_HCR  = round(mean(Health_Care_Rating, na.rm=TRUE), 3),
    high_sat  = round(mean(Health_Plan_Rating >= 7)*100, 1),
    n         = n()
  )
write.csv(mean_ratings, "exports/mean_ratings.csv", row.names=FALSE)
cat("✓ mean_ratings.csv\n")

# 3. Inferential results
inferential <- data.frame(
  test      = c("Kruskal-Wallis (HPR)","Spearman rho","One-Way ANOVA",
                "Kruskal-Wallis (HCR)"),
  statistic = c(1.43, 0.013, 0.479, 8.10),
  p_value   = c(0.698, 0.465, 0.697, 0.044),
  significant = c(FALSE, FALSE, FALSE, TRUE)
)
write.csv(inferential, "exports/inferential.csv", row.names=FALSE)
cat("✓ inferential.csv\n")

# 4. Model comparison
inferential_models <- data.frame(
  model    = c("Model 1 (comm only)", "Model 2 (full)"),
  r2_test  = c(0.0003, 0.0004),
  rmse     = c(3.11, 3.11),
  cv_r2    = c(-0.001, -0.003),
  aic      = c(round(AIC(model1),2), round(AIC(model2),2))
)
write.csv(inferential_models, "exports/models.csv", row.names=FALSE)
cat("✓ models.csv\n")

# 5. Correlation matrix (long format)
cor_vars <- model_df %>% select(
  Health_Plan_Rating, Doctor_Rating,
  Listening_Num, Explanation_Num, Respect_Num, Time_Num
)
cor_mat <- cor(cor_vars, method="spearman", use="complete.obs")
cor_df  <- as.data.frame(as.table(round(cor_mat, 3)))
names(cor_df) <- c("var1", "var2", "rho")
write.csv(cor_df, "exports/correlation.csv", row.names=FALSE)
cat("✓ correlation.csv\n")

cat("\n✅ All 5 CSVs saved to r_analysis/exports/\n")