parametric_corr_test <- "pearson" # Pearson's product-moment correlation
parametric_corr_test <- "spearman" # Spearman's rank correlation
parametric_diff_test <- "t-test" # Welch's two-sample t-test
non_parametric_diff_test <- "wilcox" # Wilcoxon rank sum test/Mann–Whitney U test

Consider the code below for running statistical data for a clinical trial. I need to output my analysis to a PDF documents, where I explain the statistical tests conducted and each of the resulting outputs. I need you to make suggstions on how to approach this, and if you believe it is a good idea and best approach, give me an rmarkdown file for achieving this. Otherwise, make your suggestions. Also, make sure to include in your suggestion all information that can be infered from the script.

library(tidyverse)
library(rio)
library(broom)
library(openxlsx)

p.value <- 0.05

input <- paste0(
    "input/",
    c(
        "target-variables-descriptive-stats.csv",
        "target-variables-frequencies.csv",
        "target-variables-group-differences.csv",
        "target-variables-correlations.csv",
        "target-variables-age-differences.csv"
    )
)

read <- function(path) {
    names(import(path, setclass = "tibble"))
}

encoding <- function(data) {
    lapply(data, function(x) {
        iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    })
}

targets <- lapply(input, read) %>% lapply(encoding)
names(targets) <- c("stats", "freqs", "diffs", "corrs", "age_diffs")

data <- import("input/data.xlsx", setclass = "tibble", na = "NA") %>%
    setNames(iconv(colnames(.), from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(Categoria_Idade = if_else(Idade <= 20, "20_ou_menos", "acima_de_20"))

shapiro <- function(data, vars) {
    data %>%
        select(all_of(vars)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            n = n(),
            shapiro_w = shapiro.test(Value)$statistic,
            shapiro_p = shapiro.test(Value)$p.value,
            normality = if_else(shapiro.test(Value)$p.value > p.value, "parametric", "non-parametric")
        )
}

calc_stats <- function(data, vars) {
    data %>%
        select(all_of(vars)) %>%
        gather(Variable, Value) %>%
        group_by(Variable) %>%
        summarise(
            n = n(),
            missing = sum(is.na(Value)),
            normality = if_else(shapiro.test(Value)$p.value > p.value, "parametric", "non-parametric"),
            mean = mean(Value, na.rm = TRUE),
            median = median(Value, na.rm = TRUE),
            sd = sd(Value, na.rm = TRUE),
            se = sd(Value, na.rm = TRUE) / sqrt(length(Value)),
            iqr = IQR(Value, na.rm = TRUE)
        )
}

calc_freqs <- function(data, vars) {
    data %>%
        select(Grupo, all_of(vars)) %>%
        gather(Variable, Value, -Grupo) %>%
        count(Grupo, Variable, Value) %>%
        group_by(Variable, Grupo) %>%
        mutate(
            Freq_rel = n / sum(n),
            Freq_abs = n
        ) %>%
        select(Variable, Value, Grupo, Freq_abs, Freq_rel)
}

calc_diffs <- function(data, vars, normality, p.value, group_var) {
    map_df(vars, function(var) {
        normality <- filter(normality, Variable == var)
        is_normal <- normality$shapiro_p > p.value
        test_result <- if (is_normal) t.test(data[[var]] ~ data[[group_var]]) else wilcox.test(data[[var]] ~ data[[group_var]], conf.int = TRUE)
        effect_size <- if (is_normal) {
            cohen_d(data, var, group_var)
        } else {
            biserial(data, var, group_var)
        }
        tibble(
            Variable = var,
            Test = test_result$method,
            df = ifelse(is_normal, test_result$parameter, NA),
            Statistic = test_result$statistic,
            p.value = test_result$p.value,
            Significance = ifelse(test_result$p.value < p.value, "*", "-"),
            Effect_Size = effect_size,
            CI_min = test_result$conf.int[1],
            CI_max = test_result$conf.int[2]
        )
    })
}

calc_corrs <- function(data, vars, norms, p.value) {
    combn(vars, 2, simplify = FALSE) %>%
        map_df(function(pair) {
            norms1 <- filter(norms, Variable == pair[1])
            norms2 <- filter(norms, Variable == pair[2])
            is_normal <- norms1$shapiro_p > p.value & norms2$shapiro_p > p.value
            corr_result <- if (is_normal) cor.test(data[[pair[1]]], data[[pair[2]]], method = "pearson") else cor.test(data[[pair[1]]], data[[pair[2]]], method = "spearman")
            tibble(
                Variable1 = pair[1],
                Variable2 = pair[2],
                Test = corr_result$method,
                df = corr_result$parameter,
                Correlation = corr_result$estimate,
                p.value = corr_result$p.value,
                Significance = ifelse(corr_result$p.value < p.value, "*", "-"),
                corr_CI_lower = corr_result$conf.int[1],
                corr_CI_upper = corr_result$conf.int[2]
            )
        })
}

cohen_d <- function(data, var, group_var) {
    group1 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[1]) %>%
        pull(var)
    group2 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[2]) %>%
        pull(var)
    mean_diff <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
    pooled_sd <- sqrt(((length(group1) - 1) * var(group1, na.rm = TRUE) + (length(group2) - 1) * var(group2, na.rm = TRUE)) / (length(group1) + length(group2) - 2))
    cohen_d <- mean_diff / pooled_sd
    return(cohen_d)
}

biserial <- function(data, var, group_var) {
    data <- data %>% mutate(Rank = rank(data[[var]], na.last = "keep", ties.method = "average"))
    group1 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[1]) %>%
        pull(Rank)
    group2 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[2]) %>%
        pull(Rank)
    rbc <- (sum(group1) / length(group1) - sum(group2) / length(group2)) / length(data[[var]])
    return(rbc)
}

normality <- shapiro(data, unlist(targets[names(targets) != "freqs"], use.names = FALSE) %>% unique())
stats <- calc_stats(data, unlist(targets$stats))
freq <- calc_freqs(data, unlist(targets$freq))
diffs <- calc_diffs(data, unlist(targets$diffs), normality, p.value, "Grupo")
age_diffs <- calc_diffs(data, unlist(targets$age_diffs), normality, p.value, "Categoria_Idade")
corrs <- calc_corrs(data, unlist(targets$corrs), normality, p.value)

# Save data
data_list <- list(
    normality,
    stats,
    freq,
    diffs,
    age_diffs,
    corrs,
    filter(corrs, Variable1 == "DNA_corrigido15x")
)

data_file_names <- c(
    "01_normality-tests.csv",
    "02_descriptive-statistics.csv",
    "03_cat-variables-frequency.csv",
    "04_group-differences.csv",
    "05_differences-by-age.csv",
    "06_variables-correlations.csv",
    "07_DNA_corrigido15x-corrs.csv"
)

dir.create("output", showWarnings = FALSE)
for (i in seq_along(data_list)) {
    write_csv(data_list[[i]], paste0("output/", data_file_names[[i]]))
}

output_workbook <- createWorkbook()
walk2(data_list, data_file_names, ~ {
    addWorksheet(output_workbook, sheetName = .y)
    writeData(output_workbook, sheet = .y, .x)
})
saveWorkbook(output_workbook, "output/results.xlsx", overwrite = TRUE)

# rm(list = Filter(function(x) !inherits(get(x), "tbl"), ls()))
