## ----import_and_encoding-------
library(tidyverse)
library(rio)
library(broom)
library(openxlsx)
library(kableExtra)
p.value <- 0.05
input <- paste0(
    "input/",
    c(
        "stats.txt",
        "freqs.txt",
        "diffs.txt",
        "corrs.txt",
        "age_diffs.txt"
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
    setNames(iconv(colnames(.),
        from = "UTF-8",
        to = "ASCII//TRANSLIT"
    )) %>%
    mutate(Categoria_Idade = if_else(
        Idade <= 20, "20_ou_menos", "acima_de_20"
    ))


## ----normality_tests-----------
shapiro <- function(data, vars) {
    data %>%
        select(all_of(vars)) %>%
        gather(Variável, Valor) %>%
        group_by(Variável) %>%
        summarise(
            n = n(),
            shapiro_w = shapiro.test(Valor)$statistic,
            shapiro_p = shapiro.test(Valor)$p.value,
            normality = if_else(shapiro.test(Valor)$p.value > p.value,
                "parametric", "non-parametric"
            )
        )
}


## ----descriptive_statistics----
calc_stats <- function(data, vars, group_by_var = NULL, overall = TRUE) {
    data %>%
        select(all_of(vars), if (!overall) all_of(group_by_var)) %>%
        gather(Variável, Valor, -if (!overall) group_by_var) %>%
        {
            if (overall) {
                group_by(., Variável)
            } else {
                group_by(., Variável, !!sym(group_by_var))
            }
        } %>%
        summarise(
            n = n(),
            missing = sum(is.na(Valor)),
            normality = if_else(shapiro.test(Valor)$p.value > p.value,
                "parametric", "non-parametric"
            ),
            mean = mean(Valor, na.rm = TRUE),
            median = median(Valor, na.rm = TRUE),
            sd = sd(Valor, na.rm = TRUE),
            se = sd(Valor, na.rm = TRUE) / sqrt(length(Valor)),
            iqr = IQR(Valor, na.rm = TRUE)
        )
}


## ----categorical_variables_frequencies-----
calc_freqs <- function(data, vars) {
    data %>%
        select(Grupo, all_of(vars)) %>%
        gather(Variável, Valor, -Grupo) %>%
        count(Grupo, Variável, Valor) %>%
        group_by(Variável, Grupo) %>%
        mutate(
            Freq_rel = n / sum(n),
            Freq_abs = n
        ) %>%
        select(Variável, Grupo, Valor, Freq_abs, Freq_rel) %>%
        arrange(Variável, Grupo, Valor)
}


## ----group_differences---------
calc_diffs <- function(data, vars, normality, p.value, group_var) {
    map_df(vars, function(var) {
        normality <- filter(normality, Variável == var)
        is_normal <- normality$shapiro_p > p.value
        test_result <- if (is_normal) {
            t.test(data[[var]] ~ data[[group_var]])
        } else {
            wilcox.test(data[[var]] ~ data[[group_var]], conf.int = TRUE)
        }
        effect_size <- if (is_normal) {
            cohen_d(data, var, group_var)
        } else {
            biserial(data, var, group_var)
        }
        tibble(
            Variável = var,
            Teste = if (is_normal) {
                "Welch Two Sample t-test"
            } else {
                "Wilcoxon rank sum test"
            },
            df = ifelse(is_normal, test_result$parameter, NA),
            Stat = test_result$statistic,
            p_value = test_result$p.value,
            Sign = ifelse(test_result$p.value < p.value, "*", "-"),
            Effect = effect_size,
            CI_low = test_result$conf.int[1],
            CI_up = test_result$conf.int[2]
        )
    }) %>%
        arrange(p_value)
}


## ----effect_size---------------
cohen_d <- function(data, var, group_var) {
    group1 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[1]) %>%
        pull(var)
    group2 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[2]) %>%
        pull(var)
    mean_diff <- mean(group1, na.rm = TRUE) - mean(group2, na.rm = TRUE)
    pooled_sd <- sqrt(((length(group1) - 1) * var(group1, na.rm = TRUE) +
        (length(group2) - 1) * var(group2, na.rm = TRUE))
    / (length(group1) + length(group2) - 2))
    cohen_d <- mean_diff / pooled_sd
    return(cohen_d)
}
biserial <- function(data, var, group_var) {
    data <- data %>% mutate(Rank = rank(data[[var]],
        na.last = "keep",
        ties.method = "average"
    ))
    group1 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[1]) %>%
        pull(Rank)
    group2 <- data %>%
        filter(data[[group_var]] == unique(data[[group_var]])[2]) %>%
        pull(Rank)
    rbc <- (sum(group1) / length(group1) - sum(group2) /
        length(group2)) / length(data[[var]])
    return(rbc)
}


## ----variables_correlations----
calc_corrs <- function(data, vars, norms, p.value) {
    combn(vars, 2, simplify = FALSE) %>%
        map_df(function(pair) {
            norms1 <- filter(norms, Variável == pair[1])
            norms2 <- filter(norms, Variável == pair[2])
            is_normal <- norms1$shapiro_p > p.value &
                norms2$shapiro_p > p.value
            corr_result <- if (is_normal) {
                corr_result <- cor.test(data[[pair[1]]], data[[pair[2]]],
                    method = "pearson"
                )
            } else {
                corr_result <- cor.test(data[[pair[1]]], data[[pair[2]]],
                    method = "spearman"
                )
            }
            tibble(
                Variável1 = pair[1],
                Variável2 = pair[2],
                Teste = if (is_normal) "Pearson" else "Spearman",
                df = corr_result$parameter,
                Corr = corr_result$estimate,
                p_value = corr_result$p.value,
                Sign = ifelse(
                    corr_result$p.value < p.value, "*", "-"
                ),
                CI_low = corr_result$conf.int[1],
                CI_up = corr_result$conf.int[2]
            )
        })
}


## ----Export-tables--------------
custom_kable <- function(table_data, ...) {
    table_output <- kable(table_data, ...)
    return(table_output)
}

normality <- shapiro(data, unlist(targets[names(targets) != "freqs"],
    use.names = FALSE
) %>% unique())
custom_kable(normality, digits = 3)

stats <- calc_stats(data, unlist(targets$stats))
custom_kable(stats, digits = 1, align = "lcclrrrrr")


stats <- calc_stats(data, unlist(targets$stats), "Grupo", overall = FALSE)
custom_kable(stats, digits = 1, align = "lcclrrrrr")


freq <- calc_freqs(data, unlist(targets$freq))
custom_kable(freq, digits = 2)


diffs <- calc_diffs(data, unlist(targets$diffs), normality, p.value, "Grupo")
custom_kable(diffs, digits = 2)


corrs <- calc_corrs(data, unlist(targets$corrs), normality, p.value)
custom_kable(corrs, digits = 2)


age_diffs <- calc_diffs(data, unlist(targets$age_diffs), normality, p.value, "Categoria_Idade")
custom_kable(age_diffs, digits = 2)



## ----export_csv, echo=FALSE----
data_list <- list(
    normality,
    stats,
    freq,
    diffs,
    age_diffs,
    corrs,
    filter(corrs, Variável1 == "DNA_corrigido15x")
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
