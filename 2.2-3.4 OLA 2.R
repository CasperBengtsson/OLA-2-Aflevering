library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(ggplot2)

## ---- Husholdningernes forbrugsudgifter: QoQ -> rolling annual growth ----
hh <- read_excel("C:/Users/helen/Downloads/DST data - Husholdningsforbrugsudgifter kædet.xlsx", skip = 2)

hh_long <- hh %>%
  select(-(1:3)) %>%
  pivot_longer(everything(), names_to = "period", values_to = "level") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    q       = as.integer(str_sub(period, 6, 6)),
    quarter = as.yearqtr(year + (q - 1) / 4)
  ) %>%
  arrange(quarter) %>%
  mutate(
    annual_growth = (level / lag(level, 4) - 1) * 100
  )

## ---- DI's forbrugertillidsindikator: average of its 4 categories, quarterly ----
di <- read_excel("C:/Users/helen/Downloads/DST data - DI forbrugertillidsindikator (-2026).xlsx", skip = 2)
names(di)[1] <- "category"

di_quarterly <- di %>%
  pivot_longer(-category, names_to = "period", values_to = "value") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    quarter = as.yearqtr(as.Date(sprintf("%d-%02d-01", year, month)))
  ) %>%
  group_by(category, quarter) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(quarter) %>%
  summarise(di_indicator = mean(value, na.rm = TRUE), .groups = "drop")

## ---- DST's official forbrugertillidsindikator: single series, quarterly average ----
dst <- read_excel("C:/Users/helen/Downloads/DST data - DST forbrugertillidsindikator.xlsx", skip = 2)
names(dst)[1] <- "category"     # only one row, but we name it for pivot_longer to work

dst_quarterly <- dst %>%
  pivot_longer(-category, names_to = "period", values_to = "value") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    quarter = as.yearqtr(as.Date(sprintf("%d-%02d-01", year, month)))
  ) %>%
  group_by(quarter) %>%
  summarise(dst_indicator = mean(value, na.rm = TRUE), .groups = "drop")

## ---- Combine everything into one table, matched by quarter (Maybe remove?) ----
model_data <- hh_long %>%
  select(quarter, annual_growth) %>%
  inner_join(di_quarterly, by = "quarter") %>%
  inner_join(dst_quarterly, by = "quarter")

## ---- Set aside Q3 2026 as the quarter to predict; train on everything else ----
target_quarter <- as.yearqtr("2026 Q3")

new_data_future <- di_quarterly %>%
  inner_join(dst_quarterly, by = "quarter") %>%
  filter(quarter == target_quarter)

pred_di_future  <- predict(model_di,  newdata = new_data_future)
pred_dst_future <- predict(model_dst, newdata = new_data_future)

tibble::tibble(
  method = c("DI's indikator", "DST's indikator"),
  predicted_annual_growth = c(pred_di_future, pred_dst_future)
)

## ---- 3.1 Estimated (fitted) quarterly annual growth rate, from each model ----
koef_di  <- coef(model_di)
koef_dst <- coef(model_dst)

model_data <- model_data %>%
  mutate(
    est_growth_di  = koef_di["(Intercept)"]  + koef_di["di_indicator"]  * di_indicator,
    est_growth_dst = koef_dst["(Intercept)"] + koef_dst["dst_indicator"] * dst_indicator,
    resid_di       = annual_growth - est_growth_di,
    resid_dst      = annual_growth - est_growth_dst
  )

result <- model_data %>% select(quarter, annual_growth, est_growth_di, est_growth_dst)
View(result)

## 3.2

plot_data_resid <- model_data %>%
  filter(!is.na(annual_growth)) %>%
  select(quarter, annual_growth, resid_di, resid_dst) %>%
  pivot_longer(-quarter, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         annual_growth = "Faktisk årlig vækst",
                         resid_di       = "Residual (DI)",
                         resid_dst      = "Residual (DST)"
  ))

ggplot(plot_data_resid, aes(x = quarter, y = value, color = series)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c(
    "Faktisk årlig vækst" = "grey30",
    "Residual (DI)"       = "#4FA8D8",
    "Residual (DST)"      = "#E07B39"
  )) +
  scale_x_yearqtr(format = "%y", n = 27) +
  labs(
    title    = "Residualer fra DI's og DST's indikator-modeller",
    subtitle = "Faktisk årlig vækst i husholdningernes forbrug vs. modellernes residualer",
    x = NULL,
    y = "Procentpoint",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

#3.3

## ---- RSS and TSS for each model ----
eval_data <- model_data %>%
  filter(quarter != target_quarter, !is.na(annual_growth))   # same quarters the models were trained on

TSS <- sum((eval_data$annual_growth - mean(eval_data$annual_growth))^2)

rss_tss_results <- tibble::tibble(
  model = c("DI's indikator", "DST's indikator"),
  RSS   = c(sum(eval_data$resid_di^2), sum(eval_data$resid_dst^2)),
  TSS   = TSS
) %>%
  mutate(R2 = 1 - RSS / TSS)

rss_tss_results

#3.4

## ---- r2, as an equation ----
r_squared <- function(actual, estimated) {
  RSS <- sum((actual - estimated)^2)
  TSS <- sum((actual - mean(actual))^2)
  1 - (RSS / TSS)
}

## ---- Apply the equation to both models ----
eval_data <- model_data %>%
  filter(quarter != target_quarter, !is.na(annual_growth))

r2_results <- tibble::tibble(
  model = c("DI's indikator", "DST's indikator"),
  R2    = c(
    r_squared(eval_data$annual_growth, eval_data$est_growth_di),
    r_squared(eval_data$annual_growth, eval_data$est_growth_dst)
  )
)

r2_results