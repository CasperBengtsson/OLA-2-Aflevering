library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(ggplot2)
library(scales)

##Forbrugertillidsindikator (Erstat fil placering med egen)
dst <- read_excel("C:/Users/helen/Downloads/DST data - DI forbrugertillidsindikator (-2026).xlsx", skip = 2)
names(dst)[1] <- "category"

dst_quarterly <- dst %>%
  pivot_longer(-category, names_to = "period", values_to = "value") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    quarter = as.yearqtr(as.Date(sprintf("%d-%02d-01", year, month)))
  ) %>%
  group_by(quarter) %>%
  summarise(dst_indicator = mean(value, na.rm = TRUE), .groups = "drop")

##Privatforbrug: quarterly QoQ growth -> rolling annual growth
pf <- read_excel("C:/Users/helen/Downloads/DST data - Privatforbrug kædeværdi.xlsx", skip = 2)
names(pf)[3:4] <- c("period", "level")

pf_long <- pf %>%
  select(period, level) %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    q       = as.integer(str_sub(period, 6, 6)),
    quarter = as.yearqtr(year + (q - 1) / 4)
  ) %>%
  arrange(quarter) %>%
  mutate(
    annual_growth = (level / lag(level, 4) - 1) * 100
  )

##Merge the two series
plot_data <- dst_quarterly %>%
  inner_join(pf_long %>% select(quarter, annual_growth), by = "quarter")

##Plot, dual axis like the DI chart
scale_factor <- 25 / 8   # left axis (-25..25) vs right axis (-8..8)

ggplot(plot_data, aes(x = quarter)) +
  geom_col(aes(y = annual_growth * scale_factor),
           fill = "#4FA8D8", width = 0.2) +
  geom_line(aes(y = dst_indicator), color = "grey30", linewidth = 1) +
  scale_y_continuous(
    name = "Nettotal",
    breaks = seq(-50, 50, by = 50/6),
    labels = label_number(accuracy = 0.1, drop0trailing = TRUE),
    sec.axis = sec_axis(~ . / scale_factor, name = "Pct.",
                        breaks = seq(-16, 16, by = 16/6),
                        labels = label_number(accuracy = 0.1, drop0trailing = TRUE))
  ) +
  coord_cartesian(ylim = c(-50, 50)) +          # <- zoom, don't delete
  scale_x_yearqtr(format = "%y", n = 27) +      # more years now on the x-axis too
  labs(
    title = "DI's Forbrugertillidsindikator følger Privatforbruget i fald",
    subtitle = "Forsøg på at matche artiklen fra Baum, 2016",
    caption = "Udarbejdet i R med data hentet fra Danmarkstatistik (FORV1 + NKN1)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

#### EKSTRA: GRAF MED DST DATA

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(ggplot2)
library(scales)

##Forbrugertillidsindikator (Erstat fil placering med egen)
dst <- read_excel("C:/Users/helen/Downloads/DST data - DST forbrugertillidsindikator.xlsx", skip = 2)
names(dst)[1] <- "category"

dst_quarterly <- dst %>%
  pivot_longer(-category, names_to = "period", values_to = "value") %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    month   = as.integer(str_sub(period, 6, 7)),
    quarter = as.yearqtr(as.Date(sprintf("%d-%02d-01", year, month)))
  ) %>%
  group_by(quarter) %>%
  summarise(dst_indicator = mean(value, na.rm = TRUE), .groups = "drop")

##Privatforbrug: quarterly QoQ growth -> rolling annual growth
pf <- read_excel("C:/Users/helen/Downloads/DST data - Privatforbrug kædeværdi.xlsx", skip = 2)
names(pf)[3:4] <- c("period", "level")

pf_long <- pf %>%
  select(period, level) %>%
  mutate(
    year    = as.integer(str_sub(period, 1, 4)),
    q       = as.integer(str_sub(period, 6, 6)),
    quarter = as.yearqtr(year + (q - 1) / 4)
  ) %>%
  arrange(quarter) %>%
  mutate(
    annual_growth = (level / lag(level, 4) - 1) * 100
  )

##Merge the two series
plot_data <- dst_quarterly %>%
  inner_join(pf_long %>% select(quarter, annual_growth), by = "quarter")

##Plot, dual axis like the DI chart
scale_factor <- 50 / 16   # left axis (-50..50) vs right axis (-16..16)

ggplot(plot_data, aes(x = quarter)) +
  geom_col(aes(y = annual_growth * scale_factor),
           fill = "#4FA8D8", width = 0.2) +
  geom_line(aes(y = dst_indicator), color = "grey30", linewidth = 1) +
  scale_y_continuous(
    name = "Nettotal",
    breaks = seq(-50, 50, by = 50/6),
    labels = label_number(accuracy = 0.1, drop0trailing = TRUE),
    sec.axis = sec_axis(~ . / scale_factor, name = "Pct.",
                        breaks = seq(-16, 16, by = 16/6),
                        labels = label_number(accuracy = 0.1, drop0trailing = TRUE))
  ) +
  coord_cartesian(ylim = c(-50, 50)) +
  scale_x_yearqtr(format = "%y", n = 27) +
  labs(
    title = "DST's Forbrugertillidsindikator følger Privatforbruget",
    subtitle = "Forsøg på at matche artiklen fra Baum, 2016",
    caption = "Udarbejdet i R med data hentet fra Danmarkstatistik (FORV1 + NKN1)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )