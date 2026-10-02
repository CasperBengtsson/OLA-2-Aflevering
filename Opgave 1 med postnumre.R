# ==========================================
# Opgave 1.1:
# ==========================================
library(dkstat)
library(dplyr)
library(stringr)

meta_postnr1 <- dst_meta("POSTNR1")
meta_postnr1$variables

# Definerer query til POSTNR1
my_query <- list(
  PNR20 = "*",
  Tid = "2026"
)

dst_data <- dst_get_data(table = "POSTNR1", query = my_query)


# ==========================================
# Opgave 1.2:
# ==========================================
dst_data <- dst_data %>%
  rename(
    indbyggertal = value
  ) %>%
  mutate(
# Trækker kun de 4 første tal ud af teksten:
    postnr = str_extract(PNR20, "^[0-9]{4}"),
    
# Intervaller til bytyper
dst_data <- dst_data %>%
  mutate(bycat = case_when(
    indbyggertal < 1000 ~ "landsby",
    indbyggertal >= 1000 & indbyggertal < 3000 ~ "lille by",
    indbyggertal >= 3000 & indbyggertal < 10000 ~ "almindelig by", 
    indbyggertal >= 10000 & indbyggertal < 25000 ~ "større by",
    indbyggertal >= 25000 ~ "storby" 
  )))

# Konverterer bycat til factor
dst_data$bycat <- as.factor(dst_data$bycat)


# ==========================================
# Opgave 1.3:
# ==========================================
bolig_df <- read.csv2("Bolig_siden.csv", fileEncoding = "UTF-8") 

# Sikrer at postnr i bolig_df læses som tekst (character) for at mache med DST
bolig_df <- bolig_df %>%
  mutate(postnr = as.character(postnr))

# SAMLER DATASÆTTENE PÅ POSTNUMMER
merged_df <- merge(bolig_df, dst_data, by = "postnr", all.x = TRUE)

# Skaber et overblik over hvor mange NA-værdier det drejer sig om
na_overblik <- na_rækker %>%
  count(postnr) %>%
  arrange(desc(n)) # Sorterer, så postnummeret med flest mangler ligger øverst

# Printer listen
print(na_overblik)
View(na_rækker)

# Trækker alle de rækker ud, hvor indbyggertallet ikke kom med over fra DST-data
na_rækker <- merged_df %>% 
  filter(is.na(indbyggertal))

# Beholder alle rækker, hvor bycat IKKE er NA
merged_df_renset <- merged_df[!is.na(merged_df$bycat), ]

# Viser overblik over fordelingen
table(merged_df_renset$bycat)

# ==========================================
# Opgave 1.4
# ==========================================
library(ggplot2)
library(scales)

plot_data <- merged_df_renset

# Låser rækkefølgen af faktorerne til grafen
plot_data$bycat <- factor(
  plot_data$bycat, 
  levels = c("landsby", "lille by", "almindelig by", "større by", "storby")
)

# Graf 1: Antal boliger pr. bytype
ggplot(plot_data, aes(x = bycat, fill = bycat)) +
  geom_bar() +
  geom_text(
    stat = "count", 
    aes(
      label = label_percent(accuracy = 0.1)(after_stat(prop)), 
      group = 1
    ),
    vjust = -0.5,     
    fontface = "bold" 
  ) +
  labs(title = "Der er flest boliger til salg i bytypen større by", 
       x = "Bytype", 
       y = "Antal", 
       caption = "Kilde: Danmarks Statistik (POSTNR1). Bolig_siden webscraping.") +
  theme_minimal() + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray"))

# Rensning af kvadratmeterpris for at fjerne punktummer før gennemsnittet beregnes
plot_data <- plot_data %>%
  mutate(
    kvmpris = as.numeric(str_remove_all(kvmpris, "\\."))
  )

# Graf 2: Kvadratmeterpris pr. bytype
ggplot(plot_data, aes(x = bycat, y = kvmpris, fill = bycat)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", suffix = " kr.")
  ) +
  labs(
    title = "Den gennemsnitlige kvm2 er højest i kategorien storby", 
    x = "Bytype", 
    y = "Gns. Kvadratmeterpris",
    caption = "Kilde: Danmarks Statistik (POSTNR1). Bolig_siden webscraping.",
    fill = "Bytype"
  ) +
  theme_minimal() + 
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray"))
