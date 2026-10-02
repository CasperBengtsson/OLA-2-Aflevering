library(dkstat)
library(dplyr)
library(tidyverse)
library(tidyr)
library(scales)
library(zoo)
library(ggplot2)

#Opgave 4.1
meta_forv1 <- dst_meta("FORV1")
meta_forv1$variables
meta_forv1$values$INDIKATOR

query_opg1 <- list(
  INDIKATOR = "Forbrugertillidsindikatoren",
  Tid = "*"
)
forv1_data <- dst_get_data(table = "FORV1", query = query_opg1)

forv1_1996 <- forv1_data %>%
  filter(TID >= as.Date("1996-01-01"))

# Filtrer data fra 1996 og frem og omregner til kvartal
ftillid_ts <- ts(forv1_1996$value, start = c(1996, 1), frequency = 12) 
ftillid_kvartal <- aggregate(ftillid_ts, nfrequency = 4) / 3
View(ftillid_kvartal)

# Konvertere tidsserien til en dataframe
df_plot <- data.frame(
  Tid = as.numeric(time(ftillid_kvartal)), 
  Vaerdi = as.numeric(ftillid_kvartal))

# Bygger grafen
ggplot(df_plot, aes(x = Tid, y = Vaerdi)) +
  
  # Tilføjer en vandret stiplet nul-linje for at vise balancepunktet
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
  
  # Tegner selve dataen som en flot blå linje
  geom_line(color = "#0088cc", size = 1) + 
  
  # Sætter et mærke på x-aksen for hvert 4. år for at øge læsbarheden
  scale_x_continuous(breaks = seq(1996, 2026, by = 4)) + 
  
  # Gør baggrunden ren og fjerner det grå gitter
  theme_classic() + 
  
  # Tilføjer læsevenlige titler og kilde
  labs(
    title = "Danskerne var mest optemistikse mellem 2005 & 2007 og mest pessimistiske mellem 2022 & 2023",
    subtitle = "Forbrugertillidsindikatoren (1996 - 2026)",
    x = "Årstal",
    y = "Forbrugertillid",
    caption = "Kilde: Danmarks Statistik (FORV1)"
  ) +
  
  # Ekstra visuel finpudsning af titlen
  theme(plot.title = element_text(face = "bold", size = 14))

#Opgave 4.2
meta_forv1 <- dst_meta("FORV1")
meta_forv1$variables
meta_forv1$values$INDIKATOR

# Definer forespørgsel for underspørgsmålet
query_opg2 <- list(
  INDIKATOR = "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket", 
  TID = "*" 
)

# Henter data
df_goder <- dst_get_data(table = "FORV1", query = query_opg2)

forv1_goder_2000 <- df_goder %>%
  filter(TID >= as.Date("2000-01-01"))

# Beregner gennemsnit
gennemsnit_goder <- mean(forv1_goder_2000$value, na.rm = TRUE)
print(gennemsnit_goder)

#Opgave 4.3
meta_forbrug <- dst_meta("NKHC021")
meta_forbrug$variables
meta_forbrug$values$SÆSON

query_opg3 <- list(
  FORMAAAL = c("Fødevarer mv.", "Drikkevarer og tobak mv.", "Beklædning og fodtøj", "Boligbenyttelse", "Elektricitet, fjernvarme og andet brændsel", "Boligudstyr, husholdningstjenester mv.", "Medicin, lægeudgifter o.l.", "Køb af køretøjer", "Drift af køretøjer og transporttjenester", "Information og kommunikation", "Fritid, sport og kultur", "Undervisning", "Restauranter og hoteller", "Forsikring og finansielle tjenester", "Andre varer og tjenester"), 
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*" 
)

df_forbrug <- dst_get_data(table = "NKHC021", query = query_opg3)

df_forbrug_2020 <- df_forbrug %>%
  filter(TID >= as.Date("2020-01-01"))

df_forbrug_bred <- tidyr::spread(df_forbrug_2020, key = TID, value = value)

# Hvad brugte danskerne flest penge på i 2022?
df_forbrug_2022 <- df_forbrug_bred %>%
  mutate(
    gns_2022 = rowMeans(select(., `2022-01-01`, `2022-04-01`, `2022-07-01`, `2022-10-01`), na.rm = TRUE)
  ) %>%
  select(FORMAAAL, gns_2022) %>%
  arrange(desc(gns_2022))
head(df_forbrug_2022)

# Vi bruger 'head()' til at tage de 10 største formål (da datasættet allerede er sorteret)
plot_data_2022 <- head(df_forbrug_2022, 10)

ggplot(plot_data_2022, aes(x = reorder(FORMAAAL, gns_2022), y = gns_2022)) +
  # Laver søjlerne (geom_col er lig geom_bar(stat = "identity"))
  geom_col(fill = "steelblue") +
  
  # Vender grafen 90 grader, så formålene står på y-aksen og kan læses
  coord_flip() +
  
  # Sætter gennemsnittet som tekst for enden af søjlen (afrundet til hele tal)
  geom_text(aes(label = round(gns_2022, 0)), hjust = -0.2, size = 3.5) +
  
  labs(
    title = "Danskerne brugte flest penge på Boligbenyttelse i 2022",
    subtitle = "Top 10 forbrugskategorier målt på gennemsnitligt kvartalsforbrug",
    x = "", 
    y = "Gennemsnitligt forbrug",
    caption = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray")) +
  
  # Udvider y-aksen en smule, så teksten for enden af søjlerne ikke bliver skåret af
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# Hvilken gruppe steg mest fra 2020 til 2026? - Sammenligner 1. kvartal 2020 med 2. kvartal 2026
df_forbrug_vaekst <- df_forbrug_bred %>%
  mutate(
    vaekst_pct = ((`2026-04-01` - `2020-01-01`) / `2020-01-01`) * 100
  ) %>%
  select(FORMAAAL, vaekst_pct) %>%
  arrange(desc(vaekst_pct))
head(df_forbrug_vaekst)

# Vi tager de 10 formål med højest vækst
plot_data_vaekst <- head(df_forbrug_vaekst, 10)

ggplot(plot_data_vaekst, aes(x = reorder(FORMAAAL, vaekst_pct), y = vaekst_pct)) +
  # Tegner "pinden"
  geom_segment(aes(x = reorder(FORMAAAL, vaekst_pct), xend = FORMAAAL, 
                   y = 0, yend = vaekst_pct), color = "gray", linewidth = 1) +
  
  # Tegner "slikkepinden" (prikken for enden)
  geom_point(color = "darkgreen", size = 4) +
  
  # Vender grafen vandret
  coord_flip() +
  
  # Sætter procenterne på for enden, afrundet til 1 decimal med et %-tegn
  geom_text(aes(label = paste0(round(vaekst_pct, 1), "%")), 
            hjust = -0.5, fontface = "bold") +
  
  labs(
    title = "Forbrugsgruppen Beklædning og fodtøj er steget mest i perioden",
    subtitle = "Procentvis vækst fra 1. kvartal 2020 til 2. kvartal 2026",
    x = "",
    y = "Vækst i procent (%)",
    caption = "Kilde: Danmarks Statistik"
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray"))
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

  #Opgave 4.4
#DST forbrugertillidsindikator
forv1_2000 <- forv1_data %>%
  filter(TID >= as.Date("2000-01-01"))

ftillid_ts_DST <- ts(forv1_2000$value, start = c(2000, 1), frequency = 12) 
ftillid_kvartal_DST <- aggregate(ftillid_ts_DST, nfrequency = 4) / 3

#DI Forbrugertillidsindikator
meta_forv1_DI <- dst_meta("FORV1")
meta_forv1_DI$variables
meta_forv1_DI$values$INDIKATOR

query_opg4 <- list(
  INDIKATOR = c("Familiens økonomiske situation i dag, sammenlignet med for et år siden", "Danmarks økonomiske situation i dag, sammenlignet med for et år siden", "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket", "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."),
  Tid = "*"
)
forv1_data_DI <- dst_get_data(table = "FORV1", query = query_opg4)

forv1_2000_DI <- forv1_data_DI %>%
  filter(TID >= as.Date("2000-01-01"))

forv1_2000_DI_gns <- forv1_2000_DI %>%
  group_by(TID) %>%
  summarise(value = mean(value, na.rm = TRUE))
view(forv1_2000_DI_gns)

ftillid_ts_DI <- ts(forv1_2000_DI_gns$value, start = c(2000, 1), frequency = 12) 
ftillid_kvartal_DI <- aggregate(ftillid_ts_DI, nfrequency = 4) / 3

df_forbrug_2000 <- df_forbrug %>%
  filter(TID >= as.Date("2000-01-01"))

df_forbrug_bred_2000 <- tidyr::spread(df_forbrug_2000, key = FORMAAAL, value = value) %>%
  arrange(TID)

# Gør DST tidsserien til en dataframe med en datokolonne
df_DST_tillid <- data.frame(
  TID = as.Date(as.yearqtr(time(ftillid_kvartal_DST))), # Sikrer at formatet er en dato
  DST_tillid = as.numeric(ftillid_kvartal_DST))

# Gør DI tidsserien til en dataframe med en datokolonne
df_DI_tillid <- data.frame(
  TID = as.Date(as.yearqtr(time(ftillid_kvartal_DI))),
  DI_tillid = as.numeric(ftillid_kvartal_DI))

# Join det hele sammen med left_join
df_forbrug_bred_2000 <- df_forbrug_bred_2000 %>%
  left_join(df_DST_tillid, by = "TID") %>%
  left_join(df_DI_tillid, by = "TID")

# Opreter to tomme lister til at gemme resultaterne
summary_DST <- list()
summary_DI <- list()

# Finder navnene på alle forbrugsgrupper automatisk.
forbrugsgrupper <- setdiff(names(df_forbrug_bred_2000), c("PRISENHED", "SÆSON", "TID", "DST_tillid", "DI_tillid"))
view(forbrugsgrupper)

# Loopet der kører regressionerne
for (kategori in forbrugsgrupper) {
  
  # Træk y-variablen (den specifikke forbrugsgruppe)
  y_forbrug <- df_forbrug_bred_2000[[kategori]]
  
  # Kør de to lineære regressioner
  model_dst <- lm(y_forbrug ~ DST_tillid, data = df_forbrug_bred_2000)
  model_di  <- lm(y_forbrug ~ DI_tillid, data = df_forbrug_bred_2000)
  
  # Gem summary-outputtet i de to lister
  summary_DST[[kategori]] <- summary(model_dst)
  summary_DI[[kategori]]  <- summary(model_di)
}

# Tjek resultatet for den første gruppe
summary_DST
summary_DI

# Vælg den kategori der skal visualisere & sørg for at navnet matcher 100%
valgt_kategori <- forbrugsgrupper[1] 

# Træk R-squared ud fra begge lister ved hjælp af et lille loop
r2_dst <- sapply(summary_DST, function(x) x$r.squared)
r2_di <- sapply(summary_DI, function(x) x$r.squared)

# Samler resultaterne i en dataframe
df_r2 <- data.frame(
  Kategori = names(r2_dst),
  DST_Tillid = r2_dst,
  DI_Tillid = r2_di
)

# Gør dataen 'lang' (Tidy) for at kunne bygge grupperede søjler i ggplot
df_r2_lang <- df_r2 %>%
  pivot_longer(cols = c(DST_Tillid, DI_Tillid), 
               names_to = "Indikator", 
               values_to = "R_squared")

ggplot(df_r2_lang, aes(x = Indikator, y = R_squared, fill = Indikator)) +
  geom_col() +
  
  library(scales) # Husk denne i toppen af dit script, ellers virker label_percent() ikke

ggplot(df_r2_lang, aes(x = Indikator, y = R_squared, fill = Indikator)) +
  geom_col() +
  
  # TILFØJELSE: Skriver procenttallet ud for enden af hver søjle
  geom_text(
    aes(label = label_percent(accuracy = 0.1)(R_squared)), # Omdanner tallet til procent med 1 decimal
    vjust = -0.3,  # Løfter teksten en lille smule op, så den svæver flot over søjlen
    size = 3,      # Gør teksten lidt mindre, så den passer ind i de små undergrafer
    fontface = "bold" # Gør teksten nemmere at aflæse
  ) +
  
  # Deler plottet op i små grafer for hver forbrugsgruppe
  facet_wrap(~ Kategori, ncol = 3) + 
  
  # Forlænger y-aksen en smule i toppen, så procenterne ikke rammer loftet i undergraferne
  scale_y_continuous(
    labels = label_percent(), 
    expand = expansion(mult = c(0, 0.2))
  ) + 
  
  labs(
    title = "Danmarks Statistiks tillidsindikator korrelerer mest med de 15 forbrugsgrupper",
    x = NULL, 
    y = "R-squared",
    caption = "Kilde: Danmarks Statistik. Dansk Industri",
    fill = "Model"
  ) +
  theme_minimal() +
  
  # RETTET: Tilføjet et + til sidst på denne linje
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray")) +
  
  scale_fill_manual(values = c("DST_Tillid" = "steelblue", "DI_Tillid" = "darkorange")) +
  
  theme(
    axis.text.x = element_blank(), 
    strip.text = element_text(size = 8, face = "bold") 
  )
