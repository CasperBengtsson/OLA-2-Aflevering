library(eurostat)
library(restatapi)
library(dplyr)
library(ggplot2)

allTabs <- get_eurostat_toc()

#Hent metadata
df_euro_data=get_eurostat_dsd("namq_10_gdp")

#Undersøg metadata mhp filtrering
unique(df_euro_data$concept)

#Undersøg hver variabel for vædier
df_euro_data |> filter(concept=="freq")
df_euro_data |> filter(concept=="unit")
df_euro_data |> filter(concept=="s_adj")
df_euro_data |> filter(concept=="na_item")
df_euro_data |> filter(concept=="geo")

# lav filtre
landekoder=c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES")

myeuquery=list(
  na_item="P31_S14", # Husholdningernes forbrug
  unit="CLV_PCH_SM",     # Kvartalsvis årlig realvækst (år-til-år procentvis ændring)
  s_adj="SCA",       # Sæsonkorrigeret
  time = ">2000-01-01",
  geo = landekoder
) 
df_euro<-get_eurostat_data("namq_10_gdp", 	
                         filters=myeuquery)

# Sorter dataene kronologisk pr. land (vigtigt for at lag() udregningen rammer rigtigt)
df_euro <- df_euro[order(df_euro$geo, df_euro$time), ]

# Hvordan man udtrækker data specifikke lande man vil fremhæve
df_dk <- subset(df_euro, geo == "DK")
df_de <- subset(df_euro, geo == "DE")

# Opsætter grafvinduet til at vise 2 grafer under hinanden (2 rækker, 1 kolonne)
# 'mar' justerer marginerne (bund, venstre, top, højre), så der er plads til teksten.
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# Plot den første graf for Danmark
# type = "l" fortæller Base R, at det skal være en linje og ikke punkter.
plot(df_dk$time, df_dk$values, type = "l", col = "blue", lwd = 2,
     main = "Danmark: Kvartalsvis Årlig Realvækst",
     xlab = "", ylab = "Realvækst (%)")

# Tilføjer en nul-linje for at vise, hvornår økonomien falder (<0)
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# Plot den anden graf for Tyskland
plot(df_de$time, df_de$values, type = "l", col = "darkgreen", lwd = 2,
     main = "Tyskland: Kvartalsvis Årlig Realvækst",
     xlab = "Årstal", ylab = "Realvækst (%)")

abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# Kan nulstille layoutet bagefter, så fremtidige plots ikke også bliver splittet i to
par(mfrow = c(1, 1))

# ==========================================
# Opgave 5.2
# ==========================================

# Udregner gennemsnit per land (bruger nu 'values')
gns_total <- aggregate(values ~ geo, data = df_euro, FUN = mean)

# Sortere datasættet for at finde topscoreren (minus foran betyder faldende sortering)
gns_total <- gns_total[order(-gns_total$values), ]

print("Landet med den højeste gennemsnitlige realvækst (2000 - nu):")
print(head(gns_total, 1))

ggplot(gns_total, aes(x = reorder(geo, -values), y = values)) +
  geom_col(fill = "steelblue") +
  
  # Tilføjer tallene på toppen af søjlerne med procenttegn
  geom_text(
    aes(label = paste0(round(values, 2), " %")), 
    vjust = -0.5, 
    size = 4,
    fontface = "bold"
  ) +
  
  labs(
    title = "Sverige (SE) har haft den højeste gennemsnitlige kvartalvise årlige realvækst",
    subtitle = "Gennemsnitlig realvækst pr. land (fra år 2000 og frem)",
    x = "Land",
    y = "Gns. Kvartalvise årlige realvækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

# ==========================================
# Opgave 5.3
# ==========================================

# Fjerner Coronaperioden (Vi beholder tiden FØR 2020 og efter 2021)
df_euro_uden_corona <- subset(df_euro, !grepl("2020|2021", time))

# Udregner det nye gennemsnit
gns_uden_corona <- aggregate(values ~ geo, data = df_euro_uden_corona, FUN = mean)

# Sammenligner effekten ved at merge de to gennemsnit
sammenligning <- merge(gns_total, gns_uden_corona, by = "geo", suffixes = c("_Total", "_UdenCorona"))

# Udregner differencen (Vi trækker det høje "normale" niveau FRA det lave totale niveau for at vise tabet)
sammenligning$Effekt <- sammenligning$values_Total - sammenligning$values_UdenCorona

# Sorterer data stigende (mest negative tal først) for at finde den største nedtur
sammenligning <- sammenligning[order(sammenligning$Effekt), ]
print("Landet hvor Corona-outlieren havde størst negativ påvirkning (største procentpoint fald):")
print(head(sammenligning, 1))

# De to gennemsnit samles i én kolonne, så ggplot kan farveopdele dem
plot_data_grupperet <- tidyr::pivot_longer(
  sammenligning,
  cols = c(values_Total, values_UdenCorona),
  names_to = "Periode",
  values_to = "Vaerdi"
)

# Vi omdøber teksten i dataet, så det ser pænt ud i grafens farveforklaring
plot_data_grupperet$Periode <- ifelse(
  plot_data_grupperet$Periode == "values_Total",
  "Total (inkl. Corona)",
  "Uden Corona (ekskl. 2020-2021)"
)

# Vi bygger det grupperede søjlediagram og sorterer x-aksen efter 'Effekt', så topscorerne i tab (de mest negative) står til venstre
ggplot(plot_data_grupperet, aes(x = reorder(geo, Effekt), y = Vaerdi, fill = Periode)) +
  
  # position = position_dodge() sætter søjlerne side om side i stedet for oven på hinanden
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  
  # Vælger farver (rød for totalen, blå for den rensede data)
  scale_fill_manual(
    values = c("Total (inkl. Corona)" = "indianred", 
               "Uden Corona (ekskl. 2020-2021)" = "steelblue")
  ) +
  
  labs(
    title = "Spanien er blevet påvirket mest negativt af Corona-krisen",
    subtitle = "Landene er sorteret fra venstre mod højre efter, hvor stort det økonomiske tab var",
    x = "Land",
    y = "Gennemsnitlig Realvækst (%)",
    caption = "Kilde: Eurostat",
    fill = "Datakilde:"
  ) +
  
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray"),
        legend.position = "top")

# ==========================================
# Opgave 5.4
# ==========================================

# Isolér tiden fra 2020 og frem
df_2020_og_frem <- subset(df_euro, as.numeric(substr(as.character(time), 1, 4)) >= 2020)

# Udregner gennemsnit
gns_2020_og_frem <- aggregate(values ~ geo, data = df_2020_og_frem, FUN = mean)

# Sortere stigende for at finde det mest negative gennemsnit (det største fald)
gns_2020_og_frem <- gns_2020_og_frem[order(gns_2020_og_frem$values), ]

print("Landet med det hårdeste fald/laveste vækst i Corona-årene:")
print(head(gns_2020_og_frem, 1))

# Vi tilføjer først en midlertidig kolonne for at tjekke, om tallet er over eller under 0
gns_2020_og_frem$Positiv_vaekst <- gns_2020_og_frem$values >= 0

# Vi bygger grafen og sorterer fra laveste til højeste tal
ggplot(gns_2020_og_frem, aes(x = reorder(geo, values), y = values, fill = Positiv_vaekst)) +
  
  # geom_col laver søjlerne
  geom_col() +
  
  # scale_fill_manual styrer farverne: Rød for minus, blå for plus
  scale_fill_manual(values = c("FALSE" = "indianred", "TRUE" = "steelblue"), guide = "none") +
  
  # Tilføjer en tydelig nul-linje, så det er nemt at se grænsen mellem vækst og fald
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  
  # geom_text placerer tallene
  geom_text(
    aes(
      label = paste0(round(values, 2), " %"),
      vjust = ifelse(values >= 0, -0.5, 1.5)
    ),
    size = 4,
    fontface = "bold"
  ) +
  
  labs(
    title = "Danmark (DK) har haft den svageste gennemsnitlige kvartalvise realvækst efter Corona (2020-nu)",
    subtitle = "Gennemsnitlig realvækst sorteret fra det hårdeste fald til den højeste vækst",
    x = "Land",
    y = "Gns. Realvækst (%)",
    caption = "Kilde: Eurostat"
  ) +
  
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, face = "italic", color = "darkgray")) +
  
  # Udvider y-aksen i både top og bund
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.15)))
