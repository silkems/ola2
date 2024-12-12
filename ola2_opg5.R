# Pakker og biblioteker -------------------------------------------------------
library(eurostat)
library(restatapi)
library(RColorBrewer)
library(tidyverse)

#### Opgave 5 – Eurostat og API ####

#### Opgave 5.1 – Kvartalsvis årlig realvækst for en række Eurolande ####


# Hent og forbered data -------------------------------------------------------

# Hent metadata for Eurostat datasættet
namq_10_gdp <- get_eurostat_dsd("namq_10_gdp")

# Filtrér data for husholdningernes forbrug, sæsonkorrigeret
namq.filter <- get_eurostat(
  id = "namq_10_gdp",
  filters = list(
    geo = c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES"),
    na_item = "P31_S14",
    s_adj = "SCA"
  ),
  time_format = "date"
)

# Omdan data til bredt format, så hvert land har sin egen kolonne
wide_data <- namq.filter %>%
  select(time, geo, values) %>%
  pivot_wider(names_from = geo, values_from = values, values_fn = list(values = ~ mean(.x, na.rm = TRUE)))

# Filtrér data for perioden 1999Q1 - 2024Q3
wide.data.df <- wide_data[c(97:199), ]

# Beregn årlig realvækst ------------------------------------------------------

# Loop igennem alle lande og beregn årlig realvækst
for (i in 2:ncol(wide.data.df)) {
  col_name <- names(wide.data.df)[i]
  wide.data.df[[paste0(col_name, "_Realvækst")]] <- ifelse(
    !is.na(wide.data.df[[col_name]]) & !is.na(dplyr::lag(wide.data.df[[col_name]], 4)),
    (wide.data.df[[col_name]] / dplyr::lag(wide.data.df[[col_name]], 4) - 1) * 100,
    NA
  )
}

# Saml realvækstdata i en ny tabel
realvækst.total <- wide.data.df[c(5:103), c(11:19)]
colnames(realvækst.total) <- substr(colnames(realvækst.total), 1, 2)  # Forkort kolonnenavne
realvækst.total <- cbind(wide.data.df[c(5:103), c(1)], realvækst.total)  # Tilføj tidskolonne

# Visualiser realvækst over tid ------------------------------------------------

# Konverter til langt format for lettere plotning
long_data <- realvækst.total %>%
  pivot_longer(cols = -time, names_to = "Land", values_to = "Realvækst")

# Lav fulde landenavne og brug dem direkte i en samlet pipeline
full_country_names <- c(
  "DK" = "Danmark",
  "BE" = "Belgien",
  "NL" = "Nederlandene",
  "SE" = "Sverige",
  "AT" = "Østrig",
  "DE" = "Tyskland",
  "FR" = "Frankrig",
  "IT" = "Italien",
  "ES" = "Spanien"
)

# Plot med fulde landenavne i én pipeline
ggplot(
  long_data %>%
    mutate(Land = recode(Land, !!!full_country_names)),
  aes(x = time, y = Realvækst, color = Land, group = Land)
) +
  geom_line(size = 1) +
  labs(
    title = "Landenes realvækst følger i høj grad hinanden",
    subtitle = "Realvækst varierer over tid, men viser fælles mønstre mellem landene",
    x = "År",
    y = "Realvækst (%)",
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  scale_color_brewer(palette = "Paired") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  guides(color = guide_legend(override.aes = list(size = 2)))


#### Opgave 5.2 – Højeste kvartalsvise årlige realvækst ####

# Beregn gennemsnitlig realvækst ------------------------------------------------

# Beregn gennemsnitlig realvækst for hvert land
gennemsnit_vækst <- sapply(realvækst.total[-1], function(x) mean(x, na.rm = TRUE))
gennemsnit.total.df <- data.frame(Land = names(gennemsnit_vækst), Gns = gennemsnit_vækst)

# Opdater Land-navne til fulde landenavne
full_country_names <- c(
  "DK" = "Danmark",
  "BE" = "Belgien",
  "NL" = "Nederlandene",
  "SE" = "Sverige",
  "AT" = "Østrig",
  "DE" = "Tyskland",
  "FR" = "Frankrig",
  "IT" = "Italien",
  "ES" = "Spanien"
)

# Indsæt fulde landenavne
gennemsnit.total.df$Land <- recode(gennemsnit.total.df$Land, !!!full_country_names)

# Opret faktor med rækkefølge baseret på Gns
gennemsnit.total.df$Land <- factor(gennemsnit.total.df$Land, levels = gennemsnit.total.df$Land[order(gennemsnit.total.df$Gns)])

# Plot gennemsnitlig realvækst med fulde landenavne
ggplot(gennemsnit.total.df, aes(x = Land, y = Gns, fill = Land)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Gns, 2)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Sverige har den højeste realvækst i husholdningernes forbrugsudgift",
    y = "Gns kvartalsvise realvækst (%) fra 2000Q1 til 2024Q3",
    x = NULL,
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired", breaks = levels(gennemsnit.total.df$Land)) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right",
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50")  # Venstrejuster kilden
  )




#### Opgave 5.3 – Coronakrisen som outlier ####
# Fjern corona-perioden og genberegn gennemsnit --------------------------------

# Fjern corona-perioden (2020Q1 - 2022Q2)
vækst.minus.corona <- realvækst.total[-c(81:90), ]

# Beregn gennemsnit uden corona-perioden
gns.minus.corona <- colMeans(vækst.minus.corona[-1], na.rm = TRUE)
gennemsnit.minus.corona.df <- data.frame(Land = names(gns.minus.corona), GnsUdenCorona = gns.minus.corona)

# Sammenlign gennemsnit med og uden corona ------------------------------------

# Sammenlign data
gns.sammenligning <- cbind(gennemsnit.total.df, GnsUdenCorona = gennemsnit.minus.corona.df$GnsUdenCorona)
gns.sammenligning$difference <- gns.sammenligning$Gns - gns.sammenligning$GnsUdenCorona

# Plot forskellen med forbedringer
ggplot(gns.sammenligning, aes(x = fct_reorder(Land, difference), y = difference, fill = difference >= 0)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_text(aes(label = ifelse(difference >= 0, paste0("+", round(difference, 2)), paste0("-", abs(round(difference, 2)))),
                color = difference >= 0), 
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = c("firebrick2", "forestgreen"), 
                    labels = c("Negativ effekt", "Positiv effekt")) +  # Fjern uønskede labels
  scale_color_manual(values = c("black", "black"), guide = "none") +  # Fjern color fra legend
  labs(
    title = "Coronakrisen havde størst effekt på Spaniens kvartalsvise realvækst",
    subtitle = "Forskel i gennemsnitlig realvækst med og uden corona (2020Q1–2022Q2)",
    x = NULL,
    y = "Forskel i gennemsnitlig realvækst (%)",
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Sørg for korrekt legend







# Sammenlign med forrige dataframe MED coronakrise -----------------------------

# Kombinér data til sammenligning
gns.sammenligning <- cbind(gennemsnit.total.df, gennemsnit.minus.corona.df[,-1])
colnames(gns.sammenligning) <- c("Land", "Gns. vækst med corona", "Gns. vækst uden corona")

# Omform data til langt format for lettere visualisering
data_long <- reshape2::melt(gns.sammenligning, id.vars = "Land")

# Visualisering af gennemsnit med og uden corona ------------------------------
ggplot(data_long, aes(x = fct_reorder(Land, value), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) + 
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 4, fontface = "bold") +
  labs(
    title = "Coronaperioden påvirker landenes gennemsnitlige vækst i forbruget",
    subtitle = "Sammenligning af gennemsnitlig realvækst med og uden corona (2020Q1–2022Q2)",
    x = NULL,
    y = "Gennemsnitlig vækst (%)",
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  scale_fill_brewer(palette = "Paired", 
                    labels = c("Med corona (2000Q1 - 2024Q3)", "Uden corona (- 2020Q1 - 2022Q2)")) +  # Gør labels mere læsevenlige
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50")
  )




#### Opgave 5.4 – Effekt af Corona på forbruget ####


# Effekt af corona på forbruget -----------------------------------------------

# Beregn gennemsnit for perioden
periode <- realvækst.total[c(81:99), ]  # perioden 2020Q1–2024Q3
gns.periode <- colMeans(periode[-1], na.rm = TRUE)
gns.periode.df <- data.frame(Land = names(gns.periode), Gns = gns.periode)

# Plot gennemsnit for perioden
ggplot(gns.periode.df, aes(x = fct_reorder(Land, Gns), y = Gns, fill = Land)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Sorte kanter og justeret bredde
  geom_text(aes(label = round(Gns, 2)), vjust = -0.5, size = 4, fontface = "bold") +  # Tekst med fed skrift
  labs(
    title = "Markante forskelle i økonomisk genopretning blandt europæiske lande efter coronakrisen",
    subtitle = "Gennemsnitlig kvartalsvis realvækst (%) fra 2020Q1 til 2024Q3",
    y = "Gennemsnitlig realvækst (%)",
    x = NULL,
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  scale_fill_brewer(palette = "Paired", guide = "none") +  # Ingen legend
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Gør overskriften større og centreret
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),  # Undertekst fremhævet
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Roter x-akseteksten
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  # Diskrete linjer
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50")  # Venstrejustér kilden
  )


# Konverter data til langt format for visualisering
periode.long <- periode %>%
  pivot_longer(-time, names_to = "Country", values_to = "Value")

# Find den absolut laveste værdi
overall_min_value <- min(periode.long$Value, na.rm = TRUE)
overall_min_time <- periode.long$time[which.min(periode.long$Value)]
overall_min_country <- periode.long$Country[which.min(periode.long$Value)]

# Tilføj fulde landenavne til datasættet
periode.long <- periode.long %>%
  mutate(Country = recode(
    Country,
    "DK" = "Danmark",
    "BE" = "Belgien",
    "NL" = "Nederlandene",
    "SE" = "Sverige",
    "AT" = "Østrig",
    "DE" = "Tyskland",
    "FR" = "Frankrig",
    "IT" = "Italien",
    "ES" = "Spanien"
  ))

# Plot udviklingen med fulde landenavne og årstal som x-akse
ggplot(periode.long, aes(x = time, y = Value, color = Country)) +
  geom_line(size = 1.2) +  # Gør linjerne tykkere
  geom_point(size = 3) +  # Fremhæv punkter
  annotate(
    "text", x = overall_min_time, y = overall_min_value,
    label = paste(overall_min_country, round(overall_min_value, 2)),
    vjust = 1.5, color = "red", fontface = "bold", size = 4
  ) +  # Fremhæv den absolut laveste værdi
  labs(
    title = "Spanien: Største fald og stigning i husholdningernes forbrugsudgifter",
    subtitle = "Kvartalsvise realvækst i forbrugsudgifter (%) i perioden Q1 2020 – Q3 2024",
    x = "Tid",
    y = "Kvartalsvis realvækst i forbrugsudgifter (%)",
    caption = "Kilde: Eurostat, GDP and main components (output, expenditure and income)"
  ) +
  scale_color_brewer(palette = "Paired") +  # Brug bedre farvepalet
  scale_x_date(
    date_labels = "%Y", 
    date_breaks = "1 year"
  ) +  # Brug kun årstal som labels
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
    plot.subtitle = element_text(size = 14, face = "italic", hjust = 0.5),  
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  
    axis.text.y = element_text(size = 12),
    legend.title = element_blank(),  
    legend.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0, color = "grey50") 
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))  
