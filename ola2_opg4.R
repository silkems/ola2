#### Opgave 4 – Forbrug og forbrugertillidsindikatorer fra DST og DI, samt loops i lister ####

#### Pakker ####
library(tidyverse)
library(dkstat)
library(zoo)


#### Opgave 4.1 – Illustration af forbrugertillid ####

#### Realvækst NKHC021 ####
## Hent forbrugsdata via DST API
NKHC021.meta <- dst_meta(table="NKHC021", lang="da")

NKHC021.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKHC021 <- dst_get_data(table = "NKHC021", query = NKHC021.filter)

## Fjern unødvendige kolonner, men behold `value`
NKHC021 <- NKHC021[,-c(1:3)]

## Opret en kolonne til realvækst
NKHC021$Realvækst <- NA  # Starter med at fylde med NA-værdier

## Beregn den kvartalvise årlige realvæskt
for (i in (5:nrow(NKHC021))) {
  NKHC021$Realvækst[i] <- (NKHC021$value[i] - NKHC021$value[i-4]) / NKHC021$value[i-4] * 100
}

## Skær til fra 2000Q1
NKHC021 <- NKHC021[-1:-40,]
rownames(NKHC021) <- NULL

# Formater tidsformat 
NKHC021$TID <- as.yearqtr(NKHC021$TID, format = "%YQ%q")


## Angiver en kolonne med retning af realvæksten
NKHC021$Retning <- as.factor(ifelse(NKHC021$Realvækst >= 0, "Op", "Ned"))



#### Forbrugerforvetninger FORV1 ####
FORV1.meta <- dst_meta(table = "FORV1", lang = "da")

## Liste med relevante filter-variabler
FORV1.filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)

FORV1 <- dst_get_data(table = "FORV1", query = FORV1.filter, lang = "da")

## Bredt format
FORV1 <- pivot_wider(FORV1, names_from = INDIKATOR, values_from = value)

## Gruppering og opsummering med udregning af mean
FORV1$TID <- paste(year(FORV1$TID), " Q", quarter(FORV1$TID), sep = "")

FORV1 <- FORV1 %>%
  group_by(TID) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

## Skær til fra 2000Q1
FORV1 <- FORV1[-1:-101,]

## Tilpas længde i FORV1 til NKHC021 
FORV1 <- FORV1[1:nrow(NKHC021),]

# Formater tidsformat
FORV1$TID <- as.yearqtr(FORV1$TID, format = "%Y Q%q")




#### Graf 1 ####

## Skaleringsfaktor til anden y-akse
scale_factor <- 2.0


## Plot data med ggplot
ggplot() +
  # Søjler for realvækst
  geom_bar(aes(x = NKHC021$TID, y = NKHC021$Realvækst * scale_factor), 
           stat = "identity", fill = "cornflowerblue") +
  # Linje for forbrugertillidsindikatoren
  geom_line(aes(x = FORV1$TID, y = FORV1$Forbrugertillidsindikatoren), 
            color = "orange", size = 1) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Årlig kvartalsvis realvækst i privatforbrug (%)"),
    name = "Forbrugertillidsindikatoren i nettotal"
  ) +
  scale_x_continuous(
    breaks = seq(as.numeric(as.yearqtr("2000 Q1")), as.numeric(as.yearqtr("2024 Q4")), by = 2), # Hvert andet år
    labels = function(x) format(as.Date(as.yearqtr(x, format = "%Y Q%q")), "%Y"),
    name = ""
  ) +
  labs(
    title = "Forbrugertillidsindikatoren falder i højere grad ift. realvæksten",
    subtitle = "Forbrugertillidsindikatoren er et simpelt gennemsnit af 5 spørgsmål",
    caption = paste(
      "Kilde: Danmarks Statistik<br>",
      "<span style='color:orange;'>Linjen repræsenterer Forbrugertillidsindikatoren (venstre akse)</span><br>",
      "<span style='color:cornflowerblue;'>Søjlerne repræsenterer realvæksten i privatforbrug i % (højre akse)</span>"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), # Roter årstals-labels for klarhed
    plot.title = element_text(size = 16, face = "bold"), # Gør titlen større og fed
    plot.subtitle = element_text(size = 14), # Gør underteksten større
    plot.caption = element_markdown(size = 12, hjust = 0), # Gør billedteksten større og tillad HTML/Markdown
    legend.position = "none" # Fjern standardlegenden
  )



#### Opgave 4.2 – Gennemsnit af underspørgsmål ####

## Gennemsnit for spørgsmålet "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
mean(FORV1$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)
# gennemsnit = -10.39899




#### Opgave 4.3 – De 11 grupper af forbrug ####

#2020 til 2023:

## Hent forbrug i løbende priser
husforbrug_meta <- dst_meta(table="NKHC021", lang="da")

my_query_husforbrug <- list(
  FORMAAAL = "*",
  PRISENHED = "Løbende priser",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

husforbrug_data <- dst_get_data(table = "NKHC021", query = my_query_husforbrug)
View(husforbrug_data)

## Konverter fra langt til bredt format
husforbrug_data <- husforbrug_data %>%
  pivot_wider(names_from = FORMAAAL, values_from = value)

husforbrug_data <- t(husforbrug_data)

## Klip 2023 ud
husforbrug2023_data <- as.data.frame(husforbrug_data[-1:-4, 133:136])

## Ændr kolonnenavne til kvartaler
colnames(husforbrug2023_data) <- c("Q1", "Q2", "Q3", "Q4")

## Konverter Q1, Q2, Q3 ,Q4 til numeric
husforbrug2023_data <- husforbrug2023_data %>% 
  mutate_all(as.numeric)

## Sum alle kvartalerne
husforbrug2023_data <- husforbrug2023_data %>% 
  mutate(sum_2023 = Q1 + Q2 + Q3 + Q4)

## 2020 - 2023
husforbrug2020_data <- as.data.frame(husforbrug_data[-1:-4,121:124])

## Ændr kolonnenavne
colnames(husforbrug2020_data) <- c("Q1", "Q2", "Q3", "Q4")

## Konverter Q1, Q2, Q3 ,Q4 til numeric
husforbrug2020_data <- husforbrug2020_data %>% 
  mutate_all(as.numeric)

## Sum alle kvartalerne
husforbrug2020_data <- husforbrug2020_data %>% 
  mutate(sum_2020 = Q1 + Q2 + Q3 + Q4)

## Sammensæt sum fra 2020 og 2023
husforbrug_sum <- as.data.frame(husforbrug2020_data[,-1:-4])

husforbrug_sum <- cbind(husforbrug_sum, husforbrug2023_data$sum_2023)
colnames(husforbrug_sum) = c("sum2020", "sum2023")
rownames(husforbrug_sum) = rownames(husforbrug2020_data)

## Procentvis ændring i forbruget
husforbrug_sum <- husforbrug_sum %>% 
  mutate(procentvis_ændring = round((sum2023 - sum2020) / sum2020 *100, 2))




#### Opgave 4.4 – 22 simple lineære regressioner ####


## Hent FORV1 fra forv1_nkhc021 script

#### Opgave 4.4 ####
##### Data retrieval og transformation #####
## Opret df med DI FTI - Vælg de relevante kolonner
DI <- FTI[,c(1,2,4,6,8,12)]

## Beregn DI Indikator, som simpelt gennemsnit af 4 spørgsmål
DI$DIFTI <- rowMeans(FTI[, c(4, 6, 8, 12)], na.rm = TRUE)


## Hent forbrug i kædede værdier fordelt på 15 grupper
forbrugsgrupper_meta <- dst_meta(table="NKHC021")

forbrugsgrupper_query <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrugsgrupper_data <- dst_get_data(table = "NKHC021", query = forbrugsgrupper_query)

## Konverter fra langt til bredt format
forbrugsgrupper_data <- forbrugsgrupper_data %>%
  pivot_wider(names_from = FORMAAAL, values_from = value)

## Klip fra år 2000
forbrugsgrupper_data <- forbrugsgrupper_data[-1:-36, -1:-2]

## Konverter til dataframe - bliver gemt som en tibble?
forbrugsgrupper_data <- as.data.frame(forbrugsgrupper_data)


## Opret dataframe til der hvor væksttallene skal ind
forbrugsgrupper_vækst <- data.frame(TID = forbrugsgrupper_data$TID) # Start med TID fra række 5

## Loopet beregner den kvartalsvise årlige realvækst i hver forbrugsgruppe
for (col in 2:ncol(forbrugsgrupper_data)) {
  # Beregn væksten og tilføj NA for de første 4 rækker
  vækst <- rep(NA, 4) 
  vækst <- c(vækst, (forbrugsgrupper_data[[col]][5:nrow(forbrugsgrupper_data)] - 
                       forbrugsgrupper_data[[col]][1:(nrow(forbrugsgrupper_data) - 4)]) / 
               forbrugsgrupper_data[[col]][1:(nrow(forbrugsgrupper_data) - 4)] * 100)
  
  forbrugsgrupper_vækst[[colnames(forbrugsgrupper_data)[col]]] <- vækst
}

## Fjern de 4 NA rækker - De var kun med for at få væksttallet i første periode
forbrugsgrupper_vækst <- forbrugsgrupper_vækst[-1:-4,]


## Navngiv resultaterne - DST
forbrugsgrupper <- colnames(forbrugsgrupper_data)[3:17]
names(resultater_FTI) <- forbrugsgrupper


##### Regressions loop vha lapply() - DST #####
resultater_FTI <- lapply(forbrugsgrupper, function(gruppe) {
  lm(forbrugsgrupper_vækst[[gruppe]] ~ FTI$Forbrugertillidsindikatoren)
})


##### Regressions loop vha lapply() - DI #####
resultater_DI <- lapply(forbrugsgrupper, function(gruppe) {
  lm(forbrugsgrupper_vækst[[gruppe]] ~ DI$DIFTI)
})

## Navngiv resultaterne - DI
names(resultater_DI) <- forbrugsgrupper
