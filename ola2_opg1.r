## Pakker
library(tidyverse)
library(dkstat)
library(RColorBrewer)

#### Opgave 1.1 ####
## Søger efter tabel der indeholder befolkningstal
befolkning_search = dst_search(string = "befolkning")

## Henter metadata og danner filter til query
POSTNR1_meta = dst_meta(table = "POSTNR1")
POSTNR1_filter = list(PNR20 = "*",
                      tid = "2024")

## Indhenter filteret data
POSTNR1 <- dst_get_data(table = "POSTNR1", query = POSTNR1_filter)

## Filtrer "Hele landet" fra
POSTNR1 <- POSTNR1[-1,]

## Frasorterer "(kommune-delen)"
POSTNR1$PNR20 <- gsub("\\s*\\(.*$", "", POSTNR1$PNR20)

## Tilføjer "postnr" til ny kolonne
POSTNR1$postnr <- substr(POSTNR1$PNR20, 1, 4) ## 1 og 4 specificerer første og fjerde plads
POSTNR1$postnr <- as.numeric(POSTNR1$postnr)

## Frasorterer de første 4 cifre.
POSTNR1$PNR20 <- gsub("\\d{4}", "", POSTNR1$PNR20)
POSTNR1$PNR20 <- trimws(POSTNR1$PNR20)

## Dataen aggregeres
test_df <- POSTNR1 %>%
  mutate(by = case_when(
    postnr >= 1000 & postnr <= 1799 ~ "København",
    postnr >= 1800 & postnr <= 2000 ~ "Frederiksberg",
    postnr >= 2100 & postnr <= 2450 ~ "København",
    postnr >= 5000 & postnr <= 5270 ~ "Odense",
    postnr >= 6700 & postnr <= 6715 ~ "Esbjerg",
    postnr >= 8000 & postnr <= 8210 ~ "Aarhus",
    postnr >= 8900 & postnr <= 8960 ~ "Randers",
    postnr >= 9000 & postnr <= 9220 ~ "Aalborg",
    TRUE ~ NA_character_  # Alle andre byer får NA
  ))

# Aggregérer byerne
aggregated_rows <- test_df %>%
  filter(!is.na(by)) %>%  # Vælg kun rækker med en by
  group_by(by) %>%        # Gruppér efter by
  summarize(
    PNR20 = first(by),                # Navn på byen
    TID = first(TID),                 # Beholder TID (eller summer, hvis relevant)
    value = sum(value, na.rm = TRUE)  # Summerer værdier
  )

cities <- c("København","Odense", "Esbjerg", "Frederiksberg", "Randers", "Aalborg", "Aarhus")

# Initialiser test_col med de originale værdier fra PNR20
POSTNR1$test_col <- POSTNR1$PNR20

# Iterér over hver by og opdater test_col
for (city in cities) {
  # Find rækker i POSTNR1, der indeholder byen
  match_rows <- grepl(city, POSTNR1$PNR20, ignore.case = TRUE)
  
  # Find værdien fra aggregated.rows for den præcise by
  value_to_insert <- aggregated_rows$value[aggregated_rows$PNR20 == city]
  
  # Sikr, at der kun findes én matchende værdi fra aggregated.rows
  if (length(value_to_insert) == 1) {
    POSTNR1$value[match_rows] <- value_to_insert
  } else if (length(value_to_insert) > 1) {
    stop(paste("Flere værdier fundet i aggregated.rows for byen:", city))
  } else {
    # Hvis der ikke er match i aggregated.rows, behold værdien
    warning(paste("Ingen værdi fundet i aggregated.rows for byen:", city))
  }
}

## Fjerner testkolonnen igen og opdaterer colnames
POSTNR1 <- POSTNR1[,-5]
colnames(POSTNR1) <- c("by","dato","indbyggertal","postnr")

#### opgave 1.2 - Kategorivariabel ####
## Definerer kategorierne direkte i case_when funktionen
POSTNR1 <- POSTNR1 %>%
  mutate(bystørrelse = case_when(
    indbyggertal < 250 ~ "landet",
    indbyggertal < 1000 ~ "landsby",
    indbyggertal < 2500 ~ "lille by",
    indbyggertal < 10000 ~ "almindelig by",
    indbyggertal < 50000 ~ "større by",
    TRUE ~ "storby"
  ))

#### Opgave 1.3 Merge de to dataframes ####
## Indlæs "boligsiden_clean.RDS" fra OLA 1
setwd("C:/Users/Olive/OneDrive/Dokumenter/Rfiles/OLA/OLA1/") ## Sæt din egen sti
boligsiden_clean <- readRDS("Documents/Dataanlyse/1_semester/R/OLA2_/boligsiden_clean.RDS")

boligsiden_clean <- readRDS("boligsiden_clean.RDS")


## Merger relevante kolonner 
boligsiden_clean <- left_join(boligsiden_clean, POSTNR1[,3:5], by = "postnr")

#### Opgave 1.4 Plot ####
avg_kvmpris <- data.frame(tapply(boligsiden_clean$kvmpris, boligsiden_clean$bystørrelse, mean, na.rm = TRUE))
colnames(avg_kvmpris) <- "Gns.kvmpris"
avg_kvmpris <- rownames_to_column(avg_kvmpris, var = "bystørrelse")

## Omordn dataframen med korrekt faktor-reordering
avg_kvmpris <- avg_kvmpris %>% 
  mutate(bystørrelse = fct_reorder(bystørrelse, Gns.kvmpris))

#### GGplot ####
ggplot(data = avg_kvmpris) +
  geom_bar(aes(x = fct_reorder(bystørrelse, Gns.kvmpris, .desc=F ), 
               y = Gns.kvmpris, fill = bystørrelse), stat = "identity")+
  ## Labs
  labs(x = "",
       title = "Kvadratmeterprisenerne i større byer overhaler storbyerne",
       caption = "Datakilde: Danmarks Statistik og Boligsiden")+
  
  ## Textværdier
  geom_text(aes(x = bystørrelse, y = Gns.kvmpris,
            label = round(Gns.kvmpris)),
            vjust = -0.3)+
  ## Tema
  theme_minimal()+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "Purples")
