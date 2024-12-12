#### Opgave 2 – Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift ####

#### Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator ####

#### Pakker ####
library(tidyverse)
library(dkstat)
library(zoo)

#### Realvækst NKHC021 ####
## Hent forbrugsdata via DST API
NKHC021.meta <- dst_meta(table = "NKHC021", lang = "da")

NKHC021.filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKHC021 <- dst_get_data(table = "NKHC021", query = NKHC021.filter)
NKHC021 <- NKHC021[,-c(1:3)]

## Udregner den kvartalvise årlige realvækst
NKHC021$Realvækst <- (NKHC021$value / dplyr::lag(NKHC021$value, 4) - 1) * 100

## Skær til fra 2000Q1
NKHC021 <- NKHC021[-1:-40, -2] # Tager for den udvalgte periode
rownames(NKHC021) <- NULL

# Formater tidsformat 
NKHC021$TID <- paste0(year(NKHC021$TID), "Q", quarter(NKHC021$TID))

########### Forbrugerforventninger FORV1 #############
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

# Formater tidsformat
FORV1$TID <- gsub(" ", "", FORV1$TID) 

#Q4 tal for FTI er -9.10



#### Dataframe med FTI og Realvækst i lige lange perioder ####
FTI <- data.frame(NKHC021)
FTI <- cbind(FTI, FORV1[1:nrow(FTI),-1])

#### Dataframe med DI.FTI og realvæksten i lige lange perioder ####

# Udvælg relevante indikatorer for DI fra FORV1
Forbrugertillid.udvalgte <- FORV1 %>%
  select(TID, 3, 5, 7, 11)

# Beregn DI's samlede indikator pr. kvartal 
DI.FTI.samlet <- Forbrugertillid.udvalgte %>%
  mutate(DI.FTI = rowMeans(across(-TID), na.rm = TRUE)) %>% # Beregn gennemsnit for hver række
  select(TID, DI.FTI)    # Behold kun TID og DI.FTI

#Q4 tal for DI.FTI er det -10.27

# Kombiner kun de nødvendige kolonner
DI.FTI <- left_join(NKHC021, DI.FTI.samlet, by = "TID")



#### Korrelation og forklaringsgrad FTI og realvæksten 2000Q1-2024Q3####

#### Beregn korrelation og forklaringsgrad for FTI ####
FTI.cor <- cor(FTI$Realvækst, FTI$`Forbrugertillidsindikatoren`)
print(FTI.cor)
# FTI korrelation = 0.5075718 = 0.51

FTI.reg <- lm(FTI$Forbrugertillidsindikatoren ~ FTI$Realvækst)
FTI.R2 <- summary(FTI.reg)$r.squared
print(FTI.R2)
#FTI forklaringsgrad = 0.25762913 = 0.26



#### Korrelation og forklaringsgrad for DI og realvækst 2000Q1-2024Q3 ####

# Korrelation mellem DI og realvækst
DI.FTI.cor <- cor(DI.FTI$Realvækst,DI.FTI$DI.FTI)
print(DI.FTI.cor)
#Korrelation = 0.56400198 = 0.56

# Lineær regression for DI's forklaringsgrad
DI.FTI.reg <- lm(DI.FTI$DI.FTI~DI.FTI$Realvækst)
summary(DI.FTI.reg)$r.squared 
#Forklaringsgrad = 0.31809824 = 0.32



#### BAUM periode for både FTI og DI - korrelation og foklaringsgrad - periode: 2000Q1-2016Q2 i 2020 priser ####

# FTI for BAUM-periode (2020-priser) - 2000Q1-2016Q2

# Filtrér datasæt til perioden 2000Q1 - 2016Q2
FTI_BAUM <- FTI[1:66, ]

# Beregn korrelation for perioden
FTI_BAUM_COR <- cor(FTI_BAUM$Realvækst, FTI_BAUM$`Forbrugertillidsindikatoren`)
print(FTI_BAUM_COR)
# Korrelation = 0.59482401 = 0.59

# Udfør lineær regression for perioden
FTI_BAUM_reg <- lm(FTI_BAUM$`Forbrugertillidsindikatoren` ~ FTI_BAUM$Realvækst)
FTI_BAUM_R2 <- summary(FTI_BAUM_reg)$r.squared
print(FTI_BAUM_R2)
# Forklaringsgrad = 0.3538156 = 0.35



#### DI.FTI for BAUM-periode (2020-priser) - 2000Q1-2016Q2 ####

# Filtrér datasæt til perioden 2000Q1 - 2016Q2
DI.FTI_BAUM <- DI.FTI [1:66, ]

# Beregn korrelation for perioden
DI.FTI_BAUM_cor <- cor(DI.FTI_BAUM$Realvækst, DI.FTI_BAUM$DI.FTI)
print(DI.FTI_BAUM_cor)
# korrelation = 0.68797717 = 0.69

# Udfør lineær regression for perioden
DI_BAUM_reg <- lm(DI.FTI_BAUM$DI.FTI ~ DI.FTI_BAUM$Realvækst)
DI_BAUM_R2 <- summary(DI_BAUM_reg)$r.squared
print(DI_BAUM_R2)
# forklaringsgrad = 0.47331259 = 0.47




#graf 1 - DI og FTI og realvækst 
ggplot() +
  # Barplot for årlig realvækst i kvartalsforbrug
  geom_bar(data = DI.FTI, 
           aes(x = factor(TID, levels = TID), y = Realvækst * 4, 
               fill = "Årlig realvækst pr. kvartal i privatforbrug"), 
           stat = "identity", color = "black", alpha = 0.7) +
  
  # Linjeplot for DI's forbrugertillidsindikator
  geom_line(data = DI.FTI, 
            aes(x = factor(TID, levels = TID), y = DI.FTI, 
                color = "DI's forbrugertillidsindikator"), 
            size = 1, group = 1) +
  
  # Linjeplot for FTI's forbrugertillidsindikator
  geom_line(data = FTI, 
            aes(x = factor(TID, levels = TID), y = Forbrugertillidsindikatoren, 
                color = "FTI's Forbrugertillidsindikator"), 
            size = 1, group = 1) +
  
  # Y-aksejusteringer for både DI og realvæksten
  scale_y_continuous(
    name = "DI's Forbrugertillidsindikator (Nettotal)", 
    sec.axis = sec_axis(~ . / 4, name = "Årlig Realvækst (Pct.)")) +
  
  # X-aksejustering til kun at vise hvert andet år
  scale_x_discrete(
    breaks = DI.FTI$TID[seq(1, nrow(DI.FTI), by = 8)], 
    labels = function(x) ifelse(grepl("Q1$", x), substr(x, 1, 4), "")) +
  
  # Tilføj labels og farver til fill og color
  scale_fill_manual(name = "", 
                    values = c("Årlig realvækst pr. kvartal i privatforbrug" = "lightgrey")) +
  scale_color_manual(name = "", 
                     values = c("DI's forbrugertillidsindikator" = "darkblue", 
                                "FTI's Forbrugertillidsindikator" = "red")) +
  # Labels og titel
  labs(
    x = "Tid", 
    title = "DI's forbrugertillidsindikator følger den årlige realvækst", 
    caption = "Kilde: Danmarks Statistik og DI analyse") +
  
  # Tema og layoutjusteringer
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "black", size = 12, face = "bold"),  
    axis.title.y.right = element_text(color = "blue", size = 12, face = "bold"),  
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),  
    legend.position = "bottom",  
    plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0.5),  
    panel.grid.major = element_line(color = "grey90"),  
    panel.grid.minor = element_blank(),  
    axis.text = element_text(size = 12) 
  )



## Graf 2 - 1 spg op i mod DI og FTI
ggplot() +
  # Fremhæv spørgsmålet om Danmarks økonomiske situation
  geom_line(data = FORV1, 
            aes(x = factor(TID, levels = TID), 
                y = `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`, 
                color = "Danmarks økonomiske situation sammenlignet med sidste år"), 
            size = 1.8, group = 1) +  # Tykkere linje
  
  # Linje for DI's forbrugertillidsindikator
  geom_line(data = DI.FTI.samlet, 
            aes(x = factor(TID, levels = TID), 
                y = DI.FTI, 
                color = "DI forbrugertillidsindikator"), 
            size = 1.2, linetype = "solid", group = 1) +
  
  # Linje for FTI's forbrugertillidsindikator
  geom_line(data = FTI, 
            aes(x = factor(TID, levels = TID), 
                y = `Forbrugertillidsindikatoren`, 
                color = "FTI forbrugertillidsindikator"), 
            size = 1.2, group = 1) +
  
  # Labels og titel
  labs(
    title = "Danskernes syn på landets økonomiske situation driver store dele af ændringer i forbrugertillidsindikatorerne",
    x = "Tid",
    y = "Nettotal",
    caption = "Kilde: Danmarks Statistik og DI analyse") +
  
  # Tilpas farver for linjerne
  scale_color_manual(values = c(
    "Danmarks økonomiske situation sammenlignet med sidste år" = "blue", 
    "DI forbrugertillidsindikator" = "red",
    "FTI forbrugertillidsindikator" = "green")) +
  
  # Tilpasning af X-aksen for at vise hvert andet år
  scale_x_discrete(
    breaks = FORV1$TID[seq(1, nrow(FORV1), by = 8)],  # Hvert ottende kvartal
    labels = function(x) ifelse(grepl("Q1", x), substr(x, 1, 4), "")) + 
  
  # Tema og layoutjusteringer
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),  # Venstre y-aksetitel
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Juster margin under x-aksens titel
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, size = 12),  # Roter og juster x-aksetekst
    legend.position = "bottom",  # Flyt legenden til bunden
    legend.title = element_blank(),  # Fjern legendetitel
    legend.text = element_text(size = 12),  # Større skrifttype til legenden
    plot.title = element_text(size = 16, face = "bold", color = "black", hjust = 0.5, margin = margin(b = 15)),  # Fremhæv titel
    panel.grid.major = element_line(color = "grey90"),  # Lysere gridlinjer
    panel.grid.minor = element_blank(),  # Fjern mindre gridlinjer
    axis.text = element_text(size = 12),  # Større skrifttype til aksetekst
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # Juster margener for bedre layout
  )



#### Opgave 2.2 – Forudsigelser af forbruget ####

# Nye regressionsmodeller med Realvækst som afhængig variabel
FTI.reg.rev <- lm(Realvækst ~ Forbrugertillidsindikatoren, data = FTI)
DI.reg.rev <- lm(Realvækst ~ DI.FTI, data = DI.FTI)

#benytter summary
(FTI.reg.rev)$coef
(DI.reg.rev)$coef


# Kendte værdier for FTI og DI-FTI for 2024 Q4
FTI_2024_Q4 <- data.frame(Forbrugertillidsindikatoren = -9.10000000)
DI_FTI_2024_Q4 <- data.frame(DI.FTI = -10.27500000)

#Benytter med formel for lineære regression (b0 + b1 * X)

# FTI = 1.41819990 + 0.17492999 x (−9.1) = −0.17366309 = - 0.17

#DI.FTI = 2.25569070+0.18077618⋅(−10.27) = 0.39910902 = 0.40


#Dobbelttjekker med predict
# Forudsig realvækst for 2024 Q3
FTI_realvækst_2024_Q4 <- predict(FTI.reg.rev, newdata = FTI_2024_Q4)
DI_realvækst_2024_Q4 <- predict(DI.reg.rev, newdata = DI_FTI_2024_Q4)

# Vis resultaterne
print(paste("Forudsigelse af realvækst baseret på FTI:", FTI_realvækst_2024_Q4))
print(paste("Forudsigelse af realvækst baseret på DI-FTI:", DI_realvækst_2024_Q4))

#resultat
#Forudsigelse af realvækst baseret på FTI: -0.173663007311005 = -0.17
#Forudsigelse af realvækst baseret på DI-FTI: 0.39821547629288 = 0.40