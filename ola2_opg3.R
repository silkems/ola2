#### Opgave 3 – Logistik regression, husholdningernes forbrugsudgift og forbrugertillid ####

#### Dummy-variabel for realvækst i privatforbrug ####

#### Pakker ####
library(dkstat)
library(tidyverse)
library(lattice)
library(caret)

#### Opgave 3.1 – Feature Engineering, dummy variable ####

# Filtrering til 2000Q1 til 2024Q3
FORV1 <- FORV1[FORV1$TID >= "2000Q1" & FORV1$TID <= "2024Q3",]


#### Beregning af perioder med stigende/faldende vækst ####

# Tilføj dummy-kolonne: 1 for stigende vækst, 0 for faldende vækst

NKHC021$dummy <- ifelse(NKHC021$Realvækst >= 0, 1, 0)

# Beregn antal og procent for stigende og faldende vækst
stigende <- sum(NKHC021$dummy)
faldende <- nrow(NKHC021) - stigende
procent_stigende <- (stigende / nrow(NKHC021)) * 100
procent_faldende <- 100 - procent_stigende

# Udskriv resultater
print(paste("- Stiger", stigende, "gange, svarende til ca.", round(procent_stigende, 2), "% af perioden."))
print(paste("- Falder", faldende, "gange, svarende til ca.", round(procent_faldende, 2), "% af perioden."))


#Den kvartalsvise årlige vækst i husholdningernes forbrugsudgifter:
# Stiger 77 gange, svarende til ca. 77.78 % af perioden.
# Falder 22 gange, svarende til ca. 22.22 % af perioden



#### Opgave 3.2 – Logistisk regression og forudsigelser ####

# Kombiner datasættene baseret på TID
# Sørg for, at begge datasæt har en fælles kolonne, fx "TID"
combined_data <- merge(NKHC021, DI.FTI, by = "TID")

## Multipel logistisk regression, Retning som y (forudsig), FTI som x(uafhængig variabel)

retningglm <- glm(formula = NKHC021$dummy ~ 
                    FORV1$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                    FORV1$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                    FORV1$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                    FORV1$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`,  
                  family = "binomial")

summary(retningglm)

retningglm$coefficients

# 3 ud af 4 er insignifkante - disse fjernes og vi har derfor kun 1. vi skal have alle med og tager derfor bare DI for Q2

retningglm2 <- glm(formula = NKHC021$dummy ~ 
                     FORV1$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
                   family = "binomial")

summary(retningglm2)


#tages af DI i stedet

retningglm3 <- glm(formula = NKHC021$dummy ~
                     DI.FTI$DI.FTI,
                   family = "binomial")

summary(retningglm3)


#indikatoren som helhed er siknifikant og vi bruger derfor denne.
# i dette tilfælde bruger vi DI selvom p værdien er lavere, da alle spg skal inkluderes. 

#udregner med formel 
#intercept = 2.08508
#coef = 0.14242
#x-værdi = -10.27


# Opret modellen
retningglm3 <- glm(formula = dummy ~ DI.FTI, data = combined_data, family = "binomial")

# Nye data til forudsigelse
ny_data <- data.frame(DI.FTI = -10.27)

# Forudsig sandsynligheden for Q4 2024 med predict 
ny_data$Forudsigelse <- predict(retningglm3, newdata = ny_data, type = "response")
print(ny_data$Forudsigelse)
# 2024 Q4 = 0.6507714

#Predict funktion på hele perioden (NKHC021)
Forudsigelse <- predict.glm(retningglm3, newdata = NULL, type = c("response"))
NKHC021$Forudsigelse <- Forudsigelse



#### Opgave 3.3 – Simpel validering af model ####

# Brug forudsigelserne til at tilføje en dummy-kolonne baseret på threshold 0.5
NKHC021$Dummy.forudsigelse <- ifelse(NKHC021$Forudsigelse >= 0.5, 1, 0)
table(NKHC021$Dummy.forudsigelse)

#0  1 
#9 90 

# Brug forudsigelserne til at tilføje en dummy-kolonne baseret på threshold 0.7
NKHC021$Dummy.forudsigelse2 <- ifelse(NKHC021$Forudsigelse >= 0.7, 1, 0)
table(NKHC021$Dummy.forudsigelse)

# 0  1 
#22 77 


#Høj sandsynlighed (tæt på 1): Der er en høj sandsynlighed for, at forbruget stiger.
#Lav sandsynlighed (tæt på 0): Der er en lav sandsynlighed for, at forbruget stiger (dvs. forbruget falder).
#vi vurderer at en sandsynlighed på over 0.7 betyder stigning og under betyder fald i forbruget

#reele tal
#0  1 
#22 77

#Forudsigelse
# 0  1 
#22 77


install.packages("ConfusionTableR", repos = "https://cloud.r-project.org")
library(ConfusionTableR)

Konfusionsmatrice <- rbind(NKHC021$dummy,NKHC021$Dummy.forudsigelse2)

Faktiske <- as.factor(NKHC021$dummy)
Forudsigelse <- as.factor(NKHC021$Dummy.forudsigelse2)

# confusion matrix
con_matrx <- confusionMatrix(Faktiske, Forudsigelse)

matrix.table <- as.data.frame(con_matrx$table)

ggplot(data=matrix.table, aes(x = Reference , y= Prediction, fill = Freq))+
  geom_tile()+
  geom_text(aes(label = Freq))

#Reference
#Prediction  0  1
#          0 13  9
#          1  9 68


# Opret data til grafen
forudsigelse_data <- data.frame(
  Kategori = c("Fald i forbruget (0)", "Stigning i forbruget (1)"),
  Antal = c(22, 77) # Antal fra forudsigelsen
)

# Indlæs ggplot2-pakken, hvis den ikke allerede er indlæst
library(ggplot2)

# Lav søjlediagram
ggplot(forudsigelse_data, aes(x = Kategori, y = Antal, fill = Kategori)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", show.legend = FALSE) +
  labs(
    title = "Forudsigelser af ændringer i forbrug",
    subtitle = "Baseret på forudsiget sandsynlighed for ændring",
    x = "Forbrugskategori",
    y = "Antal observationer"
  ) +
  scale_fill_manual(values = c("Fald i forbruget (0)" = "#FF9999", "Stigning i forbruget (1)" = "#99CCFF")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.major.x = element_blank()
  ) +
  geom_text(aes(label = Antal), vjust = -0.5, size = 5)



# roc kurve 

#forudsagte forudsigelser på hele perioden
predicted <- predict.glm(retningglm3, newdata = NULL, type = c("response"))

# Beregn ROC
library(pROC)
roc_curve1 <- roc(NKHC021$dummy, predictor = predicted)

plot(roc_curve1, main = "ROC Kurve", col = "darkblue", lwd = 2)

# Beregn AUC
auc_value <- auc(roc_curve1) #0.7884
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "darkblue", cex = 1.5)
