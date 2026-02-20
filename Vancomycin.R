################################################################################
## Abschlussprojekt - Sergio Alvarez, Ilkay Aydin, Jakub Marczak, Nermine Msolly
################################################################################

## DATEN LADEN -----------------------------------------------------------------
load("/Users/jakubmarczak/Downloads/vancomycin.RData")
## -----------------------------------------------------------------------------

## PAKETE LADEN ----------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(dplyr)
library(corrplot)
library(hexbin)
## -----------------------------------------------------------------------------

## POPULATION VERSTEHEN --------------------------------------------------------
## Graphik: Alter, Größe & Gewicht mit Geschlechtsunterscheidung
df <- data.frame(
  gender = dat$Gender,
  birthdate = dat$Birthdate,
  height = dat$Height,
  weight = dat$Weight
)

df <- df %>%
  mutate(
    age = as.numeric(difftime(Sys.Date(), as.Date(birthdate), units = "days"))/365.25,
    gender = factor(tolower(gender), levels = c("male","female"))
  )

long <- df %>%
  select(gender, age, height, weight) %>%
  pivot_longer(cols = c(age, height, weight),
               names_to = "variable",
               values_to = "value")

long$variable <- factor(long$variable,
                             levels = c("age","height","weight"),
                             labels = c("Alter (Jahre)", "Größe (cm)", "Gewicht (kg)"))

ggplot(long, aes(x = value, color = gender)) +
  geom_density(linewidth = 1) +
  geom_rug() +
  facet_wrap(~ variable, scales = "free", nrow = 1) +
  labs(x = NULL, y = "Dichte", color = "Geschlecht") +
  scale_color_manual(values = c("male" = "#5ac9c7", "female" = "#ec5b5b")) +
  theme_minimal() + theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18)
  )

##------------------------------------------------------------------------------

################################################################################

## BEREICH NIERE ---------------------------------------------------------------

## Beziehung zwischen SCr und eGFR
opar <- par(mar = c(4.1, 4.1, 2.1, 2.1), mfrow = c(1, 1))
par(mar=c(0,0,0,0))
dat_kidney <- data.frame(
  SCrStart = dat$SCrStart,
  SCr24 = dat$SCr24,
  SCr48 = dat$SCr48,
  SCr72 = dat$SCr72,
  SCrEnd = dat$SCrEnd,
  eGFRStart = dat$eGFRStart,
  eGFR24 = dat$eGFR24,
  eGFR48 = dat$eGFR48,
  eGFR72 = dat$eGFR72,
  eGFREnd = dat$eGFREnd
)

corrplot(cor(dat_kidney, use = "complete.obs"),
         tl.col = "black", tl.cex = 1, tl.srt = 45, cl.ratio = 0.15, cl.offset = 0.1)
par(opar)
## -------------------------------

## Auswirkung vom durchschnittl. Vancomycin-Spiegel auf Nierenmarker
dat$C_mean <- rowMeans(dat[,c("C24","C48","C72")], na.rm=TRUE)
dat$deltaSCr <- dat$SCr72 - dat$SCrStart
dat$deltaeGFR <- dat$eGFR72 - dat$eGFRStart

ggplot(longC, aes(time, C)) +
  geom_boxplot()

longC <- dat %>%
  select(C24, C48, C72) %>%
  pivot_longer(cols = everything(),
               names_to = "time",
               values_to = "C")

p1 <- ggplot(dat, aes(x = C_mean, y = deltaSCr)) +
  geom_hex(alpha = 0.75, bins = 41) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"SCr")
  ) +
  theme_minimal()

p3 <- ggplot(dat, aes(x = C_mean, y = deltaeGFR)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"eGFR")
  ) +
  theme_minimal()

longC <- dat %>%
  select(C24, C48, C72) %>%
  pivot_longer(cols = everything(),
               names_to = "time",
               values_to = "C")

p2 <- ggplot(dat, aes(x = C_mean, y = deltaeGFR)) +
  geom_hex(alpha = 0.75, bins = 41) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"eGFR")
  ) +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)
grid.arrange(p3, p2, ncol = 2)
## -----------------------------------------------------------------
## -----------------------------------------------------------------------------

################################################################################

## BEREICH MORTALITÄTSANALYSE --------------------------------------------------

## Graphik: Schweregrad der Erkrankung 
#Mortalität abhängig vom Schweregrad der Erkrankung, Subtitel: (Messwerte zu Beginn der Therapie)
# bitte über den Graphik aufschreiben
dat$mortality_status <- ifelse(is.na(dat$Mortalitydate),"Lebt","Gestorben")

my_colors <- c("Lebt" = "#4DBBD5",
               "Gestorben" = "#E64B35")


p1 <- ggplot(dat, aes(mortality_status, SOFA)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "SOFA (aus 24P)") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(dat, aes(mortality_status, SAPS)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "SAPS") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(dat, aes(mortality_status, Leukocytes)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "Leukozyten (nL)") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(dat, aes(mortality_status, CRP)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "CRP (mg/dL)") +  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(
  p1, p2, p3, p4,
  ncol = 2)
## -----------------------------------

## Graphik: Welche Komorbiditäten begleiten die Mortalität am meisten?
dfKomor <- data.frame(
  cardiovascular = dat$Cardiovascular,
  hypertension   = dat$Hypertension,
  CHF            = dat$CHF,
  CKD            = dat$CKD,
  COPD           = dat$COPD,
  DM             = dat$DM,
  malignancy     = dat$Malignancy,
  mortality_status = ifelse(is.na(dat$Mortalitydate), "Lebt", "Gestorben")
)

dfbiditaet <- pivot_longer(
  dfKomor,
  cols = c(cardiovascular, hypertension, CHF, CKD, COPD, DM, malignancy),
  names_to = "Komorbiditaet",
  values_to = "Krankheit"
)

# Nur Patienten mit jeweiliger Komorbidität
dfexposed2 <- dfbiditaet %>%
  filter(tolower(Krankheit) == "yes")

# Counts nach Komorbidität und Status (für Balken)
count_df <- dfexposed2 %>%
  count(Komorbiditaet, mortality_status, name = "n_status")

# Mortalitätsrate + Gesamt-n pro Komorbidität (für Linie)
rate_df <- dfexposed2 %>%
  group_by(Komorbiditaet) %>%
  summarise(
    n_total = n(),
    mort_rate = mean(mortality_status == "Gestorben"),
    .groups = "drop"
  )

scale_factor <- max(rate_df$n_total)

order_levels <- rate_df %>%
  arrange(desc(n_total)) %>%
  pull(Komorbiditaet)

count_df$Komorbiditaet <- factor(count_df$Komorbiditaet, levels = order_levels)
rate_df$Komorbiditaet  <- factor(rate_df$Komorbiditaet,  levels = order_levels)

ggplot() +
  # Balken: Anzahl (gestapelt nach Status)
  geom_col(
    data = count_df,
    aes(x = Komorbiditaet, y = n_status, fill = mortality_status)
  ) +
  # Linie + Punkte: Mortalitätsrate (skaliert auf Count-Achse)
  geom_line(
    data = rate_df,
    aes(x = Komorbiditaet, y = mort_rate * scale_factor, group = 1),
    color = "red", linewidth = 1.2
  ) +
  geom_point(
    data = rate_df,
    aes(x = Komorbiditaet, y = mort_rate * scale_factor, color = "Mortalitätsrate", group = 1),
    size = 3
  ) +
  scale_color_manual(
    name = "",
    values = c("Mortalitätsrate" = "red")
  ) +
  scale_y_continuous(
    name = "Anzahl Patienten",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mortalitätsrate")
  ) +
  labs(x = "Komorbidität", fill = "Status") +
  theme_minimal()
## -------------------------------------------------------------------

## Graphik: Welche Nephrotoxine begleiten die Mortalität am meisten?
dfToxinen <- data.frame(
  acei = dat$ACEI,
  ARB = dat$ARB,
  Aminoglycosides = dat$Aminoglycosides,
  Loop = dat$Loop,
  NSAID = dat$NSAID,
  Piptaz = dat$PipTaz,
  vasopressors = dat$Vasopressors
)

dfToxinen$mortality_status <- ifelse(is.na(dat$Mortalitydate), "Lebt", "Gestorben")

dfNephro <- pivot_longer(
  dfToxinen,
  cols = c(acei, ARB, Aminoglycosides, Loop, NSAID, Piptaz, vasopressors),
  names_to = "Nephrotoxin",
  values_to = "Krankheit"
)

dfexposed <- dfNephro %>%
  filter(Krankheit == "yes") 

# Nur Patienten mit jeweiligem Nephrotoxin
dfexposed2 <- dfNephro %>%
  filter(tolower(Krankheit) == "yes")

# Counts nach Komorbidität und Status (für Balken)
count_df <- dfexposed2 %>%
  count(Nephrotoxin, mortality_status, name = "n_status")

# Mortalitätsrate + Gesamt-n pro Komorbidität (für Linie)
rate_df <- dfexposed2 %>%
  group_by(Nephrotoxin) %>%
  summarise(
    n_total = n(),
    mort_rate = mean(mortality_status == "Gestorben"),
    .groups = "drop"
  )

scale_factor <- max(rate_df$n_total)

order_levels <- rate_df %>%
  arrange(desc(n_total)) %>%
  pull(Nephrotoxin)

count_df$Nephrotoxin <- factor(count_df$Nephrotoxin, levels = order_levels)
rate_df$Nephrotoxin <- factor(rate_df$Nephrotoxin,  levels = order_levels)

ggplot() +
  # Balken: Anzahl (gestapelt nach Status)
  geom_col(
    data = count_df,
    aes(x = Nephrotoxin, y = n_status, fill = mortality_status)
  ) +
  # Linie + Punkte: Mortalitätsrate (skaliert auf Count-Achse)
  geom_line(
    data = rate_df,
    aes(x = Nephrotoxin, y = mort_rate * scale_factor, group = 1),
    color = "red", linewidth = 1.2
  ) +
  geom_point(
    data = rate_df,
    aes(x = Nephrotoxin, y = mort_rate * scale_factor, color = "Mortalitätsrate", group = 1),
    size = 3
  ) +
  scale_color_manual(
    name = "",
    values = c("Mortalitätsrate" = "red")
  ) +
  scale_y_continuous(
    name = "Anzahl Patienten",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mortalitätsrate")
  ) +
  labs(x = "Nephrotoxin", fill = "Status") +
  theme_minimal()
## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

## SCRAPPED --------------------------------------------------------------------

## Graphik: Welche Indikationen haben welche Mortalitätsraten?
dfMortInd <- data.frame(
  mortality = dat$Mortalitydate,
  sepsis = dat$Sepsis,
  schock = dat$Schock,
  bacteraemia = dat$Bacteraemia,
  catheter = dat$Catheter,
  bji = dat$BJI,
  endocarditis = dat$Endocarditis,
  cns = dat$CNS,
  gastrointestinal = dat$Gastrointestinal,
  genitourinary = dat$Genitourinary,
  pulmonary = dat$Pulmonary,
  ssti = dat$SSTI
)

dfMortInd$mortality_status <- ifelse(is.na(dfMortInd$mortality),"alive","dead")

long <- pivot_longer(dfMortInd,
                     cols = -c(mortality, mortality_status),
                     names_to = "indikation",
                     values_to = "vorhanden")

summary <- long %>%
  filter(vorhanden == "yes") %>%
  group_by(indikation) %>%
  summarise(mort_rate = mean(mortality_status=="dead"))

ggplot(summary, aes(indikation, mort_rate)) +
  geom_col()
## -----------------------------------------------------------

## Graphik: Scatterplots SCr x C Vergleich 24, 48, 72h
dat$nephrotox <- ifelse(
  rowSums(dat[, c("ACEI","ARB","Aminoglycosides","Loop",
                  "NSAID","PipTaz","Vasopressors")] == "yes",
          na.rm = TRUE) > 0,
  "ja","nein"
)

p1 <- ggplot(
  dat, aes(x = C24, y = SCr24, color = nephrotox)) +
  geom_point() +
  labs(
    title = "Scatterplot C24 vs. SCr24",
    x = "C24",
    y = "SCr24",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("nein" = "#5ac9c7", "ja" = "#ec5b5b"))

p2 <- ggplot(
  dat, aes(x = C48, y = SCr48, color = nephrotox)) +
  geom_point() +
  labs(
    title = "Scatterplot C48 vs. SCr48",
    x = "C48",
    y = "SCr48",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("nein" = "#5ac9c7", "ja" = "#ec5b5b"))

p3 <- ggplot(
  dat, aes(x = C72, y = SCr72, color = nephrotox)) +
  geom_point() +
  labs(
    title = "Scatterplot C72 vs. SCr72",
    x = "C72",
    y = "SCr72",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("nein" = "#5ac9c7", "ja" = "#ec5b5b"))

grid.arrange(p1, p2, p3, ncol = 3)
## ---------------------------------------------------

## Graphik: Auswirkung von erster Vancomycingabe auf Nierenmarker
dat$deltaSCr <- dat$SCr72 - dat$SCrStart

p1 <- ggplot(dat, aes(C24, deltaSCr, color = nephrotox)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(x="C24", y="ΔSCr")

dat$deltaeGFR <- dat$eGFR72 - dat$eGFRStart

p2 <- ggplot(dat, aes(C24, deltaeGFR, color = nephrotox)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm") +
  labs(x="C24", y="ΔeGFR")

grid.arrange(p1, p2, ncol = 2)
## --------------------------------------------------------------

## Graphik: Scatterplots eGFR x C Vergleich 24, 48, 72h
dat$nephrotox <- ifelse(
  rowSums(dat[, c("ACEI","ARB","Aminoglycosides","Loop",
                  "NSAID","PipTaz","Vasopressors")] == "yes",
          na.rm = TRUE) > 0,
  "ja","nein"
)

p1b <- ggplot(
  dat, aes(x = C24, y = eGFR24)) +
  geom_point() +
  labs(
    title = "Scatterplot C24 vs. eGFR24",
    x = "C24",
    y = "eGFR24",
  ) +
  geom_smooth(method = "lm")

p2b <- ggplot(
  dat, aes(x = C48, y = eGFR48)) +
  geom_point() +
  labs(
    title = "Scatterplot C48 vs. eGFR48",
    x = "C48",
    y = "eGFR48",
  ) +
  geom_smooth(method = "lm")

p3b <- ggplot(
  dat, aes(x = C72, y = eGFR72)) +
  geom_point() +
  labs(
    title = "Scatterplot C72 vs. eGFR72",
    x = "C72",
    y = "eGFR72",
  ) +
  geom_smooth(method = "lm")

grid.arrange(p1b, p2b, p3b, ncol = 3)
## ----------------------------------------------------

## Graphik: Korrelation zwischen Dosen
dat_dosis <- data.frame(
  c24 = dat$C24,
  c48 = dat$C48,
  c72 = dat$C72,
  md24 = dat$MD24,
  md48 = dat$MD48,
  md72 = dat$MD72,
  ld = dat$LD
)

corrplot(cor(dat_dosis, use="complete.obs"))
## ------------------------------------

## Graphik: Altersverteilung nach Vancomycindosis pro Körpergewicht (Mortalität)
dfAgeVanc <- data.frame(
  age = dat$Birthdate,
  mortality = dat$Mortalitydate,
  md24 = dat$MD24,
  md48 = dat$MD48,
  md72 = dat$MD72
)

dfAgeVanc$age <- as.numeric(difftime(Sys.Date(),
                                     as.Date(dfAgeVanc$age),
                                     units = "days"))/365.25

dfAgeVanc$mortality_status <- ifelse(is.na(dfAgeVanc$mortality),"alive","dead")

p1 <- ggplot(
  dfAgeVanc, aes(x = age, y = md24, color = mortality_status)) +
  geom_point() +
  labs(
    title = "Scatterplot Alter vs. MD24",
    x = "age",
    y = "md24",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("alive" = "#5ac9c7", "dead" = "#ec5b5b"))

p2 <- ggplot(
  dfAgeVanc, aes(x = age, y = md48, color = mortality_status)) +
  geom_point() +
  labs(
    title = "Scatterplot Alter vs. MD48",
    x = "age",
    y = "md48",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("alive" = "#5ac9c7", "dead" = "#ec5b5b"))

p3 <- ggplot(
  dfAgeVanc, aes(x = age, y = md72, color = mortality_status)) +
  geom_point() +
  labs(
    title = "Scatterplot Alter vs. MD72",
    x = "age",
    y = "md72",
  ) +
  geom_smooth(method = "lm") + 
  scale_color_manual(values = c("alive" = "#5ac9c7", "dead" = "#ec5b5b"))

grid.arrange(p1, p2, p3, ncol = 3)
## -----------------------------------------------------------------------------

## Graphik: Mortalität nach Alter
dat$mortality_status <- ifelse(is.na(dat$Mortalitydate),"alive","dead")
dat$age <- as.numeric(difftime(Sys.Date(),
                               as.Date(dat$Birthdate),
                               units = "days"))/365.25

ggplot(dat, aes(mortality_status, age)) +
  geom_violin()
## ------------------------------

## Graphik: Verteilung des Unterschieds zwischen SCrStart und SCr72
dat$deltaSCr <- dat$SCr72 - dat$SCrStart

dens <- density(na.omit(dat$deltaSCr), bw = "nrd0", adjust = 1, 
                kernel = "gaussian")
plot(dens, main = "SCr-Verteilung SCr72 - SCrStart", xlab = "deltaSCr", 
     ylab = "density")
rug(dat$deltaSCr)
## ----------------------------------------------------------------

## Graphik: Verschlechtern Nephrotoxine die Niere?
df <- data.frame(
  c24 = dat$C24,
  scr24 = dat$SCr24,
  c48 = dat$C48,
  scr48 = dat$SCr48,
  c72 = dat$C72,
  scr72 = dat$SCr72,
  acei = dat$ACEI,
  arb = dat$ARB,
  aminoglycosides = dat$Aminoglycosides,
  loop = dat$Loop,
  nsaid = dat$NSAID,
  piptaz = dat$PipTaz,
  vasopressors = dat$Vasopressors
)

dat$nephrotox <- ifelse(
  rowSums(df[, c("acei","arb","aminoglycosides","loop",
                 "nsaid","piptaz","vasopressors")] == "yes",
          na.rm = TRUE) > 0,
  "ja","nein"
)

dat$deltaSCr <- dat$SCr72 - dat$SCrStart

ggplot(dat, aes(nephrotox, deltaSCr)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.05)
## -----------------------------------------------

## Graphik: Wurden älteren Patienten häufiger Nephrotoxine verabreicht?
dfalterNephro <- data.frame(
  alter = dat$Birthdate,
  acei = dat$ACEI,
  arb = dat$ARB,
  aminoglycosides = dat$Aminoglycosides,
  loop = dat$Loop,
  nsaid = dat$NSAID,
  piptaz = dat$PipTaz,
  vasopressors = dat$Vasopressors
)

dfalterNephro$alter <- as.numeric(difftime(Sys.Date(),
                                           as.Date(dfalterNephro$alter),
                                           units = "days"))/365.25

long <- pivot_longer(dfalterNephro,
                     cols = -alter,
                     names_to = "medikament",
                     values_to = "gegeben")

ggplot(long, aes(x = factor(gegeben), y = alter)) +
  geom_boxplot() +
  facet_wrap(~ medikament)
## --------------------------------------------------------------------

## Graphik: Welche Komorbiditäten begleiten die Mortalität am meisten?
dfMortKom <- data.frame(
  mortality = dat$Mortalitydate,
  cardiovascular = dat$Cardiovascular,
  hypertension = dat$Hypertension,
  chf = dat$CHF,
  ckd = dat$CKD,
  copd = dat$COPD,
  dm = dat$DM,
  malignacy = dat$Malignancy
)

dfMortKom$mortality_status <- ifelse(is.na(dfMortKom$mortality),"alive","dead")

long <- pivot_longer(dfMortKom,
                     cols = -c(mortality, mortality_status),
                     names_to = "comorbidity",
                     values_to = "vorhanden")

summary <- long %>%
  filter(vorhanden == "yes") %>%
  group_by(comorbidity) %>%
  summarise(mort_rate = mean(mortality_status=="dead"))

ggplot(summary, aes(comorbidity, mort_rate)) +
  geom_col()
## -------------------------------------------------------------------



## -----------------------------------------------------------------------------
