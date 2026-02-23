################################################################################
## Abschlussprojekt - Sergio Alvarez, Ilkay Aydin, Jakub Marczak, Nermine Msolly
################################################################################

## DATEN LADEN -----------------------------------------------------------------
## load("/Users/---/Downloads/vancomycin.RData")
## -----------------------------------------------------------------------------

## PAKETE LADEN ----------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)
library(dplyr)
library(corrplot)
library(hexbin)
library(scales)
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
  geom_density(linewidth = 0.5) +
  geom_rug(alpha = 0.15, linewidth = 0.5) +
  facet_wrap(~ variable, scales = "free_x", nrow = 1) +
  labs(x = NULL, y = "Dichte", color = "Geschlecht") +
  scale_color_manual(values = c("male" = "#5ac9c7", "female" = "#ec5b5b"),
                     labels = c("männlich", "weiblich")) +
  guides(color = guide_legend(override.aes = list(linewidth = 0.5))) +
  theme_minimal() + theme(
    text = element_text(size = 9),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6),
    plot.title = element_text(size = 13),
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.title = element_text(size=7)
  )

ggsave("population.pdf",
       width = 14,
       height = 6.5,
       units = "cm")

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
         tl.col = "black", tl.cex = 1, tl.srt = 45, cl.cex = 0.75, cl.ratio = 0.2, cl.offset = 1)
par(opar)
dev.off()
## -------------------------------

## Auswirkung vom durchschnittl. Vancomycin-Spiegel auf Nierenmarker
dat$C_mean <- rowMeans(dat[,c("C24","C48","C72")], na.rm=TRUE)
dat$deltaSCr <- dat$SCr72 - dat$SCrStart
dat$deltaeGFR <- dat$eGFR72 - dat$eGFRStart

longC <- dat %>%
  select(C24, C48, C72) %>%
  pivot_longer(cols = everything(),
               names_to = "time",
               values_to = "C")

## [Vancomycin-Spiegel über 3 Tage verteilt, nicht in der Präsentation]
ggplot(longC, aes(time, C)) +
  geom_boxplot()

p1 <- ggplot(dat, aes(x = C_mean, y = deltaSCr)) +
  geom_hex(alpha = 0.75, bins = 41) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_fill_continuous(name = "Patienten") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"SCr"),
    
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.x = unit(0.1, "cm")
        )

p3 <- ggplot(dat, aes(x = C_mean, y = deltaeGFR)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_fill_continuous(name = "Patienten") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"eGFR")
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.x = unit(0.1, "cm")
        )

longC <- dat %>%
  select(C24, C48, C72) %>%
  pivot_longer(cols = everything(),
               names_to = "time",
               values_to = "C")

p2 <- ggplot(dat, aes(x = C_mean, y = deltaeGFR)) +
  geom_hex(alpha = 0.75, bins = 41) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  scale_fill_continuous(name = "Patienten") +
  labs(
    x = expression(bar(C)),
    y = expression(Delta*"eGFR")
  ) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.x = unit(0.1, "cm"))

plot_save_1 <- grid.arrange(p1, p2, ncol = 2)

ggsave("ZhSCreGFR.pdf",
       plot = plot_save_1,
       width = 19,
       height = 7,
       units = "cm")

plot_save_2 <- grid.arrange(p3, p2, ncol = 2)

ggsave("HexScat.pdf",
       plot = plot_save_2,
       width = 19,
       height = 7,
       units = "cm")

## -----------------------------------------------------------------

## Graphik: Nierenfunktion verglichen mit Vancomycin Spiegel
ggplot(dat, aes(x = eGFRStart, y = C24)) +
  geom_point(aes(color = LD), alpha = 0.8) +
  geom_smooth(method = "gam", color = "hotpink2", linewidth = 1, se= FALSE) +
  scale_color_viridis_c() +
  guides(color = guide_colorbar(
    frame.colour = "black",
    frame.linewidth = 0.3,
    ticks.colour = "black"
  )) +
  labs(x = expression(paste(eGFR[Start], " (ml/min)")),
       y = expression(paste(C[24], " (mg/L)")),
       color = "LD (mg/kg)"
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8),
        axis.title.x = element_text(size = 8, margin = margin(t = 8)),
        axis.title.y = element_text(size = 8, margin = margin(r = 8)),
        legend.title = element_text(size=8),
        legend.text = element_text(size=7),
        plot.margin = margin(10, 10, 10, 15, unit = "pt")
  )
ggsave("C24xeGFRStart.pdf",
       width = 11,
       height = 7,
       units = "cm")


#--------------------------------

#Graphik: Mortalität in Abhängigkeit vom Vancomycin-Spiegel nach 24h

plot_data_smooth <- dat %>%
  filter(!is.na(eGFR24), !is.na(SAPS), !is.na(SOFA)) %>%
  select(eGFR24, SAPS, SOFA) %>%
  pivot_longer(cols = c(SAPS, SOFA), names_to = "Score", values_to = "Punkte") %>%
  mutate(Score_Label = ifelse(Score == "SAPS", "SAPS Score", "SOFA Score"))

ggplot(plot_data_smooth, aes(x = eGFR24, y = Punkte, color = Score, fill = Score)) +
  geom_smooth(method = "loess", span = 0.8, linewidth = 1.5, alpha = 0.25, se = TRUE) +
  facet_wrap(~ Score_Label, scales = "free_y") +
  scale_color_manual(values = c("SAPS" = "#2c7fb8", "SOFA" = "#e34a33")) +
  scale_fill_manual(values = c("SAPS" = "#2c7fb8", "SOFA" = "#e34a33")) +
  scale_x_continuous(limits = c(0, NA)) +
  labs(
    x = "eGFR [ml/min]",
    y = "Punkte im Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(2, "lines")
  )

ggsave("SAPSSOFAeGFR.pdf", width = 16, height = 8.5, units = "cm")


## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

################################################################################

## BEREICH MORTALITÄTSANALYSE --------------------------------------------------

## ---------------------------------------------------------------------

#Graphik:Prozentuale Abweichung der klinischen Parameter bei Verstorbenen (Referenz: Überlebende)

dat <- dat %>%
  mutate(Status = ifelse(is.na(Mortalitydate), "Überlebt", "Verstorben"))

dat %>%
  filter(!is.na(eGFRStart)) %>%
  ggplot(aes(x = Status, y = eGFRStart)) +
  geom_boxplot(width = 0.45, outlier.shape = NA, fill= "grey85", color="black") +
  geom_jitter(width = 0.12, alpha = 0.1, size = 1.2, color = "grey40") +
  coord_flip() +
  labs(
    x = NULL,
    y = expression(paste(eGFR[Start]))
  ) +
  theme_minimal(base_size = 13)+
  theme( 
    legend.position = "FALSE",
    plot.margin = margin(10, 25, 10, 10, unit = "pt"),
    axis.title.x = element_text(size= 10),
    panel.grid.major.y = element_blank()
  )

ggsave("eGFRxMort.pdf", 
       width = 16, height = 8.5, 
       units = "cm")

## ---------------------------------------------------------------------

## Graphik: Schweregrad der Erkrankung 
#Mortalität abhängig vom Schweregrad der Erkrankung, Subtitel: (Messwerte zu Beginn der Therapie)
# bitte über den Graphik aufschreiben
dat$mortality_status <- ifelse(is.na(dat$Mortalitydate),"Lebend","Gestorben")

my_colors <- c("Lebend" = "#5DA5DA",
               "Gestorben" = "#C44E52")


p1 <- ggplot(dat, aes(mortality_status, SOFA)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "SOFA (aus 24P)") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 15))

p2 <- ggplot(dat, aes(mortality_status, SAPS)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "SAPS") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 15))

p3 <- ggplot(dat, aes(mortality_status, Leukocytes)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "Leukozyten (nL)") +
  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 15))

p4 <- ggplot(dat, aes(mortality_status, CRP)) +
  geom_boxplot() +
  labs(x = "Lebenstatus", y = "CRP (mg/dL)") +  aes(fill = mortality_status) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 15))

grid.arrange(
  p1, p2, p3, p4,
  ncol = 2)

## -----------------------------------

## Graphik: Welche Komorbiditäten begleiten die Mortalität am meisten?
dfKomor <- data.frame(
  Cardiovascular = dat$Cardiovascular,
  Hypertension   = dat$Hypertension,
  CHF            = dat$CHF,
  CKD            = dat$CKD,
  COPD           = dat$COPD,
  DM             = dat$DM,
  Malignancy     = dat$Malignancy,
  mortality_status = ifelse(is.na(dat$Mortalitydate), "Lebend", "Gestorben")
)

dfbiditaet <- pivot_longer(
  dfKomor,
  cols = c(Cardiovascular, Hypertension, CHF, CKD, COPD, DM, Malignancy),
  names_to = "Komorbiditaet",
  values_to = "Krankheit"
)

dfexposed2 <- dfbiditaet %>%
  filter(tolower(Krankheit) == "yes")

count_df <- dfexposed2 %>%
  count(Komorbiditaet, mortality_status, name = "n_status")

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
  geom_col(
    data = count_df,
    aes(x = Komorbiditaet, y = n_status, fill = mortality_status)
  ) +
  scale_fill_manual(
    values = c(
      "Lebend" = "#5DA5DA",
      "Gestorben" = "#C44E52"
    )
  ) +
  geom_line(
    data = rate_df,
    aes(x = Komorbiditaet, y = mort_rate * scale_factor, group = 1),
    color = "black", linewidth = 1.2
  ) +
  geom_point(
    data = rate_df,
    aes(x = Komorbiditaet, y = mort_rate * scale_factor, color = "Mortalitätsrate", group = 1),
    size = 3
  ) +
  scale_color_manual(
    name = "",
    values = c("Mortalitätsrate" = "black")
  ) +
  scale_y_continuous(
    name = "Anzahl Patienten",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mortalitätsrate")
  ) +
  labs(x = "Komorbidität", fill = "Status") +
  theme_minimal() +
  theme( axis.title.y = element_text(size = 17),
         axis.title.y.right = element_text(size = 17),
         axis.text.y = element_text(size = 15),
         axis.text.y.right = element_text(size = 15),
         axis.title.x = element_text(size = 17),
         axis.text.x = element_text(size = 15),
         
         legend.title = element_text(size = 17),
         legend.text  = element_text(size = 15),
         panel.grid.major = element_blank()
  )
## -------------------------------------------------------------------

## Graphik: Welche Nephrotoxine begleiten die Mortalität am meisten?
dfToxinen <- data.frame(
  ACEI = dat$ACEI,
  ARB = dat$ARB,
  Aminoglycosides = dat$Aminoglycosides,
  Loop = dat$Loop,
  NSAID = dat$NSAID,
  PipTaz = dat$PipTaz,
  Vasopressors = dat$Vasopressors
)

dfToxinen$mortality_status <- ifelse(is.na(dat$Mortalitydate), "Lebend", "Gestorben")

dfNephro <- pivot_longer(
  dfToxinen,
  cols = c(ACEI, ARB, Aminoglycosides, Loop, NSAID, PipTaz, Vasopressors),
  names_to = "Nephrotoxin",
  values_to = "Krankheit"
)

dfexposed <- dfNephro %>%
  filter(Krankheit == "yes") 

dfexposed2 <- dfNephro %>%
  filter(tolower(Krankheit) == "yes")

count_df <- dfexposed2 %>%
  count(Nephrotoxin, mortality_status, name = "n_status")

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
  geom_col(
    data = count_df,
    aes(x = Nephrotoxin, y = n_status, fill = mortality_status)
  ) +
  scale_fill_manual(
    values = c(
      "Lebend" = "#5DA5DA",
      "Gestorben" = "#C44E52"
    )
  ) +
  geom_line(
    data = rate_df,
    aes(x = Nephrotoxin, y = mort_rate * scale_factor, group = 1),
    color = "black", linewidth = 1.2
  ) +
  geom_point(
    data = rate_df,
    aes(x = Nephrotoxin, y = mort_rate * scale_factor, color = "Mortalitätsrate", group = 1),
    size = 3
  ) +
  scale_color_manual(
    name = "",
    values = c("Mortalitätsrate" = "black")
  ) +
  scale_y_continuous(
    name = "Anzahl Patienten",
    sec.axis = sec_axis(~ . / scale_factor, name = "Mortalitätsrate"),
  ) +
  labs(x = "Nephrotoxin", fill = "Status") +
  theme_minimal() +
  theme( axis.title.y = element_text(size = 17),
        axis.title.y.right = element_text(size = 17),
        axis.text.y = element_text(size = 15),
        axis.text.y.right = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.text.x = element_text(size = 15),
        
        legend.title = element_text(size = 17),
        legend.text  = element_text(size = 15),
        panel.grid.major = element_blank()
    ) 
  
## ---------------------------------------------------------------------

## GRAPHIK 7: Therapiedauer nach Mortalitätsstatus -----------------------------

dat$mortality_status <- ifelse(is.na(dat$Mortalitydate), "Lebt", "Gestorben")

dat <- dat %>%
  mutate(
    Start         = as.Date(Start),
    End           = as.Date(End),
    Therapiedauer = as.numeric(difftime(End, Start, units = "days"))
  ) %>%
  filter(!is.na(Therapiedauer), Therapiedauer >= 0)

n_counts <- dat %>%
  count(mortality_status) %>%
  mutate(lbl = paste0(mortality_status, ": n=", n)) %>%
  pull(lbl) %>%
  paste(collapse = " | ")

g_duration <- ggplot(dat, aes(x = mortality_status, y = Therapiedauer, fill = mortality_status)) +
  geom_boxplot(width = 0.55, alpha = 0.85, outlier.alpha = 0.25) +
  geom_jitter(width = 0.12, alpha = 0.08, size = 1) +
  scale_fill_manual(values = my_colors) +
  labs(
    title    = "Therapiedauer nach Mortalitätsstatus",
    subtitle = paste0("Dauer der kontinuierlichen Vancomycin-Infusion (", n_counts, ")"),
    x        = "Lebenstatus",
    y        = "Therapiedauer (Tage)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title      = element_text(face = "bold"))

print(g_duration)

## GRAPHIK 8 ------------------------------------------------------------------

dat_c24_sum <- dat %>%
  filter(!is.na(C24)) %>%
  mutate(
    C24_Quartil = factor(ntile(C24, 4),
                         levels = 1:4,
                         labels = c("Q1 (niedrig)", "Q2", "Q3", "Q4 (hoch)")),
    gestorben = mortality_status == "Gestorben"
  ) %>%
  group_by(C24_Quartil) %>%
  summarise(
    Mortalitaetsrate = mean(gestorben, na.rm = TRUE),
    C24_median       = median(C24, na.rm = TRUE),
    n                = n(),
    .groups          = "drop"
  )

g_c24 <- ggplot(dat_c24_sum, aes(x = C24_Quartil, y = Mortalitaetsrate, 
                                 fill = C24_median)) +
  geom_col(alpha = 0.9, width = 0.62) +
  geom_text(aes(label = paste0(percent(Mortalitaetsrate, accuracy = 0.1), 
                               "\n(n=", n, ")")),
            vjust = -0.35, size = 4.5, fontface = "bold") +
  scale_fill_viridis_c(
    option = "viridis",
    name   = "C24 Median\n(mg/L)"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Mortalitätsrate nach C24-Serumspiegel",
    subtitle = "Quartile des C24-Spiegels | Farbe = medianer C24-Wert",
    x        = "C24-Quartil",
    y        = "Mortalitätsrate"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 10, color = "gray40"),
    legend.position    = "right",
    legend.key.height  = unit(2, "cm"),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(g_c24)

## SAVE -----------------------------------------------------------------------

ggsave("therapiedauer.png",   g_duration, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("mortalitaet_c24.png", g_c24,      width = 10, height = 6, dpi = 300, bg = "white")
## -----------------------------------------------------------------------------

## SCRAPPED -- DIESE GRAPHIKEN SIND NICHT IN DER PRÄSENTATION ------------------

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