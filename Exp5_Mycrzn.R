#Exp5 micorrización y crecimiento de la planta por tipo de inóculo
#Paper: L.t._SExt
#The dose makes the poison
#Rodolfo Ángeles, 11-12/12/25

#1. Load data
#2. Normality
#3. ANOVA
#4. Tukey
#5. Statistics
#6. Boxplot
#7. Save files

# loading the appropriate libraries
library(datasets)
library(ggplot2)
library(multcomp)
library(multcompView)
library(dplyr)
library(readr)
library(tidyr)
library(paletteer)
library(car)

#ir al directorio con los datos
#setwd("/Users/re.anar/Documents/0_INECOL/Estudiantes/3_JesusContreras_MAST/PalPaper")
# Leer el archivo CSV, tratando los "-" como NA
data <- read.csv("data/Exp5_Mycrzn.csv", na.strings = "-")

# Paso 1: Crear la columna de Tratamient a partir de Media
#####
data_summary <- data %>%
  mutate(Treatment = as.factor(Media))

# Convertir variables respuesta a numeric
response_vars <- c("Mycorrhization3m", "Mycorrhization12m",
                   "Diametermm3m", "Diametermm12m",
                   "Heightcm3m", "Heightcm12m")

data_summary <- data_summary %>%
  mutate(across(all_of(response_vars), as.numeric))
#Remover filas completamente vacías (planta muerta)
data_summary <- data_summary %>%
  filter(!if_all(response_vars, is.na))

# Verificar la estructura de los datos después de la conversión
#head(data_summary)
str(data_summary)

#####
# Paso 2: Verificar normalidad de los datos por grupo
#####

#variables de 3 meses
#Altura de la planta
PH3m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Heightcm3m)$p.value)

#Diametro del tallo
SD3m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Diametermm3m)$p.value)

#% de Mycorrhización
M3m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Mycorrhizas3m)$p.value)

#variables de 12 meses
#Altura de la planta
PH12m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Heightcm12m)$p.value)

#Diametro del tallo
SD12m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Diametermm12m)$p.value)

#% de Mycorrhización
M12m_normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(Mycorrhizas12m)$p.value)

#####
# Paso 3: Análisis de ANOVA
#####

#3 months
PH3m_anova_result <- aov(Heightcm3m ~ Treatment, data = data_summary)
PH3m_anova_summary <- summary(PH3m_anova_result)

SD3m_anova_result <- aov(Diametermm3m ~ Treatment, data = data_summary)
SD3m_anova_summary <- summary(SD3m_anova_result)

M3m_anova_result <- aov(Mycorrhizas3m ~ Treatment, data = data_summary)
M3m_anova_summary <- summary(M3m_anova_result)

#12 months
PH12m_anova_result <- aov(Heightcm12m ~ Treatment, data = data_summary)
PH12m_anova_summary <- summary(PH12m_anova_result)

SD12m_anova_result <- aov(Diametermm12m ~ Treatment, data = data_summary)
SD12m_anova_summary <- summary(SD12m_anova_result)

M12m_anova_result <- aov(Mycorrhizas12m ~ Treatment, data = data_summary)
M12m_anova_summary <- summary(M12m_anova_result)

#####
# Paso 4: Prueba de Tukey
#####
#3 months
PH3m_tukey_result <- TukeyHSD(PH3m_anova_result)
PH3m_tukey_cld <- multcompLetters4(PH3m_anova_result, PH3m_tukey_result)
# Crear tabla de letras de Tukey
PH3m_cld_table <- as.data.frame.list(PH3m_tukey_cld$Treatment)
PH3m_cld_table$Treatment <- rownames(PH3m_cld_table)

#
SD3m_tukey_result <- TukeyHSD(SD3m_anova_result)
SD3m_tukey_cld <- multcompLetters4(SD3m_anova_result, SD3m_tukey_result)
SD3m_cld_table <- as.data.frame.list(SD3m_tukey_cld$Treatment)
SD3m_cld_table$Treatment <- rownames(SD3m_cld_table)
#
M3m_tukey_result <- TukeyHSD(M3m_anova_result)
M3m_tukey_cld <- multcompLetters4(M3m_anova_result, M3m_tukey_result)
M3m_cld_table <- as.data.frame.list(M3m_tukey_cld$Treatment)
M3m_cld_table$Treatment <- rownames(M3m_cld_table)

#12 months
PH12m_tukey_result <- TukeyHSD(PH12m_anova_result)
PH12m_tukey_cld <- multcompLetters4(PH12m_anova_result, PH12m_tukey_result)
PH12m_cld_table <- as.data.frame.list(PH12m_tukey_cld$Treatment)
PH12m_cld_table$Treatment <- rownames(PH12m_cld_table)
#
SD12m_tukey_result <- TukeyHSD(SD12m_anova_result)
SD12m_tukey_cld <- multcompLetters4(SD12m_anova_result, SD12m_tukey_result)
SD12m_cld_table <- as.data.frame.list(SD12m_tukey_cld$Treatment)
SD12m_cld_table$Treatment <- rownames(SD12m_cld_table)
#
M12m_tukey_result <- TukeyHSD(M12m_anova_result)
M12m_tukey_cld <- multcompLetters4(M12m_anova_result, M12m_tukey_result)
M12m_cld_table <- as.data.frame.list(M12m_tukey_cld$Treatment)
M12m_cld_table$Treatment <- rownames(M12m_cld_table)

#####
# Paso 5: Crear tablas con estadísticas descriptivas
#####
#3m
PH3m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Heightcm3m, na.rm = TRUE),
            SD   = sd(Heightcm3m, na.rm = TRUE),
            N    = sum(!is.na(Heightcm3m)))
PH3m_summary <- left_join(PH3m_summary, PH3m_cld_table,
                          by = c("Media" = "Treatment"))
#View(PH3m_summary)

#
SD3m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Diametermm3m, na.rm = TRUE),
            SD   = sd(Diametermm3m, na.rm = TRUE),
            N    = sum(!is.na(Diametermm3m)))
SD3m_summary <- left_join(SD3m_summary, SD3m_cld_table,
                          by = c("Media" = "Treatment"))
#View(SD3m_summary)
#
M3m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Mycorrhization3m, na.rm = TRUE),
            SD   = sd(Mycorrhization3m, na.rm = TRUE),
            N    = sum(!is.na(Mycorrhization3m)))
M3m_summary <- left_join(M3m_summary, M3m_cld_table,
                          by = c("Media" = "Treatment"))
#View(M3m_summary)

#12m
PH12m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Heightcm12m, na.rm = TRUE),
            SD   = sd(Heightcm12m, na.rm = TRUE),
            N    = sum(!is.na(Heightcm12m)))
PH12m_summary <- left_join(PH12m_summary, PH12m_cld_table,
                          by = c("Media" = "Treatment"))
#View(PH12m_summary)

#
SD12m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Diametermm12m, na.rm = TRUE),
            SD   = sd(Diametermm12m, na.rm = TRUE),
            N    = sum(!is.na(Diametermm12m)))
SD12m_summary <- left_join(SD12m_summary, SD12m_cld_table,
                          by = c("Media" = "Treatment"))
#View(SD12m_summary)
#
M12m_summary <- data %>%
  group_by(Media) %>%
  summarise(Mean = mean(Mycorrhization12m, na.rm = TRUE),
            SD   = sd(Mycorrhization12m, na.rm = TRUE),
            N    = sum(!is.na(Mycorrhization12m)))
M12m_summary <- left_join(M12m_summary, M12m_cld_table,
                         by = c("Media" = "Treatment"))
#View(M12m_summary)

#####
# Paso 6: Crear los boxplots con las letras de Tukey#
#####
#colores
media_colors <- c(
  "Control" = "#FED789FF",
  "BAF"     = "#453947FF",
  "exH2O"   = "#023743FF",
  "exOH"    = "#72874EFF")

#Mycorrhization
# Seleccionar y reordenar datos de micorrización
myc_data <- data %>%
  dplyr::select(Media, Mycorrhization3m, Mycorrhization12m) %>%
  tidyr::pivot_longer(
    cols = c(Mycorrhization3m, Mycorrhization12m),
    names_to = "Time",
    values_to = "Mycorrhization"
  ) %>%
  dplyr::mutate(
    Time = dplyr::recode(
      Time,
      "Mycorrhization3m"  = "3 months",
      "Mycorrhization12m" = "12 months"
    ),
    Time = factor(Time, levels = c("3 months", "12 months")),
    Media = factor(
      Media,
      levels = c("Control", "BAF", "exH2O", "exOH")
    )
  )
#Tukey
myc_letters <- dplyr::bind_rows(
  M3m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "3 months"),
  
  M12m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "12 months"))
myc_letters <- myc_letters %>%
  mutate(
    x_pos = as.numeric(factor(Media,
                              levels = c("Control", "BAF", "exH2O", "exOH")
    )),
    x_pos = ifelse(Time == "3 months",
                   x_pos - 0.2,
                   x_pos + 0.2))
myc_letters <- myc_letters %>%
  mutate(
    Letters = ifelse(
      Time == "12 months",
      toupper(Letters),
      Letters
    )
  )

#Graficar Myc
myc_boxplot <- ggplot(myc_data,
                      aes(x = Media,
                          y = Mycorrhization,
                          fill = Media)) +
  # Cajas
  geom_boxplot(
    aes(color = Media, linetype = Time),
    outlier.shape = NA,
    alpha = 0.3,
    linewidth = 1) +
  # Puntos individuales
  geom_jitter(
    aes(color = Media),
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.9) +
  # Letras de Tukey
  geom_text(
    data = myc_letters,
    aes(x = x_pos,
        y = max(myc_data$Mycorrhization, na.rm = TRUE) + 2,
        label = Letters),
        inherit.aes = FALSE,
        size = 4) +
  # Escalas
  scale_fill_manual(values = media_colors) +
  scale_color_manual(values = media_colors) +
  scale_linetype_manual(
    values = c(
      "3 months"  = "dashed",
      "12 months" = "solid"),
    name = "Time") +
  # Etiquetas
  labs(
    title = "Mycorrhization (%) by growth medium and time",
    x = "Culture medium",
    y = "Mycorrhization (%)") +
  # Tema
  theme_bw() + theme(
    panel.border = element_rect(colour = "black", linewidth = 1.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right")
# Imprimir en pantalla
print(myc_boxplot)

#Stem Diameter
# Seleccionar y reordenar datos de diámetro del tallo
sd_data <- data %>%
  dplyr::select(Media, Diametermm3m, Diametermm12m) %>%
  tidyr::pivot_longer(
    cols = c(Diametermm3m, Diametermm12m),
    names_to = "Time",
    values_to = "Diameter"
  ) %>%
  dplyr::mutate(
    Time = dplyr::recode(
      Time,
      "Diametermm3m"  = "3 months",
      "Diametermm12m" = "12 months"
    ),
    Time = factor(Time, levels = c("3 months", "12 months")),
    Media = factor(
      Media,
      levels = c("Control", "BAF", "exH2O", "exOH")
    )
  )
#Tukey
sd_letters <- dplyr::bind_rows(
  SD3m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "3 months"),
  
  SD12m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "12 months"))
sd_letters <- sd_letters %>%
  mutate(
    x_pos = as.numeric(factor(Media,
                              levels = c("Control", "BAF", "exH2O", "exOH")
    )),
    x_pos = ifelse(Time == "3 months",
                   x_pos - 0.2,
                   x_pos + 0.2))
sd_letters <- sd_letters %>%
  mutate(
    Letters = ifelse(
      Time == "12 months",
      toupper(Letters),
      Letters
    )
  )

#Graficar SD
sd_boxplot <- ggplot(sd_data,
                      aes(x = Media,
                          y = Diameter,
                          fill = Media)) +
  # Cajas
  geom_boxplot(
    aes(color = Media, linetype = Time),
    outlier.shape = NA,
    alpha = 0.3,
    linewidth = 1) +
  # Puntos individuales
  geom_jitter(
    aes(color = Media),
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.9) +
  # Letras de Tukey
  geom_text(
    data = sd_letters,
    aes(x = x_pos,
        y = max(sd_data$Diameter, na.rm = TRUE) + .2,
        label = Letters),
    inherit.aes = FALSE,
    size = 4) +
  # Escalas
  scale_fill_manual(values = media_colors) +
  scale_color_manual(values = media_colors) +
  scale_linetype_manual(
    values = c(
      "3 months"  = "dashed",
      "12 months" = "solid"),
    name = "Time") +
  # Etiquetas
  labs(
    title = "Stem diameter (mm) by growth medium and time",
    x = "Culture medium",
    y = "Stem diameter (mm)") +
  # Tema
  theme_bw() + theme(
    panel.border = element_rect(colour = "black", linewidth = 1.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right")
# Imprimir en pantalla
print(sd_boxplot)

#Plant height
# Seleccionar y reordenar datos de altura de la planta
ph_data <- data %>%
  dplyr::select(Media, Heightcm3m, Heightcm12m) %>%
  tidyr::pivot_longer(
    cols = c(Heightcm3m, Heightcm12m),
    names_to = "Time",
    values_to = "Height"
  ) %>%
  dplyr::mutate(
    Time = dplyr::recode(
      Time,
      "Heightcm3m"  = "3 months",
      "Heightcm12m" = "12 months"
    ),
    Time = factor(Time, levels = c("3 months", "12 months")),
    Media = factor(
      Media,
      levels = c("Control", "BAF", "exH2O", "exOH")
    )
  )
#Tukey
ph_letters <- dplyr::bind_rows(
  PH3m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "3 months"),
  
  PH12m_cld_table %>%
    dplyr::rename(Media = Treatment) %>%
    dplyr::mutate(Time = "12 months"))
ph_letters <- ph_letters %>%
  mutate(
    x_pos = as.numeric(factor(Media,
                              levels = c("Control", "BAF", "exH2O", "exOH")
    )),
    x_pos = ifelse(Time == "3 months",
                   x_pos - 0.2,
                   x_pos + 0.2))
ph_letters <- ph_letters %>%
  mutate(
    Letters = ifelse(
      Time == "12 months",
      toupper(Letters),
      Letters
    )
  )

#Graficar PH
ph_boxplot <- ggplot(ph_data,
                     aes(x = Media,
                         y = Height,
                         fill = Media)) +
  # Cajas
  geom_boxplot(
    aes(color = Media, linetype = Time),
    outlier.shape = NA,
    alpha = 0.3,
    linewidth = 1) +
  # Puntos individuales
  geom_jitter(
    aes(color = Media),
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.9) +
  # Letras de Tukey
  geom_text(
    data = sd_letters,
    aes(x = x_pos,
        y = max(ph_data$Height, na.rm = TRUE) + .2,
        label = Letters),
    inherit.aes = FALSE,
    size = 4) +
  # Escalas
  scale_fill_manual(values = media_colors) +
  scale_color_manual(values = media_colors) +
  scale_linetype_manual(
    values = c(
      "3 months"  = "dashed",
      "12 months" = "solid"),
    name = "Time") +
  # Etiquetas
  labs(
    title = "Plant height (cm) by growth medium and time",
    x = "Culture medium",
    y = "Plant height (cm)") +
  # Tema
  theme_bw() + theme(
    panel.border = element_rect(colour = "black", linewidth = 1.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right")
# Imprimir en pantalla
print(ph_boxplot)

#####
#7. Paso 7: Guardar resultados en archivos
#####
#Myc
# Guardar Shapiro
#print(M3m_normality_tests)
write.csv(as.data.frame(M3m_normality_tests),
          "out/Exp5.M3m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
#print(M3m_anova_summary)
write.csv(as.data.frame(M3m_anova_summary[[1]]),
          "out/Exp5.M3m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(M3m_tukey_result$Treatment),
          "out/Exp5.M3m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(M3m_summary, "out/Exp5.M3m_Summary.csv", row.names = FALSE)
#
# Guardar Shapiro
write.csv(as.data.frame(M12m_normality_tests),
          "out/Exp5.M12m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
write.csv(as.data.frame(M12m_anova_summary[[1]]),
          "out/Exp5.M12m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(M12m_tukey_result$Treatment),
          "out/Exp5.M12m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(M12m_summary, "out/Exp5.M12m_Summary.csv", row.names = FALSE)
## Guardar la gráfica de mycorrhización
#ggsave("out/Exp5.Myc_Boxplot.pdf", myc_boxplot, width = 8, height = 6)

#SD
# Guardar Shapiro
#print(SD3m_normality_tests)
write.csv(as.data.frame(SD3m_normality_tests),
          "out/Exp5.SD3m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
#print(SD3m_anova_summary)
write.csv(as.data.frame(SD3m_anova_summary[[1]]),
          "out/Exp5.SD3m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(SD3m_tukey_result$Treatment),
          "out/Exp5.SD3m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(SD3m_summary, "out/Exp5.SD3m_Summary.csv", row.names = FALSE)
#
# Guardar Shapiro
write.csv(as.data.frame(SD12m_normality_tests),
          "out/Exp5.SD12m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
write.csv(as.data.frame(SD12m_anova_summary[[1]]),
          "out/Exp5.SD12m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(SD12m_tukey_result$Treatment),
          "out/Exp5.SD12m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(SD12m_summary, "out/Exp5.SD12m_Summary.csv", row.names = FALSE)
## Guardar la gráfica de mycorrhización
#ggsave("out/Exp5.sd_boxplot.pdf", sd_boxplot, width = 8, height = 6)

#PH
# Guardar Shapiro
#print(PH3m_normality_tests)
write.csv(as.data.frame(PH3m_normality_tests),
          "out/Exp5.PH3m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
#print(PH3m_anova_summary)
write.csv(as.data.frame(PH3m_anova_summary[[1]]),
          "out/Exp5.PH3m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(PH3m_tukey_result$Treatment),
          "out/Exp5.PH3m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(PH3m_summary, "out/Exp5.PH3m_Summary.csv", row.names = FALSE)
#
# Guardar Shapiro
write.csv(as.data.frame(PH12m_normality_tests),
          "out/Exp5.PH12m_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
write.csv(as.data.frame(PH12m_anova_summary[[1]]),
          "out/Exp5.PH12m_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(PH12m_tukey_result$Treatment),
          "out/Exp5.PH12m_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(PH12m_summary, "out/Exp5.PH12m_Summary.csv", row.names = FALSE)
## Guardar la gráfica de mycorrhización
#ggsave("out/Exp5.ph_boxplot.pdf", ph_boxplot, width = 8, height = 6)

