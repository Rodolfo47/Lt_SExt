#Exp1 Site : Dose : pH
#Paper: L.t._SExt
#The dose makes the poison
#Rodolfo Ángeles, 26/01/25
#
#
#1. Load data
#2. Normality
#3. ANOVA
#4. Tukey
#5. Statistics
#6. Boxplot
#7. Save files

# Librerías necesarias
library(dplyr)
library(tidyr)
library(paletteer)
library(ggplot2)
library(multcomp)
library(multcompView)
library(car)

#Working directory
#setwd("/Users/re.anar/Documents/0_INECOL/Estudiantes/3_JesusContreras_MAST/PalPaper") 

# Paso 1: Crear la columna de Tratamiento combinando las tres variables categóricas
data <- read.csv("data/Exp1_pHSiteDose.csv")
data_summary <- data %>%
  mutate(Treatment = interaction(Dose, pH, Site, sep = ":"))
data_summary$X <- as.numeric(data_summary$X)

# Paso 2: Verificar normalidad de los datos por grupo
normality_tests <- data_summary %>%
  group_by(Treatment) %>%
  summarise(
    Shapiro_p = shapiro.test(X)$p.value)
print(normality_tests)

# Paso 3: Análisis de ANOVA
anova_result <- aov(X ~ Treatment, data = data_summary)
anova_summary <- summary(anova_result)
print(anova_summary)

# Paso 4: Prueba de Tukey
tukey_result <- TukeyHSD(anova_result)
tukey_cld <- multcompLetters4(anova_result, tukey_result)

# Crear tabla de letras de Tukey
cld_table <- as.data.frame.list(tukey_cld$Treatment)
cld_table$Treatment <- rownames(cld_table)

# Separar las combinaciones de tratamiento
cld_table <- cld_table %>%
  separate(Treatment, into = c("Dose", "pH", "Site"), sep = ":")
cld_table$Dose <- as.numeric(cld_table$Dose)

# Paso 5: Crear tabla principal con estadísticas descriptivas
summary_table <- data_summary %>%
  group_by(Dose, pH, Site) %>%
  summarise(
    Mean_X = mean(X, na.rm = TRUE),
    SD_X = sd(X, na.rm = TRUE),
    Median_X = median(X, na.rm = TRUE)) %>%
  left_join(cld_table, by = c("Dose", "pH", "Site")) %>%
  rename(Tukey_Group = Letters)
print(summary_table)

# Paso 6: Crear el gráfico de cajas con las letras de Tukey
# Crear el gráfico boxplot
boxplot_graph <- ggplot(data_summary,
                        aes(x = interaction(Dose, pH, Site), 
                            y = X.g.L, fill = Site)) +
  # Diferenciar Site con colores, y pH con el tipo de línea del borde
  geom_boxplot(aes(color = Site, linetype = as.factor(pH)), 
               outlier.shape = NA, lwd = 1, alpha = 0.3) + # Añade cajas y bigotes
  # Diferenciar Dose con la forma de los puntos
  geom_jitter(aes(shape = as.factor(Dose), color = Site), 
              position = position_jitter(0.2), size = 2, alpha = 0.9) + # Puntos dispersos
  # Agregar etiquetas de Tukey
  geom_text(data = summary_table,
            aes(x = interaction(Dose, pH, Site),
                y = max(data_summary$X.g.L, na.rm = TRUE) + 0.1, 
                label = Tukey_Group),
            size = 4, vjust = 0) +
  # Personalizar escalas
  scale_fill_paletteer_d("nationalparkcolors::Acadia") + # Colores para Site
  scale_color_paletteer_d("nationalparkcolors::Acadia") + # Colores para bordes
  scale_linetype_manual(values = c("solid", "twodash"), # Líneas para pH
                        name = "pH",
                        labels = c("5.5", "Natural")) +
  scale_shape_manual(values = c(1, 0, 2), # Formas para Dose
                     name = "Dose",
                     labels = levels(data_summary$Dose)) +
  # Etiquetas del gráfico
  labs(title = "Biomass by Dose, Site, and pH",
       x = "Treatment (Dose : pH : Site)",
       y = expression(paste("Biomass (g ", L^{-1}, ")"))) +
  # Estilos del tema
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(
    panel.border = element_rect(colour = "black", size = 1.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right")

# Imprimir en pantalla
print(boxplot_graph)

# Paso 7: Guardar resultados en archivos

# Guardar Shapiro
print(normality_tests)
write.csv(as.data.frame(normality_tests), "out/Exp1_Shapiro_results.csv", row.names = TRUE)
# Guardar ANOVA
print(anova_summary)
write.csv(as.data.frame(anova_summary[[1]]), "out/Exp1_Anova_results.csv", row.names = TRUE)
# Guardar prueba de Tukey
write.csv(as.data.frame(tukey_result$Treatment), "out/Exp1_Tukey_results.csv", row.names = TRUE)
# Guardar tabla resumen
write.csv(summary_table, "out/Exp1_Summary_table.csv", row.names = FALSE)
# Guardar la gráfica
ggsave("out/Exp1_Boxplot.pdf", boxplot_graph, width = 8, height = 6)

###

###