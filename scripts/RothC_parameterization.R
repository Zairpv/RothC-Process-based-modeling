# 1. LIBRARIES -----------------------------------------------------------------

library(SoilR)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(here)
library(ggpubr)
library(scales)
library(dplyr)
library(grid)
library(ggpubr)
library(readxl)
library(gridExtra)
library(DiagrammeR)

setwd("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization")
windowsFonts(A = windowsFont("Times New Roman"))

# 2. CLIMATIC DATA AND PLOT ----------------------------------------------------
## 2.1. SMN Data - 1990 - 2020 -------------------------------------------------
Temp=data.frame(Month=1:12,Temp=c(11.5,13,15,17.1,17.8,17.4,16.7,16.8,16.4,15.1,13,12.4))
Precip=data.frame(Month=1:12,Precip=c(40.6,33.2,36,47.3,52.2,173.4,158.5,206.9,313.5,211.2,100.7,29.5))
Evp=data.frame(Month=1:12,Evp=c(63.6,79.2,107.1,124.1,128.2,112.8,100.5,106.6,86.8,79.9,64,61.6))

meses=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
tmax=c(19,	21,	22.9,	25.4,	25.6,	23.9,	22.8,	22.9,	21.9,	21.4,	19.6,	19.9)
tmin=c(4,	5,	7,	8.9,	10.1,	11,	10.6,	10.7,	10.8,	8.7,	6.5,	4.9)

DATA_SMN_1990_2020=data.frame(meses,Temp[,1],Temp[,2],Precip[,2],Evp[,2],tmax,tmin)
names(DATA_SMN_1990_2020)=c("Month","Month_number","Temperature", "Precipitation","Evaporation","TMax","TMIN")

climogram_plot <- ggplot(DATA_SMN_1990_2020, aes(x = Month_number)) +
  # Línea para Precipitación y evaporación
  geom_bar(aes(y = Evaporation, fill = "Evaporation"), stat = "identity", alpha = 0.5, width = 0.6) +  # Barras para evaporación
  geom_line(aes(y = Precipitation, color = "Precipitation"), size = 0.7, linetype = 2) +  # Línea para precipitación
  # Eje secundario para Temperatura
  scale_y_continuous(
    name = "Precipitation, pan evaporation (mm)", 
    breaks = c(0,50,100,150,200,250,300),
    sec.axis = sec_axis(~ . / 10, 
                        breaks = c(0,5,10,15,20,25,30),
                        name = "Temperature (°C)")) + # Ajuste de escala para temperatura
  # Líneas de temperatura
  geom_line(aes(y = Temperature * 10, color = "Temperature"), size = 0.7, linetype = 1) +  # Temperatura media (Escala ajustada)
  geom_line(aes(y = TMax * 10, color = "Temperature Max"), size = 0.7, linetype = 1) +  # Temperatura máxima
  geom_line(aes(y = TMIN * 10, color = "Temperature Min"), size = 0.7, linetype = 1) +  # Temperatura mínima
  labs(title = "a) SMN (1990 - 2020)",
       x = "Month",
       y = "Rainfall and evaporation (mm)",
       color = "Parameters:",
       fill = ""
  ) +
  scale_color_manual(values = c(
    "Precipitation" = "#09122C", 
    "Temperature" = "#5CB338", 
    "Temperature Max" = "#B82132", 
    "Temperature Min" = "#FFAF00"),
  labels = c(
    "Precipitation" = "Precipitation", 
    "Temperature" = "Tmean", 
    "Temperature Max" = "Tmax", 
    "Temperature Min" = "Tmin") )+
  scale_fill_manual(values = c("Evaporation" = "#608BC1"),labels="Pan evaporation") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        legend.margin = margin(t = 20),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black')) +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  )+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.hjust = 0), # Stand age en una fila
    fill = guide_legend(order = 2, nrow = 1, title.hjust = 0) # Model en otra fila
  )
# Mostrar el gráfico
print(climogram_plot)


## 2.2. Field data 2017 - 2021  --------------------------------------------------
Temp_MELI=data.frame(Month=1:12,Temp=c(7.4,12.4,13.5,14.4,15.6,15,14.7,14.1,14.5,12.8,11.8,10.1))
Precip_MELI=data.frame(Month=1:12,Precip=c(83.3,35.6,44.8,39.6,29.3,100.3,76.3,124.1,89.8,294.9,21.1,13.8))
Evp_MELI=data.frame(Month=1:12,Evp=c(84.67,96.53,125.33,124.00,108.93,94.80,127.33,113.73,109.20,124.40,106.67,91.33))
tmax_MELI=c(21.8,21.2,24.9,25,26.5,20.4,23.1,18.6,19.8,21.7,21.3,20.6)
tmin_MELI=c(-0.4,6.8,4.8,3.9,9.2,10.7,9.8,10.5,10.7,3.8,1.7,-4.7)

DATA_TOWER_2017_2020=data.frame(meses,Temp_MELI[,1],Temp_MELI[,2],Precip_MELI[,2],Evp_MELI[,2],tmax_MELI,tmin_MELI)
names(DATA_TOWER_2017_2020)=c("Month","Month_number","Temperature", "Precipitation","Evaporation","TMax","TMIN")

climogram_plot2 <- ggplot(DATA_TOWER_2017_2020, aes(x = Month_number)) +
  # Línea para Precipitación y evaporación
  geom_bar(aes(y = Evaporation, fill = "Evaporation"), stat = "identity", alpha = 0.5, width = 0.6) +  # Barras para evaporación
  geom_line(aes(y = Precipitation, color = "Precipitation"), size = 0.7, linetype = 2) +  # Línea para precipitación
  # Eje secundario para Temperatura
  scale_y_continuous(
    name = "Precipitation, pan evaporation (mm)", 
    breaks = c(0,50,100,150,200,250,300),
    sec.axis = sec_axis(~ . / 10,
                        breaks = c(-5,0,5,10,15,20,25,30), 
                        name = "Temperature (°C)"),# Ajuste de escala para temperatura
  ) +
  # Líneas de temperatura
  geom_line(aes(y = Temperature * 10, color = "Temperature"), size = 0.7, linetype = 1) +  # Temperatura media (Escala ajustada)
  geom_line(aes(y = TMax * 10, color = "Temperature Max"), size = 0.7, linetype = 1) +  # Temperatura máxima
  geom_line(aes(y = TMIN * 10, color = "Temperature Min"), size = 0.7, linetype = 1) +  # Temperatura mínima
  labs(title = "b) Field data (2017 - 2021)",
       x = "Month",
       y = "Rainfall and evaporation (mm)",
       color = "Parameters:",
       fill = ""
  ) +
  scale_color_manual(values = c(
    "Precipitation" = "#09122C", 
    "Temperature" = "#5CB338", 
    "Temperature Max" = "#B82132", 
    "Temperature Min" = "#FFAF00"),
  labels = c(
    "Precipitation" = "Precipitation", 
    "Temperature" = "Tmean", 
    "Temperature Max" = "Tmax", 
    "Temperature Min" = "Tmin") )+
  scale_fill_manual(values = c("Evaporation" = "#608BC1"),labels="Pan evaporation") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black')) +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.hjust = 0), # Stand age en una fila
    fill = guide_legend(order = 2, nrow = 1, title.hjust = 0) # Model en otra fila
  )
# Mostrar el gráfico
print(climogram_plot2)


## 2.3. Join plots  ------------------------------------------------------------
jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Both_weatherdata.jpeg"),
     width = 190,height = 200,units = "mm",res = 1000)
CLIMGRAMS=ggarrange(climogram_plot,
          climogram_plot2,
          ncol = 1,nrow = 2,widths = c(2, 1),
          common.legend = TRUE,
          legend = "bottom")

annotate_figure(CLIMGRAMS, 
                bottom = textGrob("Months",vjust = -5,gp = gpar(cex = 1,fontfamily="A")),
                left = textGrob("Precipitation and pan evaporation (mm)", rot = 90, vjust = 0.5,gp = gpar(cex = 1,fontfamily="A")),
                right = textGrob("Temperature (°C)", rot = 270, vjust = 0.5,gp = gpar(cex = 1,fontfamily="A")))

dev.off()

# 3 TEMPERATURE EFFECT --------------------------------------------------------
fT=fT.RothC(Temp[,2]) #Temperature effects per month
fT_MELI=fT.RothC(Temp_MELI[,2])

months <- 1:12
fT_df=data.frame(Month=1:12,
                 fT=fT,
                 fT_MELI=fT_MELI) 
fT_df_long_both <- fT_df %>%
  pivot_longer(cols = c(fT, fT_MELI), names_to = "Factor", values_to = "Value")

temperature_effect=ggplot(fT_df_long_both, aes(x = Month, y = Value, color = reorder(Factor,Value), group = Factor)) +
  geom_line(size = 0.7) +  # Línea para mostrar la tendencia
  geom_point(size = 1.5) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  scale_y_continuous(limits = c(0.5,2.5),breaks = c(0.5,1,1.5,2,2.5))+
  labs(title = "Temperature effect",
       x = "Month",
       y = "Rate modifying factor (a)",
       color = "Data:") +
  scale_color_manual(values = c("fT"="#FF8225",
                                "fT_MELI"="#2E5077"),
                     labels=c("fT_MELI"="Field data (2017-2021)",
                              "fT"="SMN (1990-2020)"
                              ))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        legend.margin = margin(t = 20),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black'))

temperature_effect

# 4. MOISTURE EFFECT -----------------------------------------------------------
soil.thick=30 #Soil thickness (organic layer topsoil), in cm
years=seq(1/12,100,by=1/12)
## 4.1. Folio 38 (7 años) ------------------------------------------------------
clay_F38_2014= 48.5       #Percent clay

### a) SMN data
fW_F38=fW.RothC(P=(Precip[,2]), 
                E=(Evp[,2]), 
                S.Thick = soil.thick, 
                pClay = clay_F38_2014, 
                pE = 0.75, 
                bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F38

fW_F38_MELI=fW.RothC(P=(Precip_MELI[,2]), 
                E=(Evp_MELI[,2]), 
                S.Thick = soil.thick, 
                pClay = clay_F38_2014, 
                pE = 0.75, 
                bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F38_MELI

fW_df=data.frame(Month=1:12,
                 fW=fW_F38,
                 fW_MELI=fW_F38_MELI) 
fW_df_long_both <- fW_df %>%
  pivot_longer(cols = c(fW, fW_MELI), names_to = "Factor", values_to = "Value")

ggplot(fW_df_long_both, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 0.9) +  # Línea para mostrar la tendencia
  geom_point(size = 1.5) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "b) Moisture effect on decomposition",
       x = "Month",
       y = "Rate modifying factor (fW)",
       color = "Data:") +
  scale_color_manual(values = c("fW"="#FF8225",
                                "fW_MELI"="#2E5077"),
                     labels=c("fW"="SMN (1990-2020)",
                              "fW_MELI"="Field data (2017-2020)"))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black'))


## 4.2. Folio 32 (18 años) ------------------------------------------------------
clay_F32_2014= 39.76      #Percent clay

### a) SMN data
fW_F32=fW.RothC(P=(Precip[,2]), 
                E=(Evp[,2]), 
                S.Thick = soil.thick, 
                pClay = clay_F32_2014, 
                pE = 0.75, 
                bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F32

fW_F32_MELI=fW.RothC(P=(Precip_MELI[,2]), 
                     E=(Evp_MELI[,2]), 
                     S.Thick = soil.thick, 
                     pClay = clay_F32_2014, 
                     pE = 0.75, 
                     bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F32_MELI

fW_df=data.frame(Month=1:12,
                 fW=fW_F32,
                 fW_MELI=fW_F32_MELI) 
fW_df_long_both <- fW_df %>%
  pivot_longer(cols = c(fW, fW_MELI), names_to = "Factor", values_to = "Value")

ggplot(fW_df_long_both, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 0.9) +  # Línea para mostrar la tendencia
  geom_point(size = 1.5) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "b) Moisture effect on decomposition",
       x = "Month",
       y = "Rate modifying factor (fW)",
       color = "Data:") +
  scale_color_manual(values = c("fW"="#FF8225",
                                "fW_MELI"="#2E5077"),
                     labels=c("fW"="SMN (1990-2020)",
                              "fW_MELI"="Field data (2017-2020)"))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black'))


## 4.3. Folio 26 (30 años) ------------------------------------------------------
clay_F26_2014= 49.54      #Percent clay

### a) SMN data
fW_F26=fW.RothC(P=(Precip[,2]), 
                E=(Evp[,2]), 
                S.Thick = soil.thick, 
                pClay = clay_F26_2014, 
                pE = 0.75, 
                bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F26

fW_F26_MELI=fW.RothC(P=(Precip_MELI[,2]), 
                     E=(Evp_MELI[,2]), 
                     S.Thick = soil.thick, 
                     pClay = clay_F26_2014, 
                     pE = 0.75, 
                     bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F26_MELI

fW_df=data.frame(Month=1:12,
                 fW=fW_F26,
                 fW_MELI=fW_F26_MELI) 
fW_df_long_both <- fW_df %>%
  pivot_longer(cols = c(fW, fW_MELI), names_to = "Factor", values_to = "Value")

ggplot(fW_df_long_both, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 0.9) +  # Línea para mostrar la tendencia
  geom_point(size = 1.5) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "b) Moisture effect on decomposition",
       x = "Month",
       y = "Rate modifying factor (fW)",
       color = "Data:") +
  scale_color_manual(values = c("fW"="#FF8225",
                                "fW_MELI"="#2E5077"),
                     labels=c("fW"="SMN (1990-2020)",
                              "fW_MELI"="Field data (2017-2020)"))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black'))


## 4.4. Folio 23 (80 años) ------------------------------------------------------
clay_F23_2014= 52.92      #Percent clay

### a) SMN data
fW_F23=fW.RothC(P=(Precip[,2]), 
                E=(Evp[,2]), 
                S.Thick = soil.thick, 
                pClay = clay_F23_2014, 
                pE = 0.75, 
                bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F23

fW_F23_MELI=fW.RothC(P=(Precip_MELI[,2]), 
                     E=(Evp_MELI[,2]), 
                     S.Thick = soil.thick, 
                     pClay = clay_F23_2014, 
                     pE = 0.75, 
                     bare = FALSE)$b #Moisture effects per month  (Only change clay)
fW_F23_MELI

fW_df=data.frame(Month=1:12,
                 fW=fW_F23,
                 fW_MELI=fW_F23_MELI) 
fW_df_long_both <- fW_df %>%
  pivot_longer(cols = c(fW, fW_MELI), names_to = "Factor", values_to = "Value")

moisture_effect=ggplot(fW_df_long_both, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 0.7) +  # Línea para mostrar la tendencia
  geom_point(size = 1.5) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1))+
  labs(title = "Moisture effect",
       x = "Month",
       y = "Rate modifying factor (b)",
       color = "Data:") +
  scale_color_manual(values = c("fW"="#FF8225",
                                "fW_MELI"="#2E5077"),
                     labels=c("fW"="SMN (1990-2020)",
                              "fW_MELI"="Field data (2017-2021)"))+
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.text.x = element_text( hjust = 1), # Para mejorar la visibilidad de los meses
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        axis.title.x = element_blank(),  # Quitar el título del eje X para mayor claridad
        panel.background = element_rect(fill='white', colour='black'))
moisture_effect

# 5. BOTH EFFECTS -------------------------------------------------------------
jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Both_effects.jpeg"),
     width = 190,height = 140,units = "mm",res = 1000)
Climeffects=ggarrange(temperature_effect,
          moisture_effect,
          ncol = 1,nrow = 2,widths = c(2, 1),
          common.legend = TRUE,
          legend = "bottom")
annotate_figure(Climeffects, 
                bottom = textGrob("Month",vjust = -4.5,gp = gpar(cex = 1,fontfamily="A")))

dev.off()


# 6. SENSITIVITY TO CARBON INPUTS (RothC models) -------------------------------
## 6.1. FOLIO 38 (7 AÑOS) ------------------------------------------------------
xi.frame_F38=data.frame(years,rep(fT*fW_F38,length.out=length(years)))

### a) Initial SOC (Mg/ha/yr) -----------------------------------------------------
SOC_F38_2014= 129.6895    #Soil organic carbon in Mg/ha 

### b) Annual dry biomass (Mg/ha/yr) ----------------------------------------------
Cinputs_F38_2014_LITTERFAL=2.338273333 
Cinputs_F38_2014_LITTERFALL_ROOT=11.68727333
Cinputs_F38_2014_FORESTFLOOR=14.24990005

### c) IOM  #IOM using Falloon method ------------------------------------------
FallIOM_F38=0.049*SOC_F38_2014^(1.139) 
FallIOM_F38

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F38=0.0236*SOC_F38_2014^(1.223)


### d) Pedotransfer function - Initial values of fractions ---------------------
#Weihermueller et al. (2013) proposed a set of functions (pedotransfer functions) to obtain 
#the initial values of RothC pool sizes using data on total carbon clay contents. 
#Their functions are given by:

(RPMptf_F38=(0.1847*SOC_F38_2014 + 0.1555)*((clay_F38_2014 + 1.275)^(-0.1158)))
(HUMptf_F38=(0.7148*SOC_F38_2014 + 0.5069)*((clay_F38_2014 + 0.3421)^(0.0184)))
(BIOptf_F38=(0.014*SOC_F38_2014 + 0.0075)*((clay_F38_2014 + 8.8473)^(0.0567)))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
(DPMptf_F38=SOC_F38_2014-(FallIOM_F38+RPMptf_F38+HUMptf_F38+BIOptf_F38))
c(DPMptf_F38, RPMptf_F38, BIOptf_F38, HUMptf_F38, FallIOM_F38)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F38_forest=SOC_F38_2014-(IOM_FOREST_F38+RPMptf_F38+HUMptf_F38+BIOptf_F38)
c(DPMptf_F38_forest, RPMptf_F38, BIOptf_F38, HUMptf_F38, IOM_FOREST_F38)


### e) RothC models (LITTERFALL) ------------------------------------------------------------
#### e.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F38=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(0, 0, 0, 0, FallIOM_F38),
                     In=Cinputs_F38_2014_LITTERFAL,
                     clay=clay_F38_2014,
                     DR=0.25,
                     xi=xi.frame_F38) #Loads the model
Ct1_F38=getC(Model1_F38) #Calculates stocks for each pool per month
TotalC1_F38=rowSums(Ct1_F38)#Calculates total (sum of all pools)  
Rt1_F38=getReleaseFlux(Model1_F38)#CO2 released for all pools 

matplot(years, Ct1_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F38_df <- data.frame(years,as.data.frame(Ct1_F38),TotalC1_F38)
names(Ct1_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F38=tail(Ct1_F38_df,1)
poolSize_F38$Stand_age="07 años"
poolSize_F38$Harvest_year=2005
poolSize_F38 #Reported data

Ct1_long_F38 <- Ct1_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F38$PSP=38
Ct1_long_F38$Stand_age=c("07 years")
Ct1_long_F38$Harvest_year=2005


#### e.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct2_F38=getC(Model2_F38) #Calculates stocks for each pool per month
TotalC2_F38=rowSums(Ct2_F38)#Calculates total (sum of all pools)  
Rt2_F38=getReleaseFlux(Model2_F38)#CO2 released for all pools 

matplot(years, Ct2_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F38_df <- data.frame(years,as.data.frame(Ct2_F38),TotalC2_F38)
names(Ct2_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F38=tail(Ct2_F38_df,1)
poolSize2_F38$Stand_age="07 años"
poolSize2_F38$Harvest_year=2005
poolSize2_F38 #Reported data

Ct2_long_F38 <- Ct2_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38$PSP=38
Ct2_long_F38$Stand_age=c("07 years")
Ct2_long_F38$Harvest_year=2005



#### e.3) RothC model 3 (IOM_G & PF) - REPORTED F38-----------------------------------------
Model3_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFAL*0.47,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct3_F38=getC(Model3_F38) #Calculates stocks for each pool per month
TotalC3_F38=rowSums(Ct3_F38)#Calculates total (sum of all pools)  
Rt3_F38=getReleaseFlux(Model3_F38)#CO2 released for all pools 
?getReleaseFlux

TotalRt3_F38=rowSums(Rt3_F38)
mean(TotalRt3_F38, na.rm = TRUE)

matplot(years, Ct3_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F38_df <- data.frame(years,as.data.frame(Ct3_F38),TotalC3_F38,TotalRt3_F38)
names(Ct3_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC","CO2")
poolSize3_F38=tail(Ct3_F38_df,1)
poolSize3_F38$Stand_age="07 years"
poolSize3_F38$Harvest_year=2005
poolSize3_F38 #Reported data
sum(Ct3_F38_df$"CO2", na.rm = TRUE)


Ct3_long_F38 <- Ct3_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F38$PSP=38
Ct3_long_F38$Stand_age=c("07 years")
Ct3_long_F38$Harvest_year=2005
View(Ct3_long_F38)


#### e.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct4_F38=getC(Model4_F38) #Calculates stocks for each pool per month
TotalC4_F38=rowSums(Ct4_F38)#Calculates total (sum of all pools)  
Rt4_F38=getReleaseFlux(Model4_F38)#CO2 released for all pools 

matplot(years, Ct4_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F38_df <- data.frame(years,as.data.frame(Ct4_F38),TotalC4_F38)
names(Ct4_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F38=tail(Ct4_F38_df,1)
poolSize4_F38$Stand_age="07 años"
poolSize4_F38$Harvest_year=2005
poolSize4_F38 #Reported data

Ct4_long_F38 <- Ct4_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F38$PSP=38
Ct4_long_F38$Stand_age=c("07 years")
Ct4_long_F38$Harvest_year=2005



### f) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### f.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct5_F38=getC(Model5_F38) #Calculates stocks for each pool per month
TotalC5_F38=rowSums(Ct5_F38)#Calculates total (sum of all pools)  
Rt5_F38=getReleaseFlux(Model5_F38)#CO2 released for all pools 

matplot(years, Ct5_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F38_df <- data.frame(years,as.data.frame(Ct5_F38),TotalC5_F38)
names(Ct5_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F38=tail(Ct5_F38_df,1)
poolSize5_F38$Stand_age="07 años"
poolSize5_F38$Harvest_year=2005
poolSize5_F38 #Reported data

Ct5_long_F38 <- Ct5_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F38$PSP=38
Ct5_long_F38$Stand_age=c("07 years")
Ct5_long_F38$Harvest_year=2005


#### f.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct6_F38=getC(Model6_F38) #Calculates stocks for each pool per month
TotalC6_F38=rowSums(Ct6_F38)#Calculates total (sum of all pools)  
Rt6_F38=getReleaseFlux(Model6_F38)#CO2 released for all pools 

matplot(years, Ct6_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F38_df <- data.frame(years,as.data.frame(Ct6_F38),TotalC6_F38)
names(Ct6_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F38=tail(Ct6_F38_df,1)
poolSize6_F38$Stand_age="07 años"
poolSize6_F38$Harvest_year=2005
poolSize6_F38 #Reported data

Ct6_long_F38 <- Ct6_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F38$PSP=38
Ct6_long_F38$Stand_age=c("07 years")
Ct6_long_F38$Harvest_year=2005



#### f.3) RothC model 7 (IOM_G & PF) - REPORTED F38 -----------------------------------------
Model7_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct7_F38=getC(Model7_F38) #Calculates stocks for each pool per month
TotalC7_F38=rowSums(Ct7_F38)#Calculates total (sum of all pools)  
Rt7_F38=getReleaseFlux(Model7_F38)#CO2 released for all pools 

matplot(years, Ct7_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F38_df <- data.frame(years,as.data.frame(Ct7_F38),TotalC7_F38)
names(Ct7_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F38=tail(Ct7_F38_df,1)
poolSize7_F38$Stand_age="07 years"
poolSize7_F38$Harvest_year=2005
poolSize7_F38 #Reported data

Ct7_long_F38 <- Ct7_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F38$PSP=38
Ct7_long_F38$Stand_age=c("07 years")
Ct7_long_F38$Harvest_year=2005


#### f.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct8_F38=getC(Model8_F38) #Calculates stocks for each pool per month
TotalC8_F38=rowSums(Ct8_F38)#Calculates total (sum of all pools)  
Rt8_F38=getReleaseFlux(Model8_F38)#CO2 released for all pools 

matplot(years, Ct8_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F38_df <- data.frame(years,as.data.frame(Ct8_F38),TotalC8_F38)
names(Ct8_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F38=tail(Ct8_F38_df,1)
poolSize8_F38$Stand_age="07 años"
poolSize8_F38$Harvest_year=2005
poolSize8_F38 #Reported data

Ct8_long_F38 <- Ct8_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F38$PSP=38
Ct8_long_F38$Stand_age=c("07 years")
Ct8_long_F38$Harvest_year=2005



### g) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### g.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014_FORESTFLOOR,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct9_F38=getC(Model9_F38) #Calculates stocks for each pool per month
TotalC9_F38=rowSums(Ct9_F38)#Calculates total (sum of all pools)  
Rt9_F38=getReleaseFlux(Model9_F38)#CO2 released for all pools 

matplot(years, Ct9_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F38_df <- data.frame(years,as.data.frame(Ct9_F38),TotalC9_F38)
names(Ct9_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F38=tail(Ct9_F38_df,1)
poolSize9_F38$Stand_age="07 años"
poolSize9_F38$Harvest_year=2005
poolSize9_F38 #Reported data

Ct9_long_F38 <- Ct9_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F38$PSP=38
Ct9_long_F38$Stand_age=c("07 years")
Ct9_long_F38$Harvest_year=2005


#### g.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014_FORESTFLOOR,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct10_F38=getC(Model10_F38) #Calculates stocks for each pool per month
TotalC10_F38=rowSums(Ct10_F38)#Calculates total (sum of all pools)  
Rt10_F38=getReleaseFlux(Model10_F38)#CO2 released for all pools 

matplot(years, Ct10_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F38_df <- data.frame(years,as.data.frame(Ct10_F38),TotalC10_F38)
names(Ct10_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F38=tail(Ct10_F38_df,1)
poolSize10_F38$Stand_age="07 años"
poolSize10_F38$Harvest_year=2005
poolSize10_F38 #Reported data

Ct10_long_F38 <- Ct10_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F38$PSP=38
Ct10_long_F38$Stand_age=c("07 years")
Ct10_long_F38$Harvest_year=2005



#### g.3) RothC model 7 (IOM_G & PF) - REPORTED F38 ----------------------------------
Model11_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_FORESTFLOOR,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct11_F38=getC(Model11_F38) #Calculates stocks for each pool per month
TotalC11_F38=rowSums(Ct11_F38)#Calculates total (sum of all pools)  
Rt11_F38=getReleaseFlux(Model11_F38)#CO2 released for all pools 

matplot(years, Ct11_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F38_df <- data.frame(years,as.data.frame(Ct11_F38),TotalC11_F38)
names(Ct11_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F38=tail(Ct11_F38_df,1)
poolSize11_F38$Stand_age="07 years"
poolSize11_F38$Harvest_year=2005
poolSize11_F38 #Reported data

Ct11_long_F38 <- Ct11_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F38$PSP=38
Ct11_long_F38$Stand_age=c("07 years")
Ct11_long_F38$Harvest_year=2005

#### g.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                      In=Cinputs_F38_2014_FORESTFLOOR,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model
Ct12_F38=getC(Model12_F38) #Calculates stocks for each pool per month
TotalC12_F38=rowSums(Ct12_F38)#Calculates total (sum of all pools)  
Rt12_F38=getReleaseFlux(Model12_F38)#CO2 released for all pools 

matplot(years, Ct12_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F38_df <- data.frame(years,as.data.frame(Ct8_F38),TotalC8_F38)
names(Ct12_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F38=tail(Ct12_F38_df,1)
poolSize12_F38$Stand_age="07 años"
poolSize12_F38$Harvest_year=2005
poolSize12_F38 #Reported data

Ct12_long_F38 <- Ct12_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F38$PSP=38
Ct12_long_F38$Stand_age=c("07 years")
Ct12_long_F38$Harvest_year=2005



## 6.2. FOLIO 32 (18 AÑOS) ------------------------------------------------------
xi.frame_F32=data.frame(years,rep(fT*fW_F32,length.out=length(years)))

### a) Initial SOC (Mg/ha/yr) -----------------------------------------------------
SOC_F32_2014= 151.69147    #Soil organic carbon in Mg/ha 

### b) Annual dry biomass (Mg/ha/yr) ----------------------------------------------
Cinputs_F32_2014_LITTERFAL=4.500733333
Cinputs_F32_2014_LITTERFALL_ROOT=18.26673333
Cinputs_F32_2014_FORESTFLOOR=18.0249915

### c) IOM  #IOM using Falloon method ------------------------------------------
FallIOM_F32=0.049*SOC_F32_2014^(1.139) 
FallIOM_F32

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F32=0.0236*SOC_F32_2014^(1.223)
IOM_FOREST_F32

### d) Pedotransfer function - Initial values of fractions ---------------------

(RPMptf_F32=(0.1847*SOC_F32_2014 + 0.1555)*((clay_F32_2014 + 1.275)^(-0.1158)))
(HUMptf_F32=(0.7148*SOC_F32_2014 + 0.5069)*((clay_F32_2014 + 0.3421)^(0.0184)))
(BIOptf_F32=(0.014*SOC_F32_2014 + 0.0075)*((clay_F32_2014 + 8.8473)^(0.0567)))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
(DPMptf_F32=SOC_F32_2014-(FallIOM_F32+RPMptf_F32+HUMptf_F32+BIOptf_F32))
c(DPMptf_F32, RPMptf_F32, BIOptf_F32, HUMptf_F32, FallIOM_F32)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F32_forest=SOC_F32_2014-(IOM_FOREST_F32+RPMptf_F32+HUMptf_F32+BIOptf_F32)
c(DPMptf_F32_forest, RPMptf_F32, BIOptf_F32, HUMptf_F32, IOM_FOREST_F32)


### e) RothC models (LITTERFALL) ------------------------------------------------------------
#### e.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct1_F32=getC(Model1_F32) #Calculates stocks for each pool per month
TotalC1_F32=rowSums(Ct1_F32)#Calculates total (sum of all pools)  
Rt1_F32=getReleaseFlux(Model1_F32)#CO2 released for all pools 

matplot(years, Ct1_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F32_df <- data.frame(years,as.data.frame(Ct1_F32),TotalC1_F32)
names(Ct1_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F32=tail(Ct1_F32_df,1)
poolSize_F32$Stand_age="18 años"
poolSize_F32$Harvest_year=1995
poolSize_F32 #Reported data

Ct1_long_F32 <- Ct1_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F32$PSP=32
Ct1_long_F32$Stand_age=c("18 years")
Ct1_long_F32$Harvest_year=1995


#### e.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct2_F32=getC(Model2_F32) #Calculates stocks for each pool per month
TotalC2_F32=rowSums(Ct2_F32)#Calculates total (sum of all pools)  
Rt2_F32=getReleaseFlux(Model2_F32)#CO2 released for all pools 

matplot(years, Ct2_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F32_df <- data.frame(years,as.data.frame(Ct2_F32),TotalC2_F32)
names(Ct2_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F32=tail(Ct2_F32_df,1)
poolSize2_F32$Stand_age="18 years"
poolSize2_F32$Harvest_year=1995
poolSize2_F32 #Reported data

Ct2_long_F32 <- Ct2_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32$PSP=32
Ct2_long_F32$Stand_age=c("18 years")
Ct2_long_F32$Harvest_year=1995



#### e.3) RothC model 3 (IOM_G & PF) - REPORTED F32 -----------------------------------------
Model3_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct3_F32=getC(Model3_F32) #Calculates stocks for each pool per month
TotalC3_F32=rowSums(Ct3_F32)#Calculates total (sum of all pools)  
Rt3_F32=getReleaseFlux(Model3_F32)#CO2 released for all pools 

matplot(years, Ct3_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F32_df <- data.frame(years,as.data.frame(Ct3_F32),TotalC3_F32)
names(Ct3_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F32=tail(Ct3_F32_df,1)
poolSize3_F32$Stand_age="18 years"
poolSize3_F32$Harvest_year=1995
poolSize3_F32 #Reported data- reported 
Ct3_long_F32 <- Ct3_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F32$PSP=32
Ct3_long_F32$Stand_age=c("18 years")
Ct3_long_F32$Harvest_year=1995



#### e.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct4_F32=getC(Model4_F32) #Calculates stocks for each pool per month
TotalC4_F32=rowSums(Ct4_F32)#Calculates total (sum of all pools)  
Rt4_F32=getReleaseFlux(Model4_F32)#CO2 released for all pools 

matplot(years, Ct4_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F32_df <- data.frame(years,as.data.frame(Ct4_F32),TotalC4_F32)
names(Ct4_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F32=tail(Ct4_F32_df,1)
poolSize4_F32$Stand_age="18 years"
poolSize4_F32$Harvest_year=1995
poolSize4_F32 #Reported data

Ct4_long_F32 <- Ct4_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F32$PSP=32
Ct4_long_F32$Stand_age=c("18 years")
Ct4_long_F32$Harvest_year=1995



### f) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### f.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct5_F32=getC(Model5_F32) #Calculates stocks for each pool per month
TotalC5_F32=rowSums(Ct5_F32)#Calculates total (sum of all pools)  
Rt5_F32=getReleaseFlux(Model5_F32)#CO2 released for all pools 

matplot(years, Ct5_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F32_df <- data.frame(years,as.data.frame(Ct5_F32),TotalC5_F32)
names(Ct5_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F32=tail(Ct5_F32_df,1)
poolSize5_F32$Stand_age="18 years"
poolSize5_F32$Harvest_year=1995
poolSize5_F32 #Reported data

Ct5_long_F32 <- Ct5_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F32$PSP=32
Ct5_long_F32$Stand_age=c("18 years")
Ct5_long_F32$Harvest_year=1995


#### f.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct6_F32=getC(Model6_F32) #Calculates stocks for each pool per month
TotalC6_F32=rowSums(Ct6_F32)#Calculates total (sum of all pools)  
Rt6_F32=getReleaseFlux(Model6_F32)#CO2 released for all pools 

matplot(years, Ct6_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F32_df <- data.frame(years,as.data.frame(Ct6_F32),TotalC6_F32)
names(Ct6_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F32=tail(Ct6_F32_df,1)
poolSize6_F32$Stand_age="18 years"
poolSize6_F32$Harvest_year=1995
poolSize6_F32 #Reported data

Ct6_long_F32 <- Ct6_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F32$PSP=32
Ct6_long_F32$Stand_age=c("18 years")
Ct6_long_F32$Harvest_year=1995



#### f.3) RothC model 7 (IOM_G & PF) - REPORTED F32-----------------------------------------
Model7_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct7_F32=getC(Model7_F32) #Calculates stocks for each pool per month
TotalC7_F32=rowSums(Ct7_F32)#Calculates total (sum of all pools)  
Rt7_F32=getReleaseFlux(Model7_F32)#CO2 released for all pools 

matplot(years, Ct7_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F32_df <- data.frame(years,as.data.frame(Ct7_F32),TotalC7_F32)
names(Ct7_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F32=tail(Ct7_F32_df,1)
poolSize7_F32$Stand_age="18 years"
poolSize7_F32$Harvest_year=1995
poolSize7_F32 #Reported data

Ct7_long_F32 <- Ct7_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F32$PSP=32
Ct7_long_F32$Stand_age=c("18 years")
Ct7_long_F32$Harvest_year=1995



#### f.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct8_F32=getC(Model8_F32) #Calculates stocks for each pool per month
TotalC8_F32=rowSums(Ct8_F32)#Calculates total (sum of all pools)  
Rt8_F32=getReleaseFlux(Model8_F32)#CO2 released for all pools 

matplot(years, Ct8_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F32_df <- data.frame(years,as.data.frame(Ct8_F32),TotalC8_F32)
names(Ct8_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F32=tail(Ct8_F32_df,1)
poolSize8_F32$Stand_age="18 years"
poolSize8_F32$Harvest_year=1995
poolSize8_F32 #Reported data

Ct8_long_F32 <- Ct8_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F32$PSP=32
Ct8_long_F32$Stand_age=c("18 years")
Ct8_long_F32$Harvest_year=1995



### g) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### g.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_FORESTFLOOR,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model
Ct9_F32=getC(Model9_F32) #Calculates stocks for each pool per month
TotalC9_F32=rowSums(Ct9_F32)#Calculates total (sum of all pools)  
Rt9_F32=getReleaseFlux(Model9_F32)#CO2 released for all pools 

matplot(years, Ct9_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F32_df <- data.frame(years,as.data.frame(Ct9_F32),TotalC9_F32)
names(Ct9_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F32=tail(Ct9_F32_df,1)
poolSize9_F32$Stand_age="18 years"
poolSize9_F32$Harvest_year=1995
poolSize9_F32 #Reported data

Ct9_long_F32 <- Ct9_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F32$PSP=32
Ct9_long_F32$Stand_age=c("18 years")
Ct9_long_F32$Harvest_year=1995


#### g.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F32=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32) #Loads the model
Ct10_F32=getC(Model10_F32) #Calculates stocks for each pool per month
TotalC10_F32=rowSums(Ct10_F32)#Calculates total (sum of all pools)  
Rt10_F32=getReleaseFlux(Model10_F32)#CO2 released for all pools 

matplot(years, Ct10_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F32_df <- data.frame(years,as.data.frame(Ct10_F32),TotalC10_F32)
names(Ct10_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F32=tail(Ct10_F32_df,1)
poolSize10_F32$Stand_age="18 years"
poolSize10_F32$Harvest_year=1995
poolSize10_F32 #Reported data

Ct10_long_F32 <- Ct10_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F32$PSP=32
Ct10_long_F32$Stand_age=c("18 years")
Ct10_long_F32$Harvest_year=1995



#### g.3) RothC model 7 (IOM_G & PF) - REPORTED F32-----------------------------------------
Model11_F32=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32) #Loads the model
Ct11_F32=getC(Model11_F32) #Calculates stocks for each pool per month
TotalC11_F32=rowSums(Ct11_F32)#Calculates total (sum of all pools)  
Rt11_F32=getReleaseFlux(Model11_F32)#CO2 released for all pools 

matplot(years, Ct11_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F32_df <- data.frame(years,as.data.frame(Ct11_F32),TotalC11_F32)
names(Ct11_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F32=tail(Ct11_F32_df,1)
poolSize11_F32$Stand_age="18 years"
poolSize11_F32$Harvest_year=1995
poolSize11_F32 #Reported data

Ct11_long_F32 <- Ct11_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F32$PSP=32
Ct11_long_F32$Stand_age=c("18 years")
Ct11_long_F32$Harvest_year=1995



#### g.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F32=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32) #Loads the model
Ct12_F32=getC(Model12_F32) #Calculates stocks for each pool per month
TotalC12_F32=rowSums(Ct12_F32)#Calculates total (sum of all pools)  
Rt12_F32=getReleaseFlux(Model12_F32)#CO2 released for all pools 

matplot(years, Ct12_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F32_df <- data.frame(years,as.data.frame(Ct8_F32),TotalC8_F32)
names(Ct12_F32_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F32=tail(Ct12_F32_df,1)
poolSize12_F32$Stand_age="18 years"
poolSize12_F32$Harvest_year=1995
poolSize12_F32 #Reported data

Ct12_long_F32 <- Ct12_F32_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F32$PSP=32
Ct12_long_F32$Stand_age=c("18 years")
Ct12_long_F32$Harvest_year=1995



## 6.3. FOLIO 26 (30 years) ------------------------------------------------------
xi.frame_F26=data.frame(years,rep(fT*fW_F26,length.out=length(years)))

### a) Initial SOC (Mg/ha/yr) -----------------------------------------------------
SOC_F26_2014= 187.6272    #Soil organic carbon in Mg/ha 

### b) Annual dry biomass (Mg/ha/yr) ----------------------------------------------
Cinputs_F26_2014_LITTERFAL=5.812783333
Cinputs_F26_2014_LITTERFALL_ROOT=16.00278333
Cinputs_F26_2014_FORESTFLOOR=14.95230872


### c) IOM  #IOM using Falloon method ------------------------------------------
FallIOM_F26=0.049*SOC_F26_2014^(1.139) 
FallIOM_F26

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F26=0.0236*SOC_F26_2014^(1.223)
IOM_FOREST_F26

### d) Pedotransfer function - Initial values of fractions ---------------------

(RPMptf_F26=(0.1847*SOC_F26_2014 + 0.1555)*((clay_F26_2014 + 1.275)^(-0.1158)))
(HUMptf_F26=(0.7148*SOC_F26_2014 + 0.5069)*((clay_F26_2014 + 0.3421)^(0.0184)))
(BIOptf_F26=(0.014*SOC_F26_2014 + 0.0075)*((clay_F26_2014 + 8.8473)^(0.0567)))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
(DPMptf_F26=SOC_F26_2014-(FallIOM_F26+RPMptf_F26+HUMptf_F26+BIOptf_F26))
c(DPMptf_F26, RPMptf_F26, BIOptf_F26, HUMptf_F26, FallIOM_F26)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F26_forest=SOC_F26_2014-(IOM_FOREST_F26+RPMptf_F26+HUMptf_F26+BIOptf_F26)
c(DPMptf_F26_forest, RPMptf_F26, BIOptf_F26, HUMptf_F26, IOM_FOREST_F26)


### e) RothC models (LITTERFALL) ------------------------------------------------------------
#### e.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct1_F26=getC(Model1_F26) #Calculates stocks for each pool per month
TotalC1_F26=rowSums(Ct1_F26)#Calculates total (sum of all pools)  
Rt1_F26=getReleaseFlux(Model1_F26)#CO2 released for all pools 

matplot(years, Ct1_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F26_df <- data.frame(years,as.data.frame(Ct1_F26),TotalC1_F26)
names(Ct1_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F26=tail(Ct1_F26_df,1)
poolSize_F26$Stand_age="30 years"
poolSize_F26$Harvest_year=1983
poolSize_F26 #Reported data

Ct1_long_F26 <- Ct1_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F26$PSP=26
Ct1_long_F26$Stand_age=c("30 years")
Ct1_long_F26$Harvest_year=1983


#### e.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct2_F26=getC(Model2_F26) #Calculates stocks for each pool per month
TotalC2_F26=rowSums(Ct2_F26)#Calculates total (sum of all pools)  
Rt2_F26=getReleaseFlux(Model2_F26)#CO2 released for all pools 

matplot(years, Ct2_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F26_df <- data.frame(years,as.data.frame(Ct2_F26),TotalC2_F26)
names(Ct2_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F26=tail(Ct2_F26_df,1)
poolSize2_F26$Stand_age="30 years"
poolSize2_F26$Harvest_year=1983
poolSize2_F26 #Reported data

Ct2_long_F26 <- Ct2_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26$PSP=26
Ct2_long_F26$Stand_age=c("30 years")
Ct2_long_F26$Harvest_year=1983



#### e.3) RothC model 3 (IOM_G & PF) - REPORTED F26-----------------------------------------
Model3_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct3_F26=getC(Model3_F26) #Calculates stocks for each pool per month
TotalC3_F26=rowSums(Ct3_F26)#Calculates total (sum of all pools)  
Rt3_F26=getReleaseFlux(Model3_F26)#CO2 released for all pools 

matplot(years, Ct3_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F26_df <- data.frame(years,as.data.frame(Ct3_F26),TotalC3_F26)
names(Ct3_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F26=tail(Ct3_F26_df,1)
poolSize3_F26$Stand_age="30 years"
poolSize3_F26$Harvest_year=1983
poolSize3_F26 #Reported data

Ct3_long_F26 <- Ct3_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F26$PSP=26
Ct3_long_F26$Stand_age=c("30 years")
Ct3_long_F26$Harvest_year=1983

#### e.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct4_F26=getC(Model4_F26) #Calculates stocks for each pool per month
TotalC4_F26=rowSums(Ct4_F26)#Calculates total (sum of all pools)  
Rt4_F26=getReleaseFlux(Model4_F26)#CO2 released for all pools 

matplot(years, Ct4_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F26_df <- data.frame(years,as.data.frame(Ct4_F26),TotalC4_F26)
names(Ct4_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F26=tail(Ct4_F26_df,1)
poolSize4_F26$Stand_age="30 years"
poolSize4_F26$Harvest_year=1983
poolSize4_F26 #Reported data

Ct4_long_F26 <- Ct4_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F26$PSP=26
Ct4_long_F26$Stand_age=c("30 years")
Ct4_long_F26$Harvest_year=1983



### f) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### f.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct5_F26=getC(Model5_F26) #Calculates stocks for each pool per month
TotalC5_F26=rowSums(Ct5_F26)#Calculates total (sum of all pools)  
Rt5_F26=getReleaseFlux(Model5_F26)#CO2 released for all pools 

matplot(years, Ct5_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F26_df <- data.frame(years,as.data.frame(Ct5_F26),TotalC5_F26)
names(Ct5_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F26=tail(Ct5_F26_df,1)
poolSize5_F26$Stand_age="30 years"
poolSize5_F26$Harvest_year=1983
poolSize5_F26 #Reported data

Ct5_long_F26 <- Ct5_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F26$PSP=26
Ct5_long_F26$Stand_age=c("30 years")
Ct5_long_F26$Harvest_year=1983


#### f.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct6_F26=getC(Model6_F26) #Calculates stocks for each pool per month
TotalC6_F26=rowSums(Ct6_F26)#Calculates total (sum of all pools)  
Rt6_F26=getReleaseFlux(Model6_F26)#CO2 released for all pools 

matplot(years, Ct6_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F26_df <- data.frame(years,as.data.frame(Ct6_F26),TotalC6_F26)
names(Ct6_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F26=tail(Ct6_F26_df,1)
poolSize6_F26$Stand_age="30 years"
poolSize6_F26$Harvest_year=1983
poolSize6_F26 #Reported data

Ct6_long_F26 <- Ct6_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F26$PSP=26
Ct6_long_F26$Stand_age=c("30 years")
Ct6_long_F26$Harvest_year=1983



#### f.3) RothC model 7 (IOM_G & PF) - REPORTED F26-----------------------------------------
Model7_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct7_F26=getC(Model7_F26) #Calculates stocks for each pool per month
TotalC7_F26=rowSums(Ct7_F26)#Calculates total (sum of all pools)  
Rt7_F26=getReleaseFlux(Model7_F26)#CO2 released for all pools 

matplot(years, Ct7_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F26_df <- data.frame(years,as.data.frame(Ct7_F26),TotalC7_F26)
names(Ct7_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F26=tail(Ct7_F26_df,1)
poolSize7_F26$Stand_age="30 years"
poolSize7_F26$Harvest_year=1983
poolSize7_F26 #Reported data

Ct7_long_F26 <- Ct7_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F26$PSP=26
Ct7_long_F26$Stand_age=c("30 years")
Ct7_long_F26$Harvest_year=1983



#### f.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct8_F26=getC(Model8_F26) #Calculates stocks for each pool per month
TotalC8_F26=rowSums(Ct8_F26)#Calculates total (sum of all pools)  
Rt8_F26=getReleaseFlux(Model8_F26)#CO2 released for all pools 

matplot(years, Ct8_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F26_df <- data.frame(years,as.data.frame(Ct8_F26),TotalC8_F26)
names(Ct8_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F26=tail(Ct8_F26_df,1)
poolSize8_F26$Stand_age="30 years"
poolSize8_F26$Harvest_year=1983
poolSize8_F26 #Reported data

Ct8_long_F26 <- Ct8_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F26$PSP=26
Ct8_long_F26$Stand_age=c("30 years")
Ct8_long_F26$Harvest_year=1983



### g) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### g.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_FORESTFLOOR,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct9_F26=getC(Model9_F26) #Calculates stocks for each pool per month
TotalC9_F26=rowSums(Ct9_F26)#Calculates total (sum of all pools)  
Rt9_F26=getReleaseFlux(Model9_F26)#CO2 released for all pools 

matplot(years, Ct9_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F26_df <- data.frame(years,as.data.frame(Ct9_F26),TotalC9_F26)
names(Ct9_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F26=tail(Ct9_F26_df,1)
poolSize9_F26$Stand_age="30 years"
poolSize9_F26$Harvest_year=1983
poolSize9_F26 #Reported data

Ct9_long_F26 <- Ct9_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F26$PSP=26
Ct9_long_F26$Stand_age=c("30 years")
Ct9_long_F26$Harvest_year=1983


#### g.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F26=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26) #Loads the model
Ct10_F26=getC(Model10_F26) #Calculates stocks for each pool per month
TotalC10_F26=rowSums(Ct10_F26)#Calculates total (sum of all pools)  
Rt10_F26=getReleaseFlux(Model10_F26)#CO2 released for all pools 

matplot(years, Ct10_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F26_df <- data.frame(years,as.data.frame(Ct10_F26),TotalC10_F26)
names(Ct10_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F26=tail(Ct10_F26_df,1)
poolSize10_F26$Stand_age="30 years"
poolSize10_F26$Harvest_year=1983
poolSize10_F26 #Reported data

Ct10_long_F26 <- Ct10_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F26$PSP=26
Ct10_long_F26$Stand_age=c("30 years")
Ct10_long_F26$Harvest_year=1983



#### g.3) RothC model 7 (IOM_G & PF) - REPORTED F26-----------------------------------------
Model11_F26=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26) #Loads the model
Ct11_F26=getC(Model11_F26) #Calculates stocks for each pool per month
TotalC11_F26=rowSums(Ct11_F26)#Calculates total (sum of all pools)  
Rt11_F26=getReleaseFlux(Model11_F26)#CO2 released for all pools 

matplot(years, Ct11_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F26_df <- data.frame(years,as.data.frame(Ct11_F26),TotalC11_F26)
names(Ct11_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F26=tail(Ct11_F26_df,1)
poolSize11_F26$Stand_age="30 years"
poolSize11_F26$Harvest_year=1983
poolSize11_F26 #Reported data

Ct11_long_F26 <- Ct11_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F26$PSP=26
Ct11_long_F26$Stand_age=c("30 years")
Ct11_long_F26$Harvest_year=1983



#### g.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F26=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26) #Loads the model
Ct12_F26=getC(Model12_F26) #Calculates stocks for each pool per month
TotalC12_F26=rowSums(Ct12_F26)#Calculates total (sum of all pools)  
Rt12_F26=getReleaseFlux(Model12_F26)#CO2 released for all pools 

matplot(years, Ct12_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F26_df <- data.frame(years,as.data.frame(Ct8_F26),TotalC8_F26)
names(Ct12_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F26=tail(Ct12_F26_df,1)
poolSize12_F26$Stand_age="30 years"
poolSize12_F26$Harvest_year=1983
poolSize12_F26 #Reported data

Ct12_long_F26 <- Ct12_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F26$PSP=26
Ct12_long_F26$Stand_age=c("30 years")
Ct12_long_F26$Harvest_year=1983



## 6.4. FOLIO 23 (80 years) ------------------------------------------------------
xi.frame_F23=data.frame(years,rep(fT*fW_F23,length.out=length(years)))

### a) Initial SOC (Mg/ha/yr) -----------------------------------------------------
SOC_F23_2014=98.3563     #Soil organic carbon in Mg/ha 

### b) Annual dry biomass (Mg/ha/yr) ----------------------------------------------
Cinputs_F23_2014_LITTERFAL=6.798916667
Cinputs_F23_2014_LITTERFALL_ROOT=20.61791667
Cinputs_F23_2014_FORESTFLOOR=16.22767053


### c) IOM  #IOM using Falloon method ------------------------------------------
FallIOM_F23=0.049*SOC_F23_2014^(1.139) 
FallIOM_F23

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F23=0.0236*SOC_F23_2014^(1.223)
IOM_FOREST_F23

### d) Pedotransfer function - Initial values of fractions ---------------------

(RPMptf_F23=(0.1847*SOC_F23_2014 + 0.1555)*((clay_F23_2014 + 1.275)^(-0.1158)))
(HUMptf_F23=(0.7148*SOC_F23_2014 + 0.5069)*((clay_F23_2014 + 0.3421)^(0.0184)))
(BIOptf_F23=(0.014*SOC_F23_2014 + 0.0075)*((clay_F23_2014 + 8.8473)^(0.0567)))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
(DPMptf_F23=SOC_F23_2014-(FallIOM_F23+RPMptf_F23+HUMptf_F23+BIOptf_F23))
c(DPMptf_F23, RPMptf_F23, BIOptf_F23, HUMptf_F23, FallIOM_F23)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F23_forest=SOC_F23_2014-(IOM_FOREST_F23+RPMptf_F23+HUMptf_F23+BIOptf_F23)
c(DPMptf_F23_forest, RPMptf_F23, BIOptf_F23, HUMptf_F23, IOM_FOREST_F23)


### e) RothC models (LITTERFALL) ------------------------------------------------------------
#### e.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct1_F23=getC(Model1_F23) #Calculates stocks for each pool per month
TotalC1_F23=rowSums(Ct1_F23)#Calculates total (sum of all pools)  
Rt1_F23=getReleaseFlux(Model1_F23)#CO2 released for all pools 

matplot(years, Ct1_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F23_df <- data.frame(years,as.data.frame(Ct1_F23),TotalC1_F23)
names(Ct1_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F23=tail(Ct1_F23_df,1)
poolSize_F23$Stand_age="80 years"
poolSize_F23$Harvest_year=1933
poolSize_F23 #Reported data

Ct1_long_F23 <- Ct1_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F23$PSP=26
Ct1_long_F23$Stand_age=c("80 years")
Ct1_long_F23$Harvest_year=1933


#### e.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct2_F23=getC(Model2_F23) #Calculates stocks for each pool per month
TotalC2_F23=rowSums(Ct2_F23)#Calculates total (sum of all pools)  
Rt2_F23=getReleaseFlux(Model2_F23)#CO2 released for all pools 

matplot(years, Ct2_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F23_df <- data.frame(years,as.data.frame(Ct2_F23),TotalC2_F23)
names(Ct2_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F23=tail(Ct2_F23_df,1)
poolSize2_F23$Stand_age="80 years"
poolSize2_F23$Harvest_year=1933
poolSize2_F23 #Reported data

Ct2_long_F23 <- Ct2_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23$PSP=26
Ct2_long_F23$Stand_age=c("80 years")
Ct2_long_F23$Harvest_year=1933



#### e.3) RothC model 3 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model3_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct3_F23=getC(Model3_F23) #Calculates stocks for each pool per month
TotalC3_F23=rowSums(Ct3_F23)#Calculates total (sum of all pools)  
Rt3_F23=getReleaseFlux(Model3_F23)#CO2 released for all pools 

matplot(years, Ct3_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F23_df <- data.frame(years,as.data.frame(Ct3_F23),TotalC3_F23)
names(Ct3_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F23=tail(Ct3_F23_df,1)
poolSize3_F23$Stand_age="80 years"
poolSize3_F23$Harvest_year=1933
poolSize3_F23 #Reported data

Ct3_long_F23 <- Ct3_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F23$PSP=26
Ct3_long_F23$Stand_age=c("80 years")
Ct3_long_F23$Harvest_year=1933
View(Ct3_long_F23)


#### e.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct4_F23=getC(Model4_F23) #Calculates stocks for each pool per month
TotalC4_F23=rowSums(Ct4_F23)#Calculates total (sum of all pools)  
Rt4_F23=getReleaseFlux(Model4_F23)#CO2 released for all pools 

matplot(years, Ct4_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F23_df <- data.frame(years,as.data.frame(Ct4_F23),TotalC4_F23)
names(Ct4_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F23=tail(Ct4_F23_df,1)
poolSize4_F23$Stand_age="80 years"
poolSize4_F23$Harvest_year=1933
poolSize4_F23 #Reported data

Ct4_long_F23 <- Ct4_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F23$PSP=26
Ct4_long_F23$Stand_age=c("80 years")
Ct4_long_F23$Harvest_year=1933



### f) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### f.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct5_F23=getC(Model5_F23) #Calculates stocks for each pool per month
TotalC5_F23=rowSums(Ct5_F23)#Calculates total (sum of all pools)  
Rt5_F23=getReleaseFlux(Model5_F23)#CO2 released for all pools 

matplot(years, Ct5_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F23_df <- data.frame(years,as.data.frame(Ct5_F23),TotalC5_F23)
names(Ct5_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F23=tail(Ct5_F23_df,1)
poolSize5_F23$Stand_age="80 years"
poolSize5_F23$Harvest_year=1933
poolSize5_F23 #Reported data

Ct5_long_F23 <- Ct5_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F23$PSP=26
Ct5_long_F23$Stand_age=c("80 years")
Ct5_long_F23$Harvest_year=1933


#### f.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct6_F23=getC(Model6_F23) #Calculates stocks for each pool per month
TotalC6_F23=rowSums(Ct6_F23)#Calculates total (sum of all pools)  
Rt6_F23=getReleaseFlux(Model6_F23)#CO2 released for all pools 

matplot(years, Ct6_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F23_df <- data.frame(years,as.data.frame(Ct6_F23),TotalC6_F23)
names(Ct6_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F23=tail(Ct6_F23_df,1)
poolSize6_F23$Stand_age="80 years"
poolSize6_F23$Harvest_year=1933
poolSize6_F23 #Reported data

Ct6_long_F23 <- Ct6_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F23$PSP=26
Ct6_long_F23$Stand_age=c("80 years")
Ct6_long_F23$Harvest_year=1933



#### f.3) RothC model 7 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model7_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct7_F23=getC(Model7_F23) #Calculates stocks for each pool per month
TotalC7_F23=rowSums(Ct7_F23)#Calculates total (sum of all pools)  
Rt7_F23=getReleaseFlux(Model7_F23)#CO2 released for all pools 

matplot(years, Ct7_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F23_df <- data.frame(years,as.data.frame(Ct7_F23),TotalC7_F23)
names(Ct7_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F23=tail(Ct7_F23_df,1)
poolSize7_F23$Stand_age="80 years"
poolSize7_F23$Harvest_year=1933
poolSize7_F23 #Reported data

Ct7_long_F23 <- Ct7_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F23$PSP=26
Ct7_long_F23$Stand_age=c("80 years")
Ct7_long_F23$Harvest_year=1933



#### f.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct8_F23=getC(Model8_F23) #Calculates stocks for each pool per month
TotalC8_F23=rowSums(Ct8_F23)#Calculates total (sum of all pools)  
Rt8_F23=getReleaseFlux(Model8_F23)#CO2 released for all pools 

matplot(years, Ct8_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F23_df <- data.frame(years,as.data.frame(Ct8_F23),TotalC8_F23)
names(Ct8_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F23=tail(Ct8_F23_df,1)
poolSize8_F23$Stand_age="80 years"
poolSize8_F23$Harvest_year=1933
poolSize8_F23 #Reported data

Ct8_long_F23 <- Ct8_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F23$PSP=26
Ct8_long_F23$Stand_age=c("80 years")
Ct8_long_F23$Harvest_year=1933



### g) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### g.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_FORESTFLOOR,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model
Ct9_F23=getC(Model9_F23) #Calculates stocks for each pool per month
TotalC9_F23=rowSums(Ct9_F23)#Calculates total (sum of all pools)  
Rt9_F23=getReleaseFlux(Model9_F23)#CO2 released for all pools 

matplot(years, Ct9_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F23_df <- data.frame(years,as.data.frame(Ct9_F23),TotalC9_F23)
names(Ct9_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F23=tail(Ct9_F23_df,1)
poolSize9_F23$Stand_age="80 years"
poolSize9_F23$Harvest_year=1933
poolSize9_F23 #Reported data

Ct9_long_F23 <- Ct9_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F23$PSP=26
Ct9_long_F23$Stand_age=c("80 years")
Ct9_long_F23$Harvest_year=1933


#### g.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F23=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23) #Loads the model
Ct10_F23=getC(Model10_F23) #Calculates stocks for each pool per month
TotalC10_F23=rowSums(Ct10_F23)#Calculates total (sum of all pools)  
Rt10_F23=getReleaseFlux(Model10_F23)#CO2 released for all pools 

matplot(years, Ct10_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F23_df <- data.frame(years,as.data.frame(Ct10_F23),TotalC10_F23)
names(Ct10_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F23=tail(Ct10_F23_df,1)
poolSize10_F23$Stand_age="80 years"
poolSize10_F23$Harvest_year=1933
poolSize10_F23 #Reported data

Ct10_long_F23 <- Ct10_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F23$PSP=26
Ct10_long_F23$Stand_age=c("80 years")
Ct10_long_F23$Harvest_year=1933



#### g.3) RothC model 7 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model11_F23=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23) #Loads the model
Ct11_F23=getC(Model11_F23) #Calculates stocks for each pool per month
TotalC11_F23=rowSums(Ct11_F23)#Calculates total (sum of all pools)  
Rt11_F23=getReleaseFlux(Model11_F23)#CO2 released for all pools 

matplot(years, Ct11_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F23_df <- data.frame(years,as.data.frame(Ct11_F23),TotalC11_F23)
names(Ct11_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F23=tail(Ct11_F23_df,1)
poolSize11_F23$Stand_age="80 years"
poolSize11_F23$Harvest_year=1933
poolSize11_F23 #Reported data

Ct11_long_F23 <- Ct11_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F23$PSP=26
Ct11_long_F23$Stand_age=c("80 years")
Ct11_long_F23$Harvest_year=1933



#### g.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F23=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23) #Loads the model
Ct12_F23=getC(Model12_F23) #Calculates stocks for each pool per month
TotalC12_F23=rowSums(Ct12_F23)#Calculates total (sum of all pools)  
Rt12_F23=getReleaseFlux(Model12_F23)#CO2 released for all pools 

matplot(years, Ct12_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F23_df <- data.frame(years,as.data.frame(Ct8_F23),TotalC8_F23)
names(Ct12_F23_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F23=tail(Ct12_F23_df,1)
poolSize12_F23$Stand_age="80 years"
poolSize12_F23$Harvest_year=1933
poolSize12_F23 #Reported data

Ct12_long_F23 <- Ct12_F23_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F23$PSP=26
Ct12_long_F23$Stand_age=c("80 years")
Ct12_long_F23$Harvest_year=1933



# 7. SENSITITIVY TO WEATHER DATA -----------------------------------------------
## 7.1. FOLIO 38 (7 AÑOS) ------------------------------------------------------
xi.frame_F38_MELI=data.frame(years,rep(fT_MELI*fW_F38_MELI,length.out=length(years)))
### a) RothC models (LITTERFALL) ------------------------------------------------------------
#### a.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct1_F38_MELI=getC(Model1_F38_MELI) #Calculates stocks for each pool per month
TotalC1_F38_MELI=rowSums(Ct1_F38_MELI)#Calculates total (sum of all pools)  
Rt1_F38_MELI=getReleaseFlux(Model1_F38_MELI)#CO2 released for all pools 

matplot(years, Ct1_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F38_df_MELI <- data.frame(years,as.data.frame(Ct1_F38_MELI),TotalC1_F38_MELI)
names(Ct1_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F38_MELI=tail(Ct1_F38_df_MELI,1)
poolSize_F38_MELI$Stand_age="07 años"
poolSize_F38_MELI$Harvest_year=2005
poolSize_F38_MELI #Reported data

Ct1_long_F38_MELI <- Ct1_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F38_MELI$PSP=38
Ct1_long_F38_MELI$Stand_age=c("07 years")
Ct1_long_F38_MELI$Harvest_year=2005


#### a.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct2_F38_MELI=getC(Model2_F38_MELI) #Calculates stocks for each pool per month
TotalC2_F38_MELI=rowSums(Ct2_F38_MELI)#Calculates total (sum of all pools)  
Rt2_F38_MELI=getReleaseFlux(Model2_F38_MELI)#CO2 released for all pools 

matplot(years, Ct2_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F38_df_MELI <- data.frame(years,as.data.frame(Ct2_F38_MELI),TotalC2_F38_MELI)
names(Ct2_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F38_MELI=tail(Ct2_F38_df_MELI,1)
poolSize2_F38_MELI$Stand_age="07 años"
poolSize2_F38_MELI$Harvest_year=2005
poolSize2_F38_MELI #Reported data

Ct2_long_F38_MELI <- Ct2_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38_MELI$PSP=38
Ct2_long_F38_MELI$Stand_age=c("07 years")
Ct2_long_F38_MELI$Harvest_year=2005



#### a.3) RothC model 3 (IOM_G & PF) - REPORTED F38-----------------------------------------
Model3_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct3_F38_MELI=getC(Model3_F38_MELI) #Calculates stocks for each pool per month
TotalC3_F38_MELI=rowSums(Ct3_F38_MELI)#Calculates total (sum of all pools)  
Rt3_F38_MELI=getReleaseFlux(Model3_F38_MELI)#CO2 released for all pools 

matplot(years, Ct3_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F38_df_MELI <- data.frame(years,as.data.frame(Ct3_F38_MELI),TotalC3_F38_MELI)
names(Ct3_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F38_MELI=tail(Ct3_F38_df_MELI,1)
poolSize3_F38_MELI$Stand_age="07 years"
poolSize3_F38_MELI$Harvest_year=2005
poolSize3_F38_MELI #Reported data

Ct3_long_F38_MELI <- Ct3_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F38_MELI$PSP=38
Ct3_long_F38_MELI$Stand_age=c("07 years")
Ct3_long_F38_MELI$Harvest_year=2005



#### a.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFAL,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct4_F38_MELI=getC(Model4_F38_MELI) #Calculates stocks for each pool per month
TotalC4_F38_MELI=rowSums(Ct4_F38_MELI)#Calculates total (sum of all pools)  
Rt4_F38_MELI=getReleaseFlux(Model4_F38_MELI)#CO2 released for all pools 

matplot(years, Ct4_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F38_df_MELI <- data.frame(years,as.data.frame(Ct4_F38_MELI),TotalC4_F38_MELI)
names(Ct4_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F38_MELI=tail(Ct4_F38_df_MELI,1)
poolSize4_F38_MELI$Stand_age="07 años"
poolSize4_F38_MELI$Harvest_year=2005
poolSize4_F38_MELI #Reported data

Ct4_long_F38_MELI <- Ct4_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F38_MELI$PSP=38
Ct4_long_F38_MELI$Stand_age=c("07 years")
Ct4_long_F38_MELI$Harvest_year=2005



### b) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### b.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct5_F38_MELI=getC(Model5_F38_MELI) #Calculates stocks for each pool per month
TotalC5_F38_MELI=rowSums(Ct5_F38_MELI)#Calculates total (sum of all pools)  
Rt5_F38_MELI=getReleaseFlux(Model5_F38_MELI)#CO2 released for all pools 

matplot(years, Ct5_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F38_df_MELI <- data.frame(years,as.data.frame(Ct5_F38_MELI),TotalC5_F38_MELI)
names(Ct5_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F38_MELI=tail(Ct5_F38_df_MELI,1)
poolSize5_F38_MELI$Stand_age="07 años"
poolSize5_F38_MELI$Harvest_year=2005
poolSize5_F38_MELI #Reported data

Ct5_long_F38_MELI <- Ct5_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F38_MELI$PSP=38
Ct5_long_F38_MELI$Stand_age=c("07 years")
Ct5_long_F38_MELI$Harvest_year=2005


#### b.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct6_F38_MELI=getC(Model6_F38_MELI) #Calculates stocks for each pool per month
TotalC6_F38_MELI=rowSums(Ct6_F38_MELI)#Calculates total (sum of all pools)  
Rt6_F38_MELI=getReleaseFlux(Model6_F38_MELI)#CO2 released for all pools 

matplot(years, Ct6_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F38_df_MELI <- data.frame(years,as.data.frame(Ct6_F38_MELI),TotalC6_F38_MELI)
names(Ct6_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F38_MELI=tail(Ct6_F38_df_MELI,1)
poolSize6_F38_MELI$Stand_age="07 años"
poolSize6_F38_MELI$Harvest_year=2005
poolSize6_F38_MELI #Reported data

Ct6_long_F38_MELI <- Ct6_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F38_MELI$PSP=38
Ct6_long_F38_MELI$Stand_age=c("07 years")
Ct6_long_F38_MELI$Harvest_year=2005



#### b.3) RothC model 7 (IOM_G & PF) - REPORTED F38-----------------------------------------
Model7_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct7_F38_MELI=getC(Model7_F38_MELI) #Calculates stocks for each pool per month
TotalC7_F38_MELI=rowSums(Ct7_F38_MELI)#Calculates total (sum of all pools)  
Rt7_F38_MELI=getReleaseFlux(Model7_F38_MELI)#CO2 released for all pools 

matplot(years, Ct7_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F38_df_MELI <- data.frame(years,as.data.frame(Ct7_F38_MELI),TotalC7_F38_MELI)
names(Ct7_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F38_MELI=tail(Ct7_F38_df_MELI,1)
poolSize7_F38_MELI$Stand_age="07 years"
poolSize7_F38_MELI$Harvest_year=2005
poolSize7_F38_MELI #Reported data

Ct7_long_F38_MELI <- Ct7_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F38_MELI$PSP=38
Ct7_long_F38_MELI$Stand_age=c("07 years")
Ct7_long_F38_MELI$Harvest_year=2005



#### b.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                      In=Cinputs_F38_2014_LITTERFALL_ROOT,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct8_F38_MELI=getC(Model8_F38_MELI) #Calculates stocks for each pool per month
TotalC8_F38_MELI=rowSums(Ct8_F38_MELI)#Calculates total (sum of all pools)  
Rt8_F38_MELI=getReleaseFlux(Model8_F38_MELI)#CO2 released for all pools 

matplot(years, Ct8_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F38_df_MELI <- data.frame(years,as.data.frame(Ct8_F38_MELI),TotalC8_F38_MELI)
names(Ct8_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F38_MELI=tail(Ct8_F38_df_MELI,1)
poolSize8_F38_MELI$Stand_age="07 años"
poolSize8_F38_MELI$Harvest_year=2005
poolSize8_F38_MELI #Reported data

Ct8_long_F38_MELI <- Ct8_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F38_MELI$PSP=38
Ct8_long_F38_MELI$Stand_age=c("07 years")
Ct8_long_F38_MELI$Harvest_year=2005



### c) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### c.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F38_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014_FORESTFLOOR,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_MELI) #Loads the model
Ct9_F38_MELI=getC(Model9_F38_MELI) #Calculates stocks for each pool per month
TotalC9_F38_MELI=rowSums(Ct9_F38_MELI)#Calculates total (sum of all pools)  
Rt9_F38_MELI=getReleaseFlux(Model9_F38_MELI)#CO2 released for all pools 

matplot(years, Ct9_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F38_df_MELI <- data.frame(years,as.data.frame(Ct9_F38_MELI),TotalC9_F38_MELI)
names(Ct9_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F38_MELI=tail(Ct9_F38_df_MELI,1)
poolSize9_F38_MELI$Stand_age="07 años"
poolSize9_F38_MELI$Harvest_year=2005
poolSize9_F38_MELI #Reported data

Ct9_long_F38_MELI <- Ct9_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F38_MELI$PSP=38
Ct9_long_F38_MELI$Stand_age=c("07 years")
Ct9_long_F38_MELI$Harvest_year=2005


#### c.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F38_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                       In=Cinputs_F38_2014_FORESTFLOOR,
                       clay=clay_F38_2014,
                       DR=0.25,
                       xi=xi.frame_F38_MELI) #Loads the model
Ct10_F38_MELI=getC(Model10_F38_MELI) #Calculates stocks for each pool per month
TotalC10_F38_MELI=rowSums(Ct10_F38_MELI)#Calculates total (sum of all pools)  
Rt10_F38_MELI=getReleaseFlux(Model10_F38_MELI)#CO2 released for all pools 

matplot(years, Ct10_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F38_df_MELI <- data.frame(years,as.data.frame(Ct10_F38_MELI),TotalC10_F38_MELI)
names(Ct10_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F38_MELI=tail(Ct10_F38_df_MELI,1)
poolSize10_F38_MELI$Stand_age="07 años"
poolSize10_F38_MELI$Harvest_year=2005
poolSize10_F38_MELI #Reported data

Ct10_long_F38_MELI <- Ct10_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F38_MELI$PSP=38
Ct10_long_F38_MELI$Stand_age=c("07 years")
Ct10_long_F38_MELI$Harvest_year=2005



#### c.3) RothC model 7 (IOM_G & PF) - REPORTED F38-----------------------------------------
Model11_F38_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                       In=Cinputs_F38_2014_FORESTFLOOR,
                       clay=clay_F38_2014,
                       DR=0.25,
                       xi=xi.frame_F38_MELI) #Loads the model
Ct11_F38_MELI=getC(Model11_F38_MELI) #Calculates stocks for each pool per month
TotalC11_F38_MELI=rowSums(Ct11_F38_MELI)#Calculates total (sum of all pools)  
Rt11_F38_MELI=getReleaseFlux(Model11_F38_MELI)#CO2 released for all pools 

matplot(years, Ct11_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F38_df_MELI <- data.frame(years,as.data.frame(Ct11_F38_MELI),TotalC11_F38_MELI)
names(Ct11_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F38_MELI=tail(Ct11_F38_df_MELI,1)
poolSize11_F38_MELI$Stand_age="07 years"
poolSize11_F38_MELI$Harvest_year=2005
poolSize11_F38_MELI #Reported data

Ct11_long_F38_MELI <- Ct11_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F38_MELI$PSP=38
Ct11_long_F38_MELI$Stand_age=c("07 years")
Ct11_long_F38_MELI$Harvest_year=2005



#### c.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F38_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F38_forest,RPMptf_F38,BIOptf_F38,HUMptf_F38,IOM_FOREST_F38),
                       In=Cinputs_F38_2014_FORESTFLOOR,
                       clay=clay_F38_2014,
                       DR=0.25,
                       xi=xi.frame_F38_MELI) #Loads the model
Ct12_F38_MELI=getC(Model12_F38_MELI) #Calculates stocks for each pool per month
TotalC12_F38_MELI=rowSums(Ct12_F38_MELI)#Calculates total (sum of all pools)  
Rt12_F38_MELI=getReleaseFlux(Model12_F38_MELI)#CO2 released for all pools 

matplot(years, Ct12_F38_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F38_df_MELI <- data.frame(years,as.data.frame(Ct8_F38_MELI),TotalC8_F38_MELI)
names(Ct12_F38_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F38_MELI=tail(Ct12_F38_df_MELI,1)
poolSize12_F38_MELI$Stand_age="07 años"
poolSize12_F38_MELI$Harvest_year=2005
poolSize12_F38_MELI #Reported data

Ct12_long_F38_MELI <- Ct12_F38_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F38_MELI$PSP=38
Ct12_long_F38_MELI$Stand_age=c("07 years")
Ct12_long_F38_MELI$Harvest_year=2005



## 7.2. FOLIO 32 (18 AÑOS) ------------------------------------------------------
xi.frame_F32_MELI=data.frame(years,rep(fT_MELI*fW_F32_MELI,length.out=length(years)))
### d) RothC models (LITTERFALL) ------------------------------------------------------------
#### d.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct1_F32_MELI=getC(Model1_F32_MELI) #Calculates stocks for each pool per month
TotalC1_F32_MELI=rowSums(Ct1_F32_MELI)#Calculates total (sum of all pools)  
Rt1_F32_MELI=getReleaseFlux(Model1_F32_MELI)#CO2 released for all pools 

matplot(years, Ct1_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F32_df_MELI <- data.frame(years,as.data.frame(Ct1_F32_MELI),TotalC1_F32_MELI)
names(Ct1_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F32_MELI=tail(Ct1_F32_df_MELI,1)
poolSize_F32_MELI$Stand_age="18 años"
poolSize_F32_MELI$Harvest_year=1995
poolSize_F32_MELI #Reported data

Ct1_long_F32_MELI <- Ct1_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F32_MELI$PSP=32
Ct1_long_F32_MELI$Stand_age=c("18 years")
Ct1_long_F32_MELI$Harvest_year=1995


#### d.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct2_F32_MELI=getC(Model2_F32_MELI) #Calculates stocks for each pool per month
TotalC2_F32_MELI=rowSums(Ct2_F32_MELI)#Calculates total (sum of all pools)  
Rt2_F32_MELI=getReleaseFlux(Model2_F32_MELI)#CO2 released for all pools 

matplot(years, Ct2_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F32_df_MELI <- data.frame(years,as.data.frame(Ct2_F32_MELI),TotalC2_F32_MELI)
names(Ct2_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F32_MELI=tail(Ct2_F32_df_MELI,1)
poolSize2_F32_MELI$Stand_age="18 years"
poolSize2_F32_MELI$Harvest_year=1995
poolSize2_F32_MELI #Reported data

Ct2_long_F32_MELI <- Ct2_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32_MELI$PSP=32
Ct2_long_F32_MELI$Stand_age=c("18 years")
Ct2_long_F32_MELI$Harvest_year=1995



#### d.3) RothC model 3 (IOM_G & PF) - REPORTED F32-----------------------------------------
Model3_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct3_F32_MELI=getC(Model3_F32_MELI) #Calculates stocks for each pool per month
TotalC3_F32_MELI=rowSums(Ct3_F32_MELI)#Calculates total (sum of all pools)  
Rt3_F32_MELI=getReleaseFlux(Model3_F32_MELI)#CO2 released for all pools 

matplot(years, Ct3_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F32_df_MELI <- data.frame(years,as.data.frame(Ct3_F32_MELI),TotalC3_F32_MELI)
names(Ct3_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F32_MELI=tail(Ct3_F32_df_MELI,1)
poolSize3_F32_MELI$Stand_age="18 years"
poolSize3_F32_MELI$Harvest_year=1995
poolSize3_F32_MELI #Reported data

Ct3_long_F32_MELI <- Ct3_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F32_MELI$PSP=32
Ct3_long_F32_MELI$Stand_age=c("18 years")
Ct3_long_F32_MELI$Harvest_year=1995



#### d.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFAL,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct4_F32_MELI=getC(Model4_F32_MELI) #Calculates stocks for each pool per month
TotalC4_F32_MELI=rowSums(Ct4_F32_MELI)#Calculates total (sum of all pools)  
Rt4_F32_MELI=getReleaseFlux(Model4_F32_MELI)#CO2 released for all pools 

matplot(years, Ct4_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F32_df_MELI <- data.frame(years,as.data.frame(Ct4_F32_MELI),TotalC4_F32_MELI)
names(Ct4_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F32_MELI=tail(Ct4_F32_df_MELI,1)
poolSize4_F32_MELI$Stand_age="18 years"
poolSize4_F32_MELI$Harvest_year=1995
poolSize4_F32_MELI #Reported data

Ct4_long_F32_MELI <- Ct4_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F32_MELI$PSP=32
Ct4_long_F32_MELI$Stand_age=c("18 years")
Ct4_long_F32_MELI$Harvest_year=1995



### e) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### e.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct5_F32_MELI=getC(Model5_F32_MELI) #Calculates stocks for each pool per month
TotalC5_F32_MELI=rowSums(Ct5_F32_MELI)#Calculates total (sum of all pools)  
Rt5_F32_MELI=getReleaseFlux(Model5_F32_MELI)#CO2 released for all pools 

matplot(years, Ct5_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F32_df_MELI <- data.frame(years,as.data.frame(Ct5_F32_MELI),TotalC5_F32_MELI)
names(Ct5_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F32_MELI=tail(Ct5_F32_df_MELI,1)
poolSize5_F32_MELI$Stand_age="18 years"
poolSize5_F32_MELI$Harvest_year=1995
poolSize5_F32_MELI #Reported data

Ct5_long_F32_MELI <- Ct5_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F32_MELI$PSP=32
Ct5_long_F32_MELI$Stand_age=c("18 years")
Ct5_long_F32_MELI$Harvest_year=1995


#### e.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct6_F32_MELI=getC(Model6_F32_MELI) #Calculates stocks for each pool per month
TotalC6_F32_MELI=rowSums(Ct6_F32_MELI)#Calculates total (sum of all pools)  
Rt6_F32_MELI=getReleaseFlux(Model6_F32_MELI)#CO2 released for all pools 

matplot(years, Ct6_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F32_df_MELI <- data.frame(years,as.data.frame(Ct6_F32_MELI),TotalC6_F32_MELI)
names(Ct6_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F32_MELI=tail(Ct6_F32_df_MELI,1)
poolSize6_F32_MELI$Stand_age="18 years"
poolSize6_F32_MELI$Harvest_year=1995
poolSize6_F32_MELI #Reported data

Ct6_long_F32_MELI <- Ct6_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F32_MELI$PSP=32
Ct6_long_F32_MELI$Stand_age=c("18 years")
Ct6_long_F32_MELI$Harvest_year=1995



#### e.3) RothC model 7 (IOM_G & PF) - REPORTED F32-----------------------------------------
Model7_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct7_F32_MELI=getC(Model7_F32_MELI) #Calculates stocks for each pool per month
TotalC7_F32_MELI=rowSums(Ct7_F32_MELI)#Calculates total (sum of all pools)  
Rt7_F32_MELI=getReleaseFlux(Model7_F32_MELI)#CO2 released for all pools 

matplot(years, Ct7_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F32_df_MELI <- data.frame(years,as.data.frame(Ct7_F32_MELI),TotalC7_F32_MELI)
names(Ct7_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F32_MELI=tail(Ct7_F32_df_MELI,1)
poolSize7_F32_MELI$Stand_age="18 years"
poolSize7_F32_MELI$Harvest_year=1995
poolSize7_F32_MELI #Reported data

Ct7_long_F32_MELI <- Ct7_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F32_MELI$PSP=32
Ct7_long_F32_MELI$Stand_age=c("18 years")
Ct7_long_F32_MELI$Harvest_year=1995



#### e.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                      In=Cinputs_F32_2014_LITTERFALL_ROOT,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct8_F32_MELI=getC(Model8_F32_MELI) #Calculates stocks for each pool per month
TotalC8_F32_MELI=rowSums(Ct8_F32_MELI)#Calculates total (sum of all pools)  
Rt8_F32_MELI=getReleaseFlux(Model8_F32_MELI)#CO2 released for all pools 

matplot(years, Ct8_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F32_df_MELI <- data.frame(years,as.data.frame(Ct8_F32_MELI),TotalC8_F32_MELI)
names(Ct8_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F32_MELI=tail(Ct8_F32_df_MELI,1)
poolSize8_F32_MELI$Stand_age="18 years"
poolSize8_F32_MELI$Harvest_year=1995
poolSize8_F32_MELI #Reported data

Ct8_long_F32_MELI <- Ct8_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F32_MELI$PSP=32
Ct8_long_F32_MELI$Stand_age=c("18 years")
Ct8_long_F32_MELI$Harvest_year=1995



### f) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### f.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F32_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014_FORESTFLOOR,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32_MELI) #Loads the model
Ct9_F32_MELI=getC(Model9_F32_MELI) #Calculates stocks for each pool per month
TotalC9_F32_MELI=rowSums(Ct9_F32_MELI)#Calculates total (sum of all pools)  
Rt9_F32_MELI=getReleaseFlux(Model9_F32_MELI)#CO2 released for all pools 

matplot(years, Ct9_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F32_df_MELI <- data.frame(years,as.data.frame(Ct9_F32_MELI),TotalC9_F32_MELI)
names(Ct9_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F32_MELI=tail(Ct9_F32_df_MELI,1)
poolSize9_F32_MELI$Stand_age="18 years"
poolSize9_F32_MELI$Harvest_year=1995
poolSize9_F32_MELI #Reported data

Ct9_long_F32_MELI <- Ct9_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F32_MELI$PSP=32
Ct9_long_F32_MELI$Stand_age=c("18 years")
Ct9_long_F32_MELI$Harvest_year=1995


#### f.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F32_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32_MELI) #Loads the model
Ct10_F32_MELI=getC(Model10_F32_MELI) #Calculates stocks for each pool per month
TotalC10_F32_MELI=rowSums(Ct10_F32_MELI)#Calculates total (sum of all pools)  
Rt10_F32_MELI=getReleaseFlux(Model10_F32_MELI)#CO2 released for all pools 

matplot(years, Ct10_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F32_df_MELI <- data.frame(years,as.data.frame(Ct10_F32_MELI),TotalC10_F32_MELI)
names(Ct10_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F32_MELI=tail(Ct10_F32_df_MELI,1)
poolSize10_F32_MELI$Stand_age="18 years"
poolSize10_F32_MELI$Harvest_year=1995
poolSize10_F32_MELI #Reported data

Ct10_long_F32_MELI <- Ct10_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F32_MELI$PSP=32
Ct10_long_F32_MELI$Stand_age=c("18 years")
Ct10_long_F32_MELI$Harvest_year=1995



#### f.3) RothC model 7 (IOM_G & PF) - REPORTED F32 -----------------------------------------
Model11_F32_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F32,RPMptf_F32,BIOptf_F32,HUMptf_F32,FallIOM_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32_MELI) #Loads the model
Ct11_F32_MELI=getC(Model11_F32_MELI) #Calculates stocks for each pool per month
TotalC11_F32_MELI=rowSums(Ct11_F32_MELI)#Calculates total (sum of all pools)  
Rt11_F32_MELI=getReleaseFlux(Model11_F32_MELI)#CO2 released for all pools 

matplot(years, Ct11_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F32_df_MELI <- data.frame(years,as.data.frame(Ct11_F32_MELI),TotalC11_F32_MELI)
names(Ct11_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F32_MELI=tail(Ct11_F32_df_MELI,1)
poolSize11_F32_MELI$Stand_age="18 years"
poolSize11_F32_MELI$Harvest_year=1995
poolSize11_F32_MELI #Reported data

Ct11_long_F32_MELI <- Ct11_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F32_MELI$PSP=32
Ct11_long_F32_MELI$Stand_age=c("18 years")
Ct11_long_F32_MELI$Harvest_year=1995



#### f.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F32_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F32_forest,RPMptf_F32,BIOptf_F32,HUMptf_F32,IOM_FOREST_F32),
                       In=Cinputs_F32_2014_FORESTFLOOR,
                       clay=clay_F32_2014,
                       DR=0.25,
                       xi=xi.frame_F32_MELI) #Loads the model
Ct12_F32_MELI=getC(Model12_F32_MELI) #Calculates stocks for each pool per month
TotalC12_F32_MELI=rowSums(Ct12_F32_MELI)#Calculates total (sum of all pools)  
Rt12_F32_MELI=getReleaseFlux(Model12_F32_MELI)#CO2 released for all pools 

matplot(years, Ct12_F32_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F32_df_MELI <- data.frame(years,as.data.frame(Ct8_F32_MELI),TotalC8_F32_MELI)
names(Ct12_F32_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F32_MELI=tail(Ct12_F32_df_MELI,1)
poolSize12_F32_MELI$Stand_age="18 years"
poolSize12_F32_MELI$Harvest_year=1995
poolSize12_F32_MELI #Reported data

Ct12_long_F32_MELI <- Ct12_F32_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F32_MELI$PSP=32
Ct12_long_F32_MELI$Stand_age=c("18 years")
Ct12_long_F32_MELI$Harvest_year=1995



## 7.3. FOLIO 26 (30 years) ------------------------------------------------------
xi.frame_F26_MELI=data.frame(years,rep(fT_MELI*fW_F26_MELI,length.out=length(years)))
### g) RothC models (LITTERFALL) ------------------------------------------------------------
#### g.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct1_F26_MELI=getC(Model1_F26_MELI) #Calculates stocks for each pool per month
TotalC1_F26_MELI=rowSums(Ct1_F26_MELI)#Calculates total (sum of all pools)  
Rt1_F26_MELI=getReleaseFlux(Model1_F26_MELI)#CO2 released for all pools 

matplot(years, Ct1_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F26_df_MELI <- data.frame(years,as.data.frame(Ct1_F26_MELI),TotalC1_F26_MELI)
names(Ct1_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F26_MELI=tail(Ct1_F26_df_MELI,1)
poolSize_F26_MELI$Stand_age="30 years"
poolSize_F26_MELI$Harvest_year=1983
poolSize_F26_MELI #Reported data

Ct1_long_F26_MELI <- Ct1_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F26_MELI$PSP=26
Ct1_long_F26_MELI$Stand_age=c("30 years")
Ct1_long_F26_MELI$Harvest_year=1983


#### g.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct2_F26_MELI=getC(Model2_F26_MELI) #Calculates stocks for each pool per month
TotalC2_F26_MELI=rowSums(Ct2_F26_MELI)#Calculates total (sum of all pools)  
Rt2_F26_MELI=getReleaseFlux(Model2_F26_MELI)#CO2 released for all pools 

matplot(years, Ct2_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F26_df_MELI <- data.frame(years,as.data.frame(Ct2_F26_MELI),TotalC2_F26_MELI)
names(Ct2_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F26_MELI=tail(Ct2_F26_df_MELI,1)
poolSize2_F26_MELI$Stand_age="30 years"
poolSize2_F26_MELI$Harvest_year=1983
poolSize2_F26_MELI #Reported data

Ct2_long_F26_MELI <- Ct2_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26_MELI$PSP=26
Ct2_long_F26_MELI$Stand_age=c("30 years")
Ct2_long_F26_MELI$Harvest_year=1983



#### g.3) RothC model 3 (IOM_G & PF) - REPORTED F30-----------------------------------------
Model3_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct3_F26_MELI=getC(Model3_F26_MELI) #Calculates stocks for each pool per month
TotalC3_F26_MELI=rowSums(Ct3_F26_MELI)#Calculates total (sum of all pools)  
Rt3_F26_MELI=getReleaseFlux(Model3_F26_MELI)#CO2 released for all pools 

matplot(years, Ct3_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F26_df_MELI <- data.frame(years,as.data.frame(Ct3_F26_MELI),TotalC3_F26_MELI)
names(Ct3_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F26_MELI=tail(Ct3_F26_df_MELI,1)
poolSize3_F26_MELI$Stand_age="30 years"
poolSize3_F26_MELI$Harvest_year=1983
poolSize3_F26_MELI #Reported data

Ct3_long_F26_MELI <- Ct3_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F26_MELI$PSP=26
Ct3_long_F26_MELI$Stand_age=c("30 years")
Ct3_long_F26_MELI$Harvest_year=1983



#### g.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFAL,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct4_F26_MELI=getC(Model4_F26_MELI) #Calculates stocks for each pool per month
TotalC4_F26_MELI=rowSums(Ct4_F26_MELI)#Calculates total (sum of all pools)  
Rt4_F26_MELI=getReleaseFlux(Model4_F26_MELI)#CO2 released for all pools 

matplot(years, Ct4_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F26_df_MELI <- data.frame(years,as.data.frame(Ct4_F26_MELI),TotalC4_F26_MELI)
names(Ct4_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F26_MELI=tail(Ct4_F26_df_MELI,1)
poolSize4_F26_MELI$Stand_age="30 years"
poolSize4_F26_MELI$Harvest_year=1983
poolSize4_F26_MELI #Reported data

Ct4_long_F26_MELI <- Ct4_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F26_MELI$PSP=26
Ct4_long_F26_MELI$Stand_age=c("30 years")
Ct4_long_F26_MELI$Harvest_year=1983



### h) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### h.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct5_F26_MELI=getC(Model5_F26_MELI) #Calculates stocks for each pool per month
TotalC5_F26_MELI=rowSums(Ct5_F26_MELI)#Calculates total (sum of all pools)  
Rt5_F26_MELI=getReleaseFlux(Model5_F26_MELI)#CO2 released for all pools 

matplot(years, Ct5_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F26_df_MELI <- data.frame(years,as.data.frame(Ct5_F26_MELI),TotalC5_F26_MELI)
names(Ct5_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F26_MELI=tail(Ct5_F26_df_MELI,1)
poolSize5_F26_MELI$Stand_age="30 years"
poolSize5_F26_MELI$Harvest_year=1983
poolSize5_F26_MELI #Reported data

Ct5_long_F26_MELI <- Ct5_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F26_MELI$PSP=26
Ct5_long_F26_MELI$Stand_age=c("30 years")
Ct5_long_F26_MELI$Harvest_year=1983


#### h.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct6_F26_MELI=getC(Model6_F26_MELI) #Calculates stocks for each pool per month
TotalC6_F26_MELI=rowSums(Ct6_F26_MELI)#Calculates total (sum of all pools)  
Rt6_F26_MELI=getReleaseFlux(Model6_F26_MELI)#CO2 released for all pools 

matplot(years, Ct6_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F26_df_MELI <- data.frame(years,as.data.frame(Ct6_F26_MELI),TotalC6_F26_MELI)
names(Ct6_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F26_MELI=tail(Ct6_F26_df_MELI,1)
poolSize6_F26_MELI$Stand_age="30 years"
poolSize6_F26_MELI$Harvest_year=1983
poolSize6_F26_MELI #Reported data

Ct6_long_F26_MELI <- Ct6_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F26_MELI$PSP=26
Ct6_long_F26_MELI$Stand_age=c("30 years")
Ct6_long_F26_MELI$Harvest_year=1983



#### h.3) RothC model 7 (IOM_G & PF) - REPORTED F30-----------------------------------------
Model7_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct7_F26_MELI=getC(Model7_F26_MELI) #Calculates stocks for each pool per month
TotalC7_F26_MELI=rowSums(Ct7_F26_MELI)#Calculates total (sum of all pools)  
Rt7_F26_MELI=getReleaseFlux(Model7_F26_MELI)#CO2 released for all pools 

matplot(years, Ct7_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F26_df_MELI <- data.frame(years,as.data.frame(Ct7_F26_MELI),TotalC7_F26_MELI)
names(Ct7_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F26_MELI=tail(Ct7_F26_df_MELI,1)
poolSize7_F26_MELI$Stand_age="30 years"
poolSize7_F26_MELI$Harvest_year=1983
poolSize7_F26_MELI #Reported data

Ct7_long_F26_MELI <- Ct7_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F26_MELI$PSP=26
Ct7_long_F26_MELI$Stand_age=c("30 years")
Ct7_long_F26_MELI$Harvest_year=1983



#### h.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F26_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                      In=Cinputs_F26_2014_LITTERFALL_ROOT,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26_MELI) #Loads the model
Ct8_F26_MELI=getC(Model8_F26_MELI) #Calculates stocks for each pool per month
TotalC8_F26_MELI=rowSums(Ct8_F26_MELI)#Calculates total (sum of all pools)  
Rt8_F26_MELI=getReleaseFlux(Model8_F26_MELI)#CO2 released for all pools 

matplot(years, Ct8_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F26_df_MELI <- data.frame(years,as.data.frame(Ct8_F26_MELI),TotalC8_F26_MELI)
names(Ct8_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F26_MELI=tail(Ct8_F26_df_MELI,1)
poolSize8_F26_MELI$Stand_age="30 years"
poolSize8_F26_MELI$Harvest_year=1983
poolSize8_F26_MELI #Reported data

Ct8_long_F26_MELI <- Ct8_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F26_MELI$PSP=26
Ct8_long_F26_MELI$Stand_age=c("30 years")
Ct8_long_F26_MELI$Harvest_year=1983



### g) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### g.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014_FORESTFLOOR,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model
Ct9_F26=getC(Model9_F26) #Calculates stocks for each pool per month
TotalC9_F26=rowSums(Ct9_F26)#Calculates total (sum of all pools)  
Rt9_F26=getReleaseFlux(Model9_F26)#CO2 released for all pools 

matplot(years, Ct9_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F26_df <- data.frame(years,as.data.frame(Ct9_F26),TotalC9_F26)
names(Ct9_F26_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F26=tail(Ct9_F26_df,1)
poolSize9_F26$Stand_age="30 years"
poolSize9_F26$Harvest_year=1983
poolSize9_F26 #Reported data

Ct9_long_F26 <- Ct9_F26_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F26$PSP=26
Ct9_long_F26$Stand_age=c("30 years")
Ct9_long_F26$Harvest_year=1983


#### g.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F26_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26_MELI) #Loads the model
Ct10_F26_MELI=getC(Model10_F26_MELI) #Calculates stocks for each pool per month
TotalC10_F26_MELI=rowSums(Ct10_F26_MELI)#Calculates total (sum of all pools)  
Rt10_F26_MELI=getReleaseFlux(Model10_F26_MELI_MELI)#CO2 released for all pools 

matplot(years, Ct10_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F26_df_MELI <- data.frame(years,as.data.frame(Ct10_F26_MELI),TotalC10_F26_MELI)
names(Ct10_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F26_MELI=tail(Ct10_F26_df_MELI,1)
poolSize10_F26_MELI$Stand_age="30 years"
poolSize10_F26_MELI$Harvest_year=1983
poolSize10_F26_MELI #Reported data

Ct10_long_F26_MELI <- Ct10_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F26_MELI$PSP=26
Ct10_long_F26_MELI$Stand_age=c("30 years")
Ct10_long_F26_MELI$Harvest_year=1983



#### g.3) RothC model 7 (IOM_G & PF) - REPORTED F30-----------------------------------------
Model11_F26_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F26,RPMptf_F26,BIOptf_F26,HUMptf_F26,FallIOM_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26_MELI) #Loads the model
Ct11_F26_MELI=getC(Model11_F26_MELI) #Calculates stocks for each pool per month
TotalC11_F26_MELI=rowSums(Ct11_F26_MELI)#Calculates total (sum of all pools)  
Rt11_F26_MELI=getReleaseFlux(Model11_F26_MELI)#CO2 released for all pools 

matplot(years, Ct11_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F26_df_MELI <- data.frame(years,as.data.frame(Ct11_F26_MELI),TotalC11_F26_MELI)
names(Ct11_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F26_MELI=tail(Ct11_F26_df_MELI,1)
poolSize11_F26_MELI$Stand_age="30 years"
poolSize11_F26_MELI$Harvest_year=1983
poolSize11_F26_MELI #Reported data

Ct11_long_F26_MELI <- Ct11_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F26_MELI$PSP=26
Ct11_long_F26_MELI$Stand_age=c("30 years")
Ct11_long_F26_MELI$Harvest_year=1983



#### g.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F26_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F26_forest,RPMptf_F26,BIOptf_F26,HUMptf_F26,IOM_FOREST_F26),
                       In=Cinputs_F26_2014_FORESTFLOOR,
                       clay=clay_F26_2014,
                       DR=0.25,
                       xi=xi.frame_F26_MELI) #Loads the model
Ct12_F26_MELI=getC(Model12_F26_MELI) #Calculates stocks for each pool per month
TotalC12_F26_MELI=rowSums(Ct12_F26_MELI)#Calculates total (sum of all pools)  
Rt12_F26_MELI=getReleaseFlux(Model12_F26_MELI)#CO2 released for all pools 

matplot(years, Ct12_F26_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F26_df_MELI <- data.frame(years,as.data.frame(Ct8_F26_MELI),TotalC8_F26_MELI)
names(Ct12_F26_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F26_MELI=tail(Ct12_F26_df_MELI,1)
poolSize12_F26_MELI$Stand_age="30 years"
poolSize12_F26_MELI$Harvest_year=1983
poolSize12_F26_MELI #Reported data

Ct12_long_F26_MELI <- Ct12_F26_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F26_MELI$PSP=26
Ct12_long_F26_MELI$Stand_age=c("30 years")
Ct12_long_F26_MELI$Harvest_year=1983



## 7.4. FOLIO 23 (80 years) ------------------------------------------------------
xi.frame_F23_MELI=data.frame(years,rep(fT_MELI*fW_F23_MELI,length.out=length(years)))

### h) RothC models (LITTERFALL) ------------------------------------------------------------
#### h.1) RothC model 1 (IOM_G & ZERO) -----------------------------------------
Model1_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct1_F23_MELI=getC(Model1_F23_MELI) #Calculates stocks for each pool per month
TotalC1_F23_MELI=rowSums(Ct1_F23_MELI)#Calculates total (sum of all pools)  
Rt1_F23_MELI=getReleaseFlux(Model1_F23_MELI)#CO2 released for all pools 

matplot(years, Ct1_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct1_F23_df_MELI <- data.frame(years,as.data.frame(Ct1_F23_MELI),TotalC1_F23_MELI)
names(Ct1_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize_F23_MELI=tail(Ct1_F23_df_MELI,1)
poolSize_F23_MELI$Stand_age="80 years"
poolSize_F23_MELI$Harvest_year=1933
poolSize_F23_MELI #Reported data

Ct1_long_F23_MELI <- Ct1_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct1_long_F23_MELI$PSP=26
Ct1_long_F23_MELI$Stand_age=c("80 years")
Ct1_long_F23_MELI$Harvest_year=1933


#### h.2) RothC model 2 (IOM_F & ZERO) -----------------------------------------
Model2_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct2_F23_MELI=getC(Model2_F23_MELI) #Calculates stocks for each pool per month
TotalC2_F23_MELI=rowSums(Ct2_F23_MELI)#Calculates total (sum of all pools)  
Rt2_F23_MELI=getReleaseFlux(Model2_F23_MELI)#CO2 released for all pools 

matplot(years, Ct2_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct2_F23_df_MELI <- data.frame(years,as.data.frame(Ct2_F23_MELI),TotalC2_F23_MELI)
names(Ct2_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize2_F23_MELI=tail(Ct2_F23_df_MELI,1)
poolSize2_F23_MELI$Stand_age="80 years"
poolSize2_F23_MELI$Harvest_year=1933
poolSize2_F23_MELI #Reported data

Ct2_long_F23_MELI <- Ct2_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23_MELI$PSP=26
Ct2_long_F23_MELI$Stand_age=c("80 years")
Ct2_long_F23_MELI$Harvest_year=1933



#### h.3) RothC model 3 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model3_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct3_F23_MELI=getC(Model3_F23_MELI) #Calculates stocks for each pool per month
TotalC3_F23_MELI=rowSums(Ct3_F23_MELI)#Calculates total (sum of all pools)  
Rt3_F23_MELI=getReleaseFlux(Model3_F23_MELI)#CO2 released for all pools 

matplot(years, Ct3_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F23_df_MELI <- data.frame(years,as.data.frame(Ct3_F23_MELI),TotalC3_F23_MELI)
names(Ct3_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize3_F23_MELI=tail(Ct3_F23_df_MELI,1)
poolSize3_F23_MELI$Stand_age="80 years"
poolSize3_F23_MELI$Harvest_year=1933
poolSize3_F23_MELI #Reported data

Ct3_long_F23_MELI <- Ct3_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F23_MELI$PSP=26
Ct3_long_F23_MELI$Stand_age=c("80 years")
Ct3_long_F23_MELI$Harvest_year=1933



#### h.4) RothC model 4 (IOM_G & PF) -----------------------------------------
Model4_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFAL,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct4_F23_MELI=getC(Model4_F23_MELI) #Calculates stocks for each pool per month
TotalC4_F23_MELI=rowSums(Ct4_F23_MELI)#Calculates total (sum of all pools)  
Rt4_F23_MELI=getReleaseFlux(Model4_F23_MELI)#CO2 released for all pools 

matplot(years, Ct4_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct4_F23_df_MELI <- data.frame(years,as.data.frame(Ct4_F23_MELI),TotalC4_F23_MELI)
names(Ct4_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize4_F23_MELI=tail(Ct4_F23_df_MELI,1)
poolSize4_F23_MELI$Stand_age="80 years"
poolSize4_F23_MELI$Harvest_year=1933
poolSize4_F23_MELI #Reported data

Ct4_long_F23_MELI <- Ct4_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct4_long_F23_MELI$PSP=26
Ct4_long_F23_MELI$Stand_age=c("80 years")
Ct4_long_F23_MELI$Harvest_year=1933



### i) RothC models (LITTERFALL+ROOT TURNOVER) ------------------------------------------------------------
#### i.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model5_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct5_F23_MELI=getC(Model5_F23_MELI) #Calculates stocks for each pool per month
TotalC5_F23_MELI=rowSums(Ct5_F23_MELI)#Calculates total (sum of all pools)  
Rt5_F23_MELI=getReleaseFlux(Model5_F23_MELI)#CO2 released for all pools 

matplot(years, Ct5_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct5_F23_df_MELI <- data.frame(years,as.data.frame(Ct5_F23_MELI),TotalC5_F23_MELI)
names(Ct5_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize5_F23_MELI=tail(Ct5_F23_df_MELI,1)
poolSize5_F23_MELI$Stand_age="80 years"
poolSize5_F23_MELI$Harvest_year=1933
poolSize5_F23_MELI #Reported data

Ct5_long_F23_MELI <- Ct5_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct5_long_F23_MELI$PSP=26
Ct5_long_F23_MELI$Stand_age=c("80 years")
Ct5_long_F23_MELI$Harvest_year=1933


#### i.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model6_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct6_F23_MELI=getC(Model6_F23_MELI) #Calculates stocks for each pool per month
TotalC6_F23_MELI=rowSums(Ct6_F23_MELI)#Calculates total (sum of all pools)  
Rt6_F23_MELI=getReleaseFlux(Model6_F23_MELI)#CO2 released for all pools 

matplot(years, Ct6_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct6_F23_df_MELI <- data.frame(years,as.data.frame(Ct6_F23_MELI),TotalC6_F23_MELI)
names(Ct6_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize6_F23_MELI=tail(Ct6_F23_df_MELI,1)
poolSize6_F23_MELI$Stand_age="80 years"
poolSize6_F23_MELI$Harvest_year=1933
poolSize6_F23_MELI #Reported data

Ct6_long_F23_MELI <- Ct6_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct6_long_F23_MELI$PSP=26
Ct6_long_F23_MELI$Stand_age=c("80 years")
Ct6_long_F23_MELI$Harvest_year=1933



#### i.3) RothC model 7 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model7_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct7_F23_MELI=getC(Model7_F23_MELI) #Calculates stocks for each pool per month
TotalC7_F23_MELI=rowSums(Ct7_F23_MELI)#Calculates total (sum of all pools)  
Rt7_F23_MELI=getReleaseFlux(Model7_F23_MELI)#CO2 released for all pools 

matplot(years, Ct7_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct7_F23_df_MELI <- data.frame(years,as.data.frame(Ct7_F23_MELI),TotalC7_F23_MELI)
names(Ct7_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize7_F23_MELI=tail(Ct7_F23_df_MELI,1)
poolSize7_F23_MELI$Stand_age="80 years"
poolSize7_F23_MELI$Harvest_year=1933
poolSize7_F23_MELI #Reported data

Ct7_long_F23_MELI <- Ct7_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct7_long_F23_MELI$PSP=26
Ct7_long_F23_MELI$Stand_age=c("80 years")
Ct7_long_F23_MELI$Harvest_year=1933



#### i.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model8_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                      In=Cinputs_F23_2014_LITTERFALL_ROOT,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct8_F23_MELI=getC(Model8_F23_MELI) #Calculates stocks for each pool per month
TotalC8_F23_MELI=rowSums(Ct8_F23_MELI)#Calculates total (sum of all pools)  
Rt8_F23_MELI=getReleaseFlux(Model8_F23_MELI)#CO2 released for all pools 

matplot(years, Ct8_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct8_F23_df_MELI <- data.frame(years,as.data.frame(Ct8_F23_MELI),TotalC8_F23_MELI)
names(Ct8_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize8_F23_MELI=tail(Ct8_F23_df_MELI,1)
poolSize8_F23_MELI$Stand_age="80 years"
poolSize8_F23_MELI$Harvest_year=1933
poolSize8_F23_MELI #Reported data

Ct8_long_F23_MELI <- Ct8_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct8_long_F23_MELI$PSP=26
Ct8_long_F23_MELI$Stand_age=c("80 years")
Ct8_long_F23_MELI$Harvest_year=1933



### j) RothC models (FOREST FLOOR) ------------------------------------------------------------
#### j.1) RothC model 5 (IOM_G & ZERO) -----------------------------------------
Model9_F23_MELI=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014_FORESTFLOOR,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23_MELI) #Loads the model
Ct9_F23_MELI=getC(Model9_F23_MELI) #Calculates stocks for each pool per month
TotalC9_F23_MELI=rowSums(Ct9_F23_MELI)#Calculates total (sum of all pools)  
Rt9_F23=getReleaseFlux(Model9_F23_MELI)#CO2 released for all pools 

matplot(years, Ct9_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct9_F23_df_MELI <- data.frame(years,as.data.frame(Ct9_F23_MELI),TotalC9_F23_MELI)
names(Ct9_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize9_F23_MELI=tail(Ct9_F23_df_MELI,1)
poolSize9_F23_MELI$Stand_age="80 years"
poolSize9_F23_MELI$Harvest_year=1933
poolSize9_F23_MELI #Reported data

Ct9_long_F23_MELI <- Ct9_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct9_long_F23_MELI$PSP=26
Ct9_long_F23_MELI$Stand_age=c("80 years")
Ct9_long_F23_MELI$Harvest_year=1933


#### j.2) RothC model 6 (IOM_F & ZERO) -----------------------------------------
Model10_F23_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23_MELI) #Loads the model
Ct10_F23_MELI=getC(Model10_F23_MELI) #Calculates stocks for each pool per month
TotalC10_F23_MELI=rowSums(Ct10_F23_MELI)#Calculates total (sum of all pools)  
Rt10_F23_MELI=getReleaseFlux(Model10_F23_MELI)#CO2 released for all pools 

matplot(years, Ct10_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct10_F23_df_MELI <- data.frame(years,as.data.frame(Ct10_F23_MELI),TotalC10_F23_MELI)
names(Ct10_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10_F23_MELI=tail(Ct10_F23_df_MELI,1)
poolSize10_F23_MELI$Stand_age="80 years"
poolSize10_F23_MELI$Harvest_year=1933
poolSize10_F23_MELI #Reported data

Ct10_long_F23_MELI <- Ct10_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct10_long_F23_MELI$PSP=26
Ct10_long_F23_MELI$Stand_age=c("80 years")
Ct10_long_F23_MELI$Harvest_year=1933



#### j.3) RothC model 7 (IOM_G & PF) - REPORTED F23-----------------------------------------
Model11_F23_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F23,RPMptf_F23,BIOptf_F23,HUMptf_F23,FallIOM_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23_MELI) #Loads the model
Ct11_F23_MELI=getC(Model11_F23_MELI) #Calculates stocks for each pool per month
TotalC11_F23_MELI=rowSums(Ct11_F23_MELI)#Calculates total (sum of all pools)  
Rt11_F23_MELI=getReleaseFlux(Model11_F23_MELI)#CO2 released for all pools 

matplot(years, Ct11_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct11_F23_df_MELI <- data.frame(years,as.data.frame(Ct11_F23_MELI),TotalC11_F23_MELI)
names(Ct11_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11_F23_MELI=tail(Ct11_F23_df_MELI,1)
poolSize11_F23_MELI$Stand_age="80 years"
poolSize11_F23_MELI$Harvest_year=1933
poolSize11_F23_MELI #Reported data

Ct11_long_F23_MELI <- Ct11_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct11_long_F23_MELI$PSP=26
Ct11_long_F23_MELI$Stand_age=c("80 years")
Ct11_long_F23_MELI$Harvest_year=1933



#### j.4) RothC model 8 (IOM_G & PF) -----------------------------------------
Model12_F23_MELI=RothCModel(t=years,
                       ks=c(10,0.3,0.66,0.02,0),
                       C0=c(DPMptf_F23_forest,RPMptf_F23,BIOptf_F23,HUMptf_F23,IOM_FOREST_F23),
                       In=Cinputs_F23_2014_FORESTFLOOR,
                       clay=clay_F23_2014,
                       DR=0.25,
                       xi=xi.frame_F23_MELI) #Loads the model
Ct12_F23_MELI=getC(Model12_F23_MELI) #Calculates stocks for each pool per month
TotalC12_F23_MELI=rowSums(Ct12_F23_MELI)#Calculates total (sum of all pools)  
Rt12_F23_MELI=getReleaseFlux(Model12_F23_MELI)#CO2 released for all pools 

matplot(years, Ct12_F23_MELI, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct12_F23_df_MELI <- data.frame(years,as.data.frame(Ct8_F23_MELI),TotalC8_F23_MELI)
names(Ct12_F23_df_MELI)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize12_F23_MELI=tail(Ct12_F23_df_MELI,1)
poolSize12_F23_MELI$Stand_age="80 years"
poolSize12_F23_MELI$Harvest_year=1933
poolSize12_F23_MELI #Reported data

Ct12_long_F23_MELI <- Ct12_F23_df_MELI %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct12_long_F23_MELI$PSP=26
Ct12_long_F23_MELI$Stand_age=c("80 years")
Ct12_long_F23_MELI$Harvest_year=1933


# 8. GRAFICAS DE TODOS LOS MODELOS ---------------------------------------------
## 8.1. Model 1: Litterfall ----------------------------------------------------
Model1_outputs=rbind(data.frame(Ct3_long_F38,Condition="Managed"),
                     data.frame(Ct3_long_F32,Condition="Managed"),
                     data.frame(Ct3_long_F26,Condition="Managed"),
                     data.frame(Ct3_long_F23,Condition="Unmanaged"))
Model1_outputs <- Model1_outputs %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model1_outputs$SOC <- ifelse(Model1_outputs$SOC < 0, 0, Model1_outputs$SOC)
Model1_outputs$modeledtime=Model1_outputs$Time+2014
Model1_outputs$modelname="Model 1: Litterfall"
View(Model1_outputs)

Mgha_expression=expression("SOC (Mg ha"^"-1"~")")

Modelo1=ggplot(Model1_outputs, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(data = Model1_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(title = "a) Model 1: Litterfall",
       x = "Time (years)",
       color = "Stand age:",
  ) +
  ylab(Mgha_expression)+
  scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
  facet_wrap(~reorder(Fraction,ORDER_FRACTION),scales = "free_y",ncol = 3,nrow = 2)+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )



jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Modelo1.jpeg"),
     width = 190,height = 180,units = "mm",res = 1000)
Modelo1
dev.off()


## 8.2. Model 2: Litterfall & Root turnover -----------------------------------
Model2_outputs=rbind(data.frame(Ct7_long_F38,Condition="Managed"),
                     data.frame(Ct7_long_F32,Condition="Managed"),
                     data.frame(Ct7_long_F26,Condition="Managed"),
                     data.frame(Ct7_long_F23,Condition="Unmanaged"))


Model2_outputs <- Model2_outputs %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model2_outputs$SOC <- ifelse(Model2_outputs$SOC < 0, 0, Model2_outputs$SOC)
Model2_outputs$modeledtime=Model2_outputs$Time+2014
Model2_outputs$modelname="Model 2: Litterfall + root turnover"
View(Model2_outputs)

Modelo2=ggplot(Model2_outputs, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model2_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(data = Model2_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(title = "b) Model 2: Litterfall + root turnover",
       x = "Time (years)",
       color = "Stand age:") +
  ylab(Mgha_expression)+
  scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
  facet_wrap(~reorder(Fraction,ORDER_FRACTION),scales = "free_y",ncol = 2,nrow = 3)+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )



jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Modelo2.jpeg"),
     width = 190,height = 180,units = "mm",res = 1000)
Modelo2
dev.off()

## 8.3. Model 3: Forest floor --------------------------------------------------

Model3_outputs=rbind(data.frame(Ct11_long_F38,Condition="Managed"),
                     data.frame(Ct11_long_F32,Condition="Managed"),
                     data.frame(Ct11_long_F26,Condition="Managed"),
                     data.frame(Ct11_long_F23,Condition="Unmanaged"))

Model3_outputs <- Model3_outputs %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model3_outputs$SOC <- ifelse(Model3_outputs$SOC < 0, 0, Model3_outputs$SOC)
Model3_outputs$modeledtime=Model3_outputs$Time+2014
Model3_outputs$modelname="Model 3: Forest floor"
View(Model3_outputs)


Modelo3=ggplot(Model3_outputs, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model3_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(data = Model3_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(title = "b) Model 3: Forest floor",
       x = "Time (years)",
       color = "Stand age:") +
  ylab(Mgha_expression)+
  scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
  facet_wrap(~reorder(Fraction,ORDER_FRACTION),scales = "free_y",ncol = 2,nrow = 3)+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )



jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Modelo3.jpeg"),
     width = 190,height = 180,units = "mm",res = 1000)
Modelo3
dev.off()



## 8.4. All models -------------------------------------------------------------

#In different plots
jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "All models.jpeg"),
     width = 190,height = 220,units = "mm",res = 1000)
ggarrange(Modelo1,
          Modelo2,
          Modelo3,
          ncol = 1,nrow = 3,
          common.legend = TRUE,
          legend = "bottom")
dev.off()

#In the same plot
allmodels=rbind(Model1_outputs,
                Model2_outputs)

litterfall_models=ggplot(allmodels, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = modelname)) +
  geom_line(size=0.7,data = allmodels %>% filter(Fraction== "IOM"), )+
  geom_smooth(data = allmodels %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color =Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(
    x = "Time (years)",
    color= "Stand age:",
     linetype = "Model:"
  ) +
  ylab(Mgha_expression)+
  facet_wrap(~reorder(Fraction, ORDER_FRACTION), scales = "free_y",nrow = 3,ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
  #scale_color_manual(values = c("red","blue"))+
  #scale_linetype_manual(values = c(1,2,3,4))+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 2, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 3,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

forestfloor_models=ggplot(Model3_outputs, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = modelname)) +
  geom_line(size=0.7,data = Model3_outputs %>% filter(Fraction== "IOM"), )+
  geom_smooth(data = Model3_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color =Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(
    x = "Time (years)",
    color= "Stand age:",
    linetype = "Model:"
  ) +
  ylab(Mgha_expression)+
  facet_wrap(~reorder(Fraction, ORDER_FRACTION), scales = "free_y",nrow = 3,ncol = 2) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +  
  #scale_color_manual(values = c("red","blue"))+
  #scale_linetype_manual(values = c(1,2,3,4))+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 2, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 3,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

 



jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "allmodels_plot.jpeg"),
     width = 190,height = 220,units = "mm",res = 1000)
allmodels_plot
dev.off()


# 9. SEPARATE PLOTS - REPORTED ------------------------------------------------------------
## 9.1. Modelo 1 ---------------------------------------------------------------
DPM_M1 <- Model1_outputs %>% filter(Fraction == "DPM")
RPM_M1 <- Model1_outputs %>% filter(Fraction == "RPM")
BIO_M1 <- Model1_outputs %>% filter(Fraction == "BIO")
HUM_M1 <- Model1_outputs %>% filter(Fraction == "HUM")
IOM_M1 <- Model1_outputs %>% filter(Fraction == "IOM")

DPM_M1_PLOT=ggplot(DPM_M1, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("Model 1","DPM")+
  labs(x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  #scale_x_continuous(limits = c(2014,2114),breaks = c(2014,2040,2065,2090,2114)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
DPM_M1_PLOT


RPM_M1_PLOT=ggplot(RPM_M1, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "RPM",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,40),breaks = pretty_breaks(n = 6)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
RPM_M1_PLOT


BIO_M1_PLOT=ggplot(BIO_M1, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "BIO",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,3.0),breaks = pretty_breaks(n = 4)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
BIO_M1_PLOT

HUM_M1_PLOT=ggplot(HUM_M1, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "HUM",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(20,140),breaks = c(20,60,100,140)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
HUM_M1_PLOT


IOM_M1_PLOT=ggplot(IOM_M1, aes(x = Time, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  #geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "IOM",
       x = "",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(8,20),breaks = c(8,12,16,20)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        axis.title.y =  element_blank(),
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
IOM_M1_PLOT

## 9.2. Modelo 2 ---------------------------------------------------------------
DPM_M2 <- Model2_outputs %>% filter(Fraction == "DPM")
RPM_M2 <- Model2_outputs %>% filter(Fraction == "RPM")
BIO_M2 <- Model2_outputs %>% filter(Fraction == "BIO")
HUM_M2 <- Model2_outputs %>% filter(Fraction == "HUM")
IOM_M2 <- Model2_outputs %>% filter(Fraction == "IOM")

DPM_M2_PLOT=ggplot(DPM_M2, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(title = "Model 2",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
DPM_M2_PLOT


RPM_M2_PLOT=ggplot(RPM_M2, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,40),breaks = pretty_breaks(n = 6)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
RPM_M2_PLOT

BIO_M2_PLOT=ggplot(BIO_M2, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,3.0),breaks = pretty_breaks(n = 4)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
BIO_M2_PLOT


HUM_M2_PLOT=ggplot(HUM_M2, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(20,140), breaks = c(20,60,100,140)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
HUM_M2_PLOT

IOM_M2_PLOT=ggplot(IOM_M2, aes(x = Time, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model2_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  #geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Projections (equilibrium assumption)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(8,20),breaks = c(8,12,16,20)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y =  element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
IOM_M2_PLOT

## 9.3. Modelo 3 ---------------------------------------------------------------
DPM_M3 <- Model3_outputs %>% filter(Fraction == "DPM")
RPM_M3 <- Model3_outputs %>% filter(Fraction == "RPM")
BIO_M3 <- Model3_outputs %>% filter(Fraction == "BIO")
HUM_M3 <- Model3_outputs %>% filter(Fraction == "HUM")
IOM_M3 <- Model3_outputs %>% filter(Fraction == "IOM")

DPM_M3_PLOT=ggplot(DPM_M3, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(title = "Model 3",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
DPM_M3_PLOT

RPM_M3_PLOT=ggplot(RPM_M3, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,40),breaks = pretty_breaks(n = 6)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
RPM_M3_PLOT

BIO_M3_PLOT=ggplot(BIO_M3, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(0,3.0),breaks = pretty_breaks(n = 4)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
BIO_M3_PLOT


HUM_M3_PLOT=ggplot(HUM_M3, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  #geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(20,140),breaks = c(20,60,100,140)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
HUM_M3_PLOT


IOM_M3_PLOT=ggplot(IOM_M3, aes(x = Time, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_line(data = Model3_outputs %>% filter(Fraction== "IOM"), size=0.7)+
  #geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  labs(#title = "",
       subtitle = "",
       x = "",
       color = "Stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(8,20),breaks = c(8,12,16,20)) +  
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        #axis.text.y = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y =  element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
IOM_M3_PLOT

## 9.4. JOIN PLOTS ------------------------------------------------------------

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Fractions_plot.jpeg"),
     width = 190,height = 230,units = "mm",res = 1000)

ALLFRACTIONS=ggarrange(DPM_M1_PLOT,DPM_M2_PLOT,DPM_M3_PLOT,
          RPM_M1_PLOT,RPM_M2_PLOT,RPM_M3_PLOT,
          BIO_M1_PLOT,BIO_M2_PLOT,BIO_M3_PLOT,
          HUM_M1_PLOT,HUM_M2_PLOT,HUM_M3_PLOT,
          IOM_M1_PLOT,IOM_M2_PLOT,IOM_M3_PLOT,
          ncol = 3,nrow = 5,
          common.legend = TRUE,
          legend = "bottom")
annotate_figure(ALLFRACTIONS, 
                left = textGrob(Mgha_expression, rot = 90, gp = gpar(cex = 1,fontfamily="A")))
dev.off()


# 10. SOC TOTAL ----------------------------------------------------------------
finalmodels=rbind(Model1_outputs,
Model2_outputs,
Model3_outputs)
Mgha_expression_total=expression("Total SOC (Mg ha"^"-1"~")")

SOC_ALLMODELS <- finalmodels %>% filter(Fraction == "TotalSOC")

View(SOC_ALLMODELS)
Observed_SOC=data.frame(Measured_SOC=c(129.6895,151.69147,187.6272,98.3563,126.3739497,159.3813843,170.1253768,120.7096722,119.3739497,159.3813843,148.1253768,133.7096722),
                        Condition=c("Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged"),
                        modelname=c("Model 1: Litterfall"),
                        modeledtime=c(2014,2014,2014,2014,2019,2019,2019,2019,2024,2024,2024,2024),
                        Stand_age=c("07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years"))

#Duplicar para que aparezca en todos los paneles
Observed_SOC2=data.frame(Measured_SOC=c(129.6895,151.69147,187.6272,98.3563,126.3739497,159.3813843,170.1253768,120.7096722,119.3739497,159.3813843,148.1253768,133.7096722),
                         Condition=c("Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged"),
                         modelname=c("Model 2: Litterfall + root turnover"),
                         modeledtime=c(2014,2014,2014,2014,2019,2019,2019,2019,2024,2024,2024,2024),
                         Stand_age=c("07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years"))

Observed_SOC3=data.frame(Measured_SOC=c(129.6895,151.69147,187.6272,98.3563,126.3739497,159.3813843,170.1253768,120.7096722,119.3739497,159.3813843,148.1253768,133.7096722),
                         Condition=c("Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged","Managed","Managed","Managed","Unmanaged"),
                         modelname=c("Model 3: Forest floor"),
                         modeledtime=c(2014,2014,2014,2014,2019,2019,2019,2019,2024,2024,2024,2024),
                         Stand_age=c("07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years"))

Observed_SOC_plot=rbind(Observed_SOC,Observed_SOC2,Observed_SOC3)
Observed_SOC_plot$Stand_age <- factor(Observed_SOC_plot$Stand_age)


SOC_plot=ggplot(SOC_ALLMODELS, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Condition)) +
  geom_point(data = Observed_SOC_plot, 
             aes(x = modeledtime, y = Measured_SOC, color = Stand_age, shape = Stand_age), 
             size = 2) +
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.5) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 200, by = 20))+
  scale_x_continuous(labels = c(0,25,50,75,100))+
  scale_linetype_manual(values = c(1,2))+
  scale_shape_manual(values = c("07 years" = 15, "18 years" = 16, "30 years" = 17, "80 years" = 9)) +
  labs(
    x = "Projections (years; spin-up phase)",
    color = "Stand age:",
    linetype = "Condition:"
  ) +
  ylab(Mgha_expression_total) +
  facet_wrap(~modelname,nrow = 3,
             labeller = labeller(modelname = c(
               "Model 1: Litterfall" = "Model 1: Litterfall ",
               "Model 2: Litterfall + root turnover" = "Model 2: Litterfall + Fine root turnover",
               "Model 3: Forest floor" = "Model 3: Forest floor "
             )))+
  theme(text = element_text(size=11,family = "A"),
        strip.text = element_text(size = 10, face = "bold",hjust = 0),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black')) +
  guides(
    shape = "none",
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0),
    color = guide_legend(order = 1,title.position = "top",nrow = 1,override.aes = list(shape = c(15, 16, 17, 9))), # Stand age en una fila
  )
SOC_plot



jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "SOC_plot.jpeg"),
     width = 190,height = 230,units = "mm",res = 1000)
SOC_plot
dev.off()

# 11. LITTERFALL VARIATION ------------------------------------------
Litterfall_4stands <- read_excel("Litterfall_4stands.xlsx")
names(Litterfall_4stands)
Litterfall_4stands$Stand_age=as.factor(Litterfall_4stands$Stand_age)

Litterfall_4stands <- Litterfall_4stands %>%
  mutate(
    Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    YearMonth = paste(Year, Month, sep = "-"), # Crear columna Año-Mes
    Year = as.factor(Year) # Asegurarse de que los años estén como factor
  )

Mgha2_expression=expression("Mg ha"^"-1")

# Calcular la variación promedio mensual y la desviación estándar
Litterfall_monthly_stats <- Litterfall_4stands %>%
  group_by(Month,Stand_age) %>%
  summarise(
    Avg_Litterfall = mean(Litterfall, na.rm = TRUE),
    SD_Litterfall = sd(Litterfall, na.rm = TRUE)
  )

# Graficar la variación promedio mensual con desviación estándar por Stand_age
Litterfall_month=ggplot(Litterfall_monthly_stats, aes(x = Month, y = Avg_Litterfall, color = Stand_age, group = Stand_age)) +
  geom_line(size = 0.7) + # Línea de la media por Stand_age
  #geom_point(size = 3) + # Puntos de la media por Stand_age
  geom_errorbar(
    aes(ymin = Avg_Litterfall - SD_Litterfall, ymax = Avg_Litterfall + SD_Litterfall), 
    width = 0.1, size = 0.5
  ) + # Líneas de la desviación estándar
  labs(
    title = "a) Monthly variation (2014-2020)",
    x = "Month",
    y = Mgha2_expression,
    color = "Stand age:"
  ) +
  theme_bw() +
  scale_x_discrete(
    limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), # Asegura que los meses se muestren en orden
    breaks = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    #labels = c("Jan", "F", "M", "A", "M", "J","J", "A", "S", "O", "N", "D")
  )+
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=11,family = "A"),
        axis.text.x = element_text(hjust = 0.5), # Inclinar etiquetas del eje X
        legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.text = element_text(size = 11),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 11,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour='black'))
Litterfall_month



Litterfall_20142020=ggplot(Litterfall_4stands, aes(x = YearMonth, y = Litterfall, color = Stand_age, group = Stand_age)) +
  geom_line(size = 0.5) +
  labs(
    title = "b) Annual variation (2014 - 2020)",
    x = "Year",
    y = Mgha2_expression,
    color = "Stand age:"
  ) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=11,family = "A"),
        axis.text.x = element_text(hjust = 0.5), # Inclinar etiquetas del eje X
        legend.position = "bottom",
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.text = element_text(size = 11),
        legend.box = "horizontal",
        axis.title.y = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 11,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour='black'))+
  scale_x_discrete(
    breaks = Litterfall_4stands$YearMonth[seq(1, nrow(Litterfall_4stands), by = 12)], # Mostrar solo los años
    labels = Litterfall_4stands$Year[seq(1, nrow(Litterfall_4stands), by = 12)] # Etiquetas con los años
  )
 
Litterfall_20142020

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Litterfall_plot.jpeg"),
     width = 190,height = 180,units = "mm",res = 1000)

litterfallvariation=ggarrange(Litterfall_month,
                       Litterfall_20142020,
                       ncol = 1,nrow = 2,
                       common.legend = TRUE,
                       legend = "bottom")
litterfallvariation
annotate_figure(litterfallvariation, 
                left = textGrob(Mgha_expression3, rot = 90, gp = gpar(cex = 1,fontfamily="A")))
dev.off()


# 12. C INPUTS ----------------------------------------------------------------
root_turnover=c(9.349,13.766,10.19,13.819)
anual_litterfall=c(3.820560476,5.727428571,7.334840476,6.519864762)
forest_floor=c(6.536927209,8.80750066,6.940901009,7.643989298)
Stand_age=c("07 years","18 years","30 years","80 years")

Cinputs_frame=data.frame(Stand_age,anual_litterfall,root_turnover,forest_floor)

# Convertir el dataframe a formato largo para ggplot
Cinputs_long <- reshape2::melt(Cinputs_frame, id.vars = "Stand_age", variable.name = "Variable", value.name = "Mg_per_ha")

Mgha_expression3=expression("Mg ha"^"-1")

# Crear el gráfico de barras acumuladas
Cinputs_barplot=ggplot(Cinputs_long, aes(x = as.factor(Stand_age), y = Mg_per_ha, fill = Variable)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Plant residues:"
    #title = ""
  ) +
  scale_y_continuous(limits = c(0,14),breaks = c(0,2,4,6,8,10,12,14))+
  scale_fill_manual(values = c("#3E5879", "#D39D55","#A66E38"),
                    labels = c(
                      "root_turnover" = "Fine root turnover", 
                      "anual_litterfall" = "Litterfall",
                      "forest_floor"="Forest floor")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
Cinputs_barplot


jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "Cinputs.jpeg"),
     width = 140,height = 90,units = "mm",res = 1000)
Cinputs_barplot
dev.off()

# 13. BARPLOT FRACTIONS ---------------------------------------
## Data 1990 - 2020 ------------------------------------

#Model1
model1_finalfractions=rbind(poolSize3_F38,poolSize3_F32,poolSize3_F26,poolSize3_F23)
model1_finalfractions$Model="Model 1"
model1_finalfractions$data="SMN (1990-2020)"
#Model2
model2_finalfractions=rbind(poolSize7_F38,poolSize7_F32,poolSize7_F26,poolSize7_F23)
model2_finalfractions$Model="Model 2"
model2_finalfractions$data="SMN (1990-2020)"

#Model3
model3_finalfractions=rbind(poolSize11_F38,poolSize11_F32,poolSize11_F26,poolSize11_F23)
model3_finalfractions$Model="Model 3"
model3_finalfractions$data="SMN (1990-2020)"

## Data 2017 - 2020  ------------------------------------

#Model1
model1_finalfractions_MELI=rbind(poolSize3_F38_MELI,poolSize3_F32_MELI,poolSize3_F26_MELI,poolSize3_F23_MELI)
model1_finalfractions_MELI$Model="Model 1"
model1_finalfractions_MELI$data="Field data (2017-2020)"
#Model2
model2_finalfractions_MELI=rbind(poolSize7_F38_MELI,poolSize7_F32_MELI,poolSize7_F26_MELI,poolSize7_F23_MELI)
model2_finalfractions_MELI$Model="Model 2"
model2_finalfractions_MELI$data="Field data (2017-2020)"

#Model3
model3_finalfractions_MELI=rbind(poolSize11_F38_MELI,poolSize11_F32_MELI,poolSize11_F26_MELI,poolSize11_F23_MELI)
model3_finalfractions_MELI$Model="Model 3"
model3_finalfractions_MELI$data="Field data (2017-2020)"

## Plots -------------------
fractions_bydata_M1=rbind(model1_finalfractions,model1_finalfractions_MELI)
fractions_bydata_M2=rbind(model2_finalfractions,model2_finalfractions_MELI)
fractions_bydata_M3=rbind(model3_finalfractions,model3_finalfractions_MELI)


fractions_bydata_M1 <- fractions_bydata_M1 %>%
  mutate(Stand_age = ifelse(Stand_age == "07 years", "07 y", Stand_age))
fractions_bydata_M1 <- fractions_bydata_M1 %>%
  mutate(Stand_age = ifelse(Stand_age == "18 years", "18 y", Stand_age))
fractions_bydata_M1 <- fractions_bydata_M1 %>%
  mutate(Stand_age = ifelse(Stand_age == "30 years", "30 y", Stand_age))
fractions_bydata_M1 <- fractions_bydata_M1 %>%
  mutate(Stand_age = ifelse(Stand_age == "80 years", "80 y", Stand_age))


fractions_bydata_M2 <- fractions_bydata_M2 %>%
  mutate(Stand_age = ifelse(Stand_age == "07 years", "07 y", Stand_age))
fractions_bydata_M2 <- fractions_bydata_M2 %>%
  mutate(Stand_age = ifelse(Stand_age == "18 years", "18 y", Stand_age))
fractions_bydata_M2 <- fractions_bydata_M2 %>%
  mutate(Stand_age = ifelse(Stand_age == "30 years", "30 y", Stand_age))
fractions_bydata_M2 <- fractions_bydata_M2 %>%
  mutate(Stand_age = ifelse(Stand_age == "80 years", "80 y", Stand_age))

fractions_bydata_M3 <- fractions_bydata_M3 %>%
  mutate(Stand_age = ifelse(Stand_age == "07 years", "07 y", Stand_age))
fractions_bydata_M3 <- fractions_bydata_M3 %>%
  mutate(Stand_age = ifelse(Stand_age == "18 years", "18 y", Stand_age))
fractions_bydata_M3 <- fractions_bydata_M3 %>%
  mutate(Stand_age = ifelse(Stand_age == "30 years", "30 y", Stand_age))
fractions_bydata_M3 <- fractions_bydata_M3 %>%
  mutate(Stand_age = ifelse(Stand_age == "80 years", "80 y", Stand_age))

DPM_M1_BARPLOT=ggplot(fractions_bydata_M1, aes(x = as.factor(Stand_age), y = DPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    title = "Model 1",
    subtitle = "DPM"
    
  ) +
  scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
DPM_M1_BARPLOT
 
DPM_M2_BARPLOT=ggplot(fractions_bydata_M2, aes(x = as.factor(Stand_age), y = DPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    title = "Model 2",
    subtitle = ""
    
  ) +
  scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6))+
  scale_fill_manual(values =c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
DPM_M2_BARPLOT

DPM_M3_BARPLOT=ggplot(fractions_bydata_M3, aes(x = as.factor(Stand_age), y = DPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    title = "Model 3",
    subtitle = ""
    
  ) +
  scale_y_continuous(limits = c(0,0.6),breaks = c(0,0.2,0.4,0.6))+
  scale_fill_manual(values = c("#3E5879", "#D39D55","#A66E38"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
DPM_M3_BARPLOT


RPM_M1_BARPLOT=ggplot(fractions_bydata_M1, aes(x = as.factor(Stand_age), y = RPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = "RPM"
  ) +
  scale_y_continuous(limits = c(0,60),breaks = c(0,15,30,45,60))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.ticks = element_line(size = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
RPM_M1_BARPLOT

RPM_M2_BARPLOT=ggplot(fractions_bydata_M2, aes(x = as.factor(Stand_age), y = RPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0,60),breaks = c(0,15,30,45,60))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
RPM_M2_BARPLOT

RPM_M3_BARPLOT=ggplot(fractions_bydata_M3, aes(x = as.factor(Stand_age), y = RPM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
    
  ) +
  scale_y_continuous(limits = c(0,60),breaks = c(0,15,30,45,60))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
RPM_M3_BARPLOT


BIO_M1_BARPLOT=ggplot(fractions_bydata_M1, aes(x = as.factor(Stand_age), y = BIO, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = "BIO") +
  scale_y_continuous(limits = c(0,5),breaks = c(0,1,2,3,4,5))+
  scale_fill_manual(values =c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.ticks = element_line(size = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
BIO_M1_BARPLOT

BIO_M2_BARPLOT=ggplot(fractions_bydata_M2, aes(x = as.factor(Stand_age), y = BIO, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0,5),breaks = c(0,1,2,3,4,5))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
BIO_M2_BARPLOT

BIO_M3_BARPLOT=ggplot(fractions_bydata_M3, aes(x = as.factor(Stand_age), y = BIO, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
    
  ) +
  scale_y_continuous(limits = c(0,5),breaks = c(0,1,2,3,4,5))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
BIO_M3_BARPLOT


HUM_M1_BARPLOT=ggplot(fractions_bydata_M1, aes(x = as.factor(Stand_age), y = HUM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = "HUM") +
  scale_y_continuous(limits = c(0,170),breaks = c(0,50,100,150))+
  scale_fill_manual(values =c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=10,family = "A"),
        axis.ticks = element_line(size = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
HUM_M1_BARPLOT

HUM_M2_BARPLOT=ggplot(fractions_bydata_M2, aes(x = as.factor(Stand_age), y = HUM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "Stand age",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0,170),breaks = c(0,50,100,150))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
HUM_M2_BARPLOT

HUM_M3_BARPLOT=ggplot(fractions_bydata_M3, aes(x = as.factor(Stand_age), y = HUM, fill = data)) +
  geom_bar(stat = "identity",width = 0.4,position=position_dodge()) +
  labs(
    x = "",
    y = Mgha_expression3,
    fill = "Data:",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0,170),breaks = c(0,50,100,150))+
  scale_fill_manual(values = c("#3E5879", "#FF8225"),
                    labels = c(
                      "Field data (2017-2020)" = "Field data (2017-2021)", 
                      "SMN (1990-2020)" = "SMN (1990-2020)"))+
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_line(size = 0.5),
        text = element_text(size=10,family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 10,face = "bold",family = "A"),
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0.5,face = "bold"),
        plot.subtitle = element_text(size = 12,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
HUM_M3_BARPLOT


jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "FractionsBYWEATHER_plot.jpeg"),
     width = 190,height = 180,units = "mm",res = 1000)

ALLFRACTIONS_WEATHER=ggarrange(DPM_M1_BARPLOT,DPM_M2_BARPLOT,DPM_M3_BARPLOT,
                               RPM_M1_BARPLOT,RPM_M2_BARPLOT,RPM_M3_BARPLOT,
                               BIO_M1_BARPLOT,BIO_M2_BARPLOT,BIO_M3_BARPLOT,
                               HUM_M1_BARPLOT,HUM_M2_BARPLOT,HUM_M3_BARPLOT,
                               ncol = 3,nrow = 4,common.legend = TRUE,legend = "bottom")
ALLFRACTIONS_WEATHER
annotate_figure(ALLFRACTIONS_WEATHER, 
                left = textGrob(Mgha_expression, rot = 90, gp = gpar(cex = 1,fontfamily="A")))
dev.off()


# 14. SOC TOTAL (BY WEATHER DATA) () ----------------------------------------------

Model1_outputs_MELI=rbind(data.frame(Ct3_long_F38_MELI,Condition="Managed"),
                     data.frame(Ct3_long_F32_MELI,Condition="Managed"),
                     data.frame(Ct3_long_F26_MELI,Condition="Managed"),
                     data.frame(Ct3_long_F23_MELI,Condition="Unmanaged"))
Model1_outputs_MELI <- Model1_outputs_MELI %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model1_outputs_MELI$SOC <- ifelse(Model1_outputs_MELI$SOC < 0, 0, Model1_outputs_MELI$SOC)
Model1_outputs_MELI$modeledtime=Model1_outputs_MELI$Time+2014
Model1_outputs_MELI$modelname="Model 1: Litterfall"
View(Model1_outputs_MELI)


Model2_outputs_MELI=rbind(data.frame(Ct3_long_F38_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F32_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F26_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F23_MELI,Condition="Unmanaged"))
Model2_outputs_MELI <- Model2_outputs_MELI %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model2_outputs_MELI$SOC <- ifelse(Model2_outputs_MELI$SOC < 0, 0, Model2_outputs_MELI$SOC)
Model2_outputs_MELI$modeledtime=Model2_outputs_MELI$Time+2014
Model2_outputs_MELI$modelname="Model 1: Litterfall"
View(Model2_outputs_MELI)


Model3_outputs_MELI=rbind(data.frame(Ct3_long_F38_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F32_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F26_MELI,Condition="Managed"),
                          data.frame(Ct3_long_F23_MELI,Condition="Unmanaged"))
Model3_outputs_MELI <- Model3_outputs_MELI %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "TotalSOC" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))
Model3_outputs_MELI$SOC <- ifelse(Model3_outputs_MELI$SOC < 0, 0, Model3_outputs_MELI$SOC)
Model3_outputs_MELI$modeledtime=Model3_outputs_MELI$Time+2014
Model3_outputs_MELI$modelname="Model 1: Litterfall"
View(Model3_outputs_MELI)


finalmodels_MELI=rbind(Model1_outputs_MELI,
                  Model2_outputs_MELI,
                  Model3_outputs_MELI)

SOC_ALLMODELS_MELI <- finalmodels_MELI %>% filter(Fraction == "TotalSOC")

Observed_SOC_plot$Data="SMN (1990-2020)"


SOC_ALLMODELS$Data_W="SMN (1990-2020)"
SOC_ALLMODELS_MELI$Data_W="Field data (2017-2020)"

SOC_PLOT_WEATHER=rbind(SOC_ALLMODELS,SOC_ALLMODELS_MELI)
View(SOC_PLOT_WEATHER)

SOC_PLOT_WEATHER_plot=ggplot(SOC_PLOT_WEATHER, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = Data)) +
  geom_point(data = Observed_SOC_plot, 
             aes(x = modeledtime, y = Measured_SOC, color = Stand_age, shape = Stand_age), 
             size = 2) +
  geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.7) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 200, by = 20))+
  scale_x_continuous(breaks = seq(2010, 2114, by = 25))+
  scale_shape_manual(values = c("07 years" = 15, "18 years" = 16, "30 years" = 17, "80 years" = 9)) +
  labs(
    x = "Time (years)",
    color = "Stand age:",
    linetype = "Data:"
  ) +
  ylab(Mgha_expression) +
  facet_wrap(~modelname,nrow = 3)+
  theme(text = element_text(size=11,family = "A"),
        strip.text = element_text(size = 10, face = "bold",hjust = 0),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black')) +
  guides(
    shape = "none",
    color = guide_legend(nrow = 1,override.aes = list(shape = c(15, 16, 17, 9))), # Stand age en una fila
  )
SOC_PLOT_WEATHER_plot

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "SOC_plot.jpeg"),
     width = 190,height = 230,units = "mm",res = 1000)
SOC_plot
dev.off()

#15. MANAGEMENT SCENARIOS --------------------------------
# Corriendo con 7 años:
years2=seq(1/12,1,by=1/12)
xi.frame_F38_ESC=data.frame(years2,rep(fT*fW_F38,length.out=length(years2)))
litterfall_values <- c(2.3383, 4.5223, 4.0218, 3.04245, 4.2984, 4.6976, 3.8229) 


# Esc a 07 años: Folio 38 -----------------------------------------------------
initial_pools_F38_M1 <- as.numeric(poolSize3_F38[,2:6])

Y07_2014=RothCModel(t=years2,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=initial_pools_F38_M1,
                      In=litterfall_values[1],
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_ESC) #Loads the model



CT_e07y=getC(Y07_2014) #Calculates stocks for each pool per month
TotalC3_07=rowSums(CT_e07y)#Calculates total (sum of all pools)  
Rt3_07y=getReleaseFlux(Y07_2014)#CO2 released for all pools 


CT_e07y_df <- data.frame(years2,as.data.frame(CT_e07y),TotalC3_07)
names(CT_e07y_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize07y=tail(CT_e07y_df,1)
poolSize07y #Reported data- reported 

# Esc a 08 años: Folio 38 -----------------------------------------------------


Y08_2015=RothCModel(t=years2,
                    ks=c(10,0.3,0.66,0.02,0),
                    C0=as.numeric(poolSize07y[,2:6]),
                    In=litterfall_values[2],
                    clay=clay_F38_2014,
                    DR=0.25,
                    xi=xi.frame_F38_ESC) #Loads the model

CT_e08y=getC(Y08_2015) #Calculates stocks for each pool per month
TotalC3_08=rowSums(CT_e08y)#Calculates total (sum of all pools)  
Rt3_08y=getReleaseFlux(Y08_2015)#CO2 released for all pools 


CT_e08y_df <- data.frame(years2,as.data.frame(CT_e08y),TotalC3_08)
names(CT_e08y_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize08y=tail(CT_e08y_df,1)
poolSize08y #Reported data- reported   
  
# Esc a 09 años: Folio 38 -----------------------------------------------------


Y09_2016=RothCModel(t=years2,
                    ks=c(10,0.3,0.66,0.02,0),
                    C0=as.numeric(poolSize08y[,2:6]),
                    In=litterfall_values[3],
                    clay=clay_F38_2014,
                    DR=0.25,
                    xi=xi.frame_F38_ESC) #Loads the model

CT_e09y=getC(Y09_2016) #Calculates stocks for each pool per month
TotalC3_09=rowSums(CT_e09y)#Calculates total (sum of all pools)  
Rt3_09y=getReleaseFlux(Y09_2016)#CO2 released for all pools 


CT_e09y_df <- data.frame(years2,as.data.frame(CT_e09y),TotalC3_09)
names(CT_e09y_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize09y=tail(CT_e09y_df,1)
poolSize09y #Reported data- reported   

# Esc a 10 años: Folio 38 -----------------------------------------------------


Y10_2017=RothCModel(t=years2,
                    ks=c(10,0.3,0.66,0.02,0),
                    C0=as.numeric(poolSize09y[,2:6]),
                    In=litterfall_values[4],
                    clay=clay_F38_2014,
                    DR=0.25,
                    xi=xi.frame_F38_ESC) #Loads the model

CT_e10y=getC(Y10_2017) #Calculates stocks for each pool per month
TotalC3_10=rowSums(CT_e10y)#Calculates total (sum of all pools)  
Rt3_10y=getReleaseFlux(Y10_2017)#CO2 released for all pools 


CT_e10y_df <- data.frame(years2,as.data.frame(CT_e10y),TotalC3_10)
names(CT_e10y_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize10y=tail(CT_e10y_df,1)
poolSize10y #Reported data- reported   


# Esc a 11 años: Folio 38 -----------------------------------------------------


Y11_2018=RothCModel(t=years2,
                    ks=c(10,0.3,0.66,0.02,0),
                    C0=as.numeric(poolSize10y[,2:6]),
                    In=litterfall_values[5],
                    clay=clay_F38_2014,
                    DR=0.25,
                    xi=xi.frame_F38_ESC) #Loads the model

CT_e11y=getC(Y11_2018) #Calculates stocks for each pool per month
TotalC3_11=rowSums(CT_e11y)#Calculates total (sum of all pools)  
Rt3_11y=getReleaseFlux(Y11_2018)#CO2 released for all pools 


CT_e11y_df <- data.frame(years2,as.data.frame(CT_e11y),TotalC3_11)
names(CT_e11y_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC")
poolSize11y=tail(CT_e11y_df,1)
poolSize11y #Reported data- reported   










































increment_factor<-1.10

## FOLIO 38 ---------------------------------------------
# Definir variables de entrada
yearsf38 <- 1:23  # Los 23 años de simulación para que alcancen 40 años, periodo de cosecha
initial_litterfall_f38 <- Cinputs_F38_2014_LITTERFAL  # Hojarasca inicial

C0_F38=initial_pools_F38_M1

# Inicializar las listas para almacenar los resultados
pools_man_escen_F38 <- matrix(0, nrow = length(yearsf38), ncol = 5)  # Para almacenar los pools en cada año
colnames(pools_man_escen_F38) <- c("DPM", "RPM", "BIO", "HUM", "IOM")

# Asegúrate de inicializar la hojarasca antes del bucle
litterfall <- initial_litterfall_f38  # Hojarasca inicial

# Bucle para cada año
for (i in 1:length(yearsf38)) {
  
  # Mostrar el valor de hojarasca cada año
  print(paste("Año:", yearsf38[i], "Hojarasca:", litterfall))
  
  # Ejecutar el modelo RothC para este año con la hojarasca incrementada
  model_result <- RothCModel(
    t = yearsf38[i],  # Año de simulación
    ks = c(10, 0.3, 0.66, 0.02, 0),  # Parámetros de descomposición
    C0 = initial_pools_F38_M1,  # Pools iniciales
    In = litterfall,  # Hojarasca actualizada
    clay = clay_F38_2014,  # Arcilla
    DR = 0.25,  # Factor de descomposición
    xi = xi.frame_F38  # Parámetros del modelo
  )
  
  # Extraer los tamaños de los pools desde el modelo
  pools_man_escen_F38[i, ] <- model_result@initialValues  # Guardar los valores de los pools
  
  # Actualizar C0 para el siguiente año con los valores obtenidos
  initial_pools_F38_M1 <- model_result@initialValues  # Los nuevos pools calculados para el siguiente año
  
  # Mostrar el tamaño de cada pool de SOM
  print(paste("Año:", yearsf38[i], "Pools SOM:", paste(model_result@initialValues, collapse = ", ")))
  
  # Actualizar la hojarasca para el siguiente año (incremento del 10%)
  litterfall <- litterfall * 1.1  # Incrementar la hojarasca en un 10% para el siguiente año
}



















pools_man_escen_F38

print(paste("Año:", yearsf38[i], "Hojarasca actualizada:", litterfall))
print(paste("Año:", yearsf38[i], "Pools SOM después del modelo:", paste(model_result@initialValues, collapse = ", ")))
print(paste("Año:", yearsf38[i], "Pools SOM después del modelo:", paste(model_result@initialValues, collapse = ", ")))


# Graficar los resultados
plot(years, pools_man_escen_F38[, "DPM"], type = "l", col = "red", lwd = 2, 
     xlab = "Año", ylab = "Tamaño del pool de C", 
     main = "Evolución de los Pools de Carbono con Manejo de Hojarasca")
lines(years, pools_man_escen_F38[, "RPM"], col = "blue", lwd = 2)
lines(years, pools_man_escen_F38[, "BIO"], col = "green", lwd = 2)
lines(years, pools_man_escen_F38[, "HUM"], col = "purple", lwd = 2)
lines(years, pools_man_escen_F38[, "IOM"], col = "orange", lwd = 2)

# Añadir leyenda
legend("topright", legend = c("DPM", "RPM", "BIO", "HUM", "IOM"),
       col = c("red", "blue", "green", "purple", "orange"), lwd = 2)



# 16. FINAL MANAGEMENT ESCENARIOS 2014-2113. F38 ---------------------------------
# Configuración inicial
years2 <- seq(1/12, 1, by = 1/12)  # Simulación mensual durante 1 año
num_years <- 100  # Número de años a simular

library(readxl)
Litterfal_Folio38 <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Litterfal_Folio38.xlsx")
View(Litterfal_Folio38)

litterfall_values <- c(2.3383, 4.5223, 4.0218, 3.04245, 4.2984, 4.6976, 3.8229)  # Extiende a 100 valores
litterfall_values=Litterfal_Folio38$`C inputs`

# Lista para almacenar resultados
pool_sizes <- list()

df_results <- data.frame()

# Estado inicial
initial_pools <- as.numeric(poolSize3_F38[, 2:6])

for (year in 1:num_years) {
  
  # Definir el input de hojarasca para el año actual
  litterfall_input <- litterfall_values[year] 
  
  # Correr modelo RothC
  Y_model <- RothCModel(
    t = years2,
    ks = c(10, 0.3, 0.66, 0.02, 0),
    C0 = initial_pools,
    In = litterfall_input,
    clay = clay_F38_2014,
    DR = 0.25,
    xi = xi.frame_F38_ESC
  )
  
  # Obtener los stocks de carbono
  CT_year <- getC(Y_model) 
  total_C <- rowSums(CT_year)  # Suma de todas las fracciones  
  
  # Guardar resultados
  df_year <- data.frame(years2, as.data.frame(CT_year), total_C)
  names(df_year) <- c("Time", "DPM", "RPM", "BIO", "HUM", "IOM", "TotalSOC")
  
  pool_sizes[[year]] <- tail(df_year, 1)  # Último mes del año
  df_results <- rbind(df_results, cbind(Year = year, pool_sizes[[year]]))
  
  # Actualizar pools iniciales para el próximo año
  initial_pools <- as.numeric(pool_sizes[[year]][, 2:6])
}



# 17. FINAL MANAGEMENT ESCENARIOS 2014-2113. F32 ---------------------------------
# Configuración inicial
xi.frame_F32_ESC=data.frame(years2,rep(fT*fW_F32,length.out=length(years2)))

Litterfal_Folio32 <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Litterfal_Folio32.xlsx")
View(Litterfal_Folio32)

litterfall_values=Litterfal_Folio32$`C inputs`

# Lista para almacenar resultados
pool_sizes <- list()

df_results <- data.frame()

# Estado inicial
initial_pools <- as.numeric(poolSize3_F32[, 2:6])

for (year in 1:num_years) {
  
  # Definir el input de hojarasca para el año actual
  litterfall_input <- litterfall_values[year] 
  
  # Correr modelo RothC
  Y_model <- RothCModel(
    t = years2,
    ks = c(10, 0.3, 0.66, 0.02, 0),
    C0 = initial_pools,
    In = litterfall_input,
    clay = clay_F32_2014,
    DR = 0.25,
    xi = xi.frame_F32_ESC
  )
  
  # Obtener los stocks de carbono
  CT_year <- getC(Y_model) 
  total_C <- rowSums(CT_year)  # Suma de todas las fracciones  
  
  # Guardar resultados
  df_year <- data.frame(years2, as.data.frame(CT_year), total_C)
  names(df_year) <- c("Time", "DPM", "RPM", "BIO", "HUM", "IOM", "TotalSOC")
  
  pool_sizes[[year]] <- tail(df_year, 1)  # Último mes del año
  df_results <- rbind(df_results, cbind(Year = year, pool_sizes[[year]]))
  
  # Actualizar pools iniciales para el próximo año
  initial_pools <- as.numeric(pool_sizes[[year]][, 2:6])
}



# 18. FINAL MANAGEMENT ESCENARIOS 2014-2113. F26 ---------------------------------
# Configuración inicial
xi.frame_F26_ESC=data.frame(years2,rep(fT*fW_F26,length.out=length(years2)))

Litterfal_Folio26 <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Litterfal_Folio26.xlsx")
View(Litterfal_Folio26)

litterfall_values=Litterfal_Folio26$`C inputs`

# Lista para almacenar resultados
pool_sizes <- list()

df_results <- data.frame()

# Estado inicial
initial_pools <- as.numeric(poolSize3_F26[, 2:6])

for (year in 1:num_years) {
  
  # Definir el input de hojarasca para el año actual
  litterfall_input <- litterfall_values[year] 
  
  # Correr modelo RothC
  Y_model <- RothCModel(
    t = years2,
    ks = c(10, 0.3, 0.66, 0.02, 0),
    C0 = initial_pools,
    In = litterfall_input,
    clay = clay_F26_2014,
    DR = 0.25,
    xi = xi.frame_F26_ESC
  )
  
  # Obtener los stocks de carbono
  CT_year <- getC(Y_model) 
  total_C <- rowSums(CT_year)  # Suma de todas las fracciones  
  
  # Guardar resultados
  df_year <- data.frame(years2, as.data.frame(CT_year), total_C)
  names(df_year) <- c("Time", "DPM", "RPM", "BIO", "HUM", "IOM", "TotalSOC")
  
  pool_sizes[[year]] <- tail(df_year, 1)  # Último mes del año
  df_results <- rbind(df_results, cbind(Year = year, pool_sizes[[year]]))
  
  # Actualizar pools iniciales para el próximo año
  initial_pools <- as.numeric(pool_sizes[[year]][, 2:6])
}



# 19. FINAL MANAGEMENT ESCENARIOS 2014-2113. F23 ---------------------------------
# Configuración inicial
xi.frame_F23_ESC=data.frame(years2,rep(fT*fW_F23,length.out=length(years2)))

Litterfal_Folio23 <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Litterfal_Folio23.xlsx")
View(Litterfal_Folio23)

litterfall_values=Litterfal_Folio23$`C inputs`

# Lista para almacenar resultados
pool_sizes <- list()

df_results <- data.frame()

# Estado inicial
initial_pools <- as.numeric(poolSize3_F23[, 2:6])

for (year in 1:num_years) {
  
  # Definir el input de hojarasca para el año actual
  litterfall_input <- litterfall_values[year] 
  
  # Correr modelo RothC
  Y_model <- RothCModel(
    t = years2,
    ks = c(10, 0.3, 0.66, 0.02, 0),
    C0 = initial_pools,
    In = litterfall_input,
    clay = clay_F23_2014,
    DR = 0.25,
    xi = xi.frame_F23_ESC
  )
  
  # Obtener los stocks de carbono
  CT_year <- getC(Y_model) 
  total_C <- rowSums(CT_year)  # Suma de todas las fracciones  
  
  # Guardar resultados
  df_year <- data.frame(years2, as.data.frame(CT_year), total_C)
  names(df_year) <- c("Time", "DPM", "RPM", "BIO", "HUM", "IOM", "TotalSOC")
  
  pool_sizes[[year]] <- tail(df_year, 1)  # Último mes del año
  df_results <- rbind(df_results, cbind(Year = year, pool_sizes[[year]]))
  
  # Actualizar pools iniciales para el próximo año
  initial_pools <- as.numeric(pool_sizes[[year]][, 2:6])
}




# 20. GRAFICAS DE MANEJO ------------------------------------------------------
Litterfal_ESCENARIOS_MANEJO100Y <- read_excel("01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Escenarios_Manejo_Litterfall.xlsx")
Litterfal_ESCENARIOS_MANEJO100Y$`Initial Stand Age`=as.factor(Litterfal_ESCENARIOS_MANEJO100Y$`Initial Stand Age`)
levels(Litterfal_ESCENARIOS_MANEJO100Y$`Initial Stand Age`)

#DPM
DPM_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = DPM*3, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("DPM")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
DPM_escMan_100y  

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "DPM_ESCMAN100Y.jpeg"),
     width = 190,height = 190,units = "mm",res = 1000)
DPM_escMan_100y  
dev.off()



#RPM
RPM_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = RPM, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("RPM")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
RPM_escMan_100y

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "RPM_ESCMAN100Y.jpeg"),
     width = 190,height = 190,units = "mm",res = 1000)
RPM_escMan_100y  
dev.off()



#BIO
BIO_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = BIO, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("BIO")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
BIO_escMan_100y

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "BIO_ESCMAN100Y.jpeg"),
     width = 190,height = 190,units = "mm",res = 1000)
BIO_escMan_100y  
dev.off()

#HUM
HUM_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = HUM, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("HUM")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2035,2060,2085,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
HUM_escMan_100y


#IOM
IOM_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = IOM, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("IOM")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2035,2060,2085,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
IOM_escMan_100y

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "HUM_IOM_ESCMAN100Y.jpeg"),
     width = 190,height = 220,units = "mm",res = 1000)
HUM_IOM_ESCMAN=ggarrange(HUM_escMan_100y,
          IOM_escMan_100y,
          ncol = 1,nrow = 2,widths = c(2, 1),
          common.legend = TRUE,
          legend = "bottom")
annotate_figure(HUM_IOM_ESCMAN, 
                left = textGrob(Mgha_expression, rot = 90, gp = gpar(cex = 1,fontfamily="A")))
dev.off()


#Total SOC
SOC_escMan_100y=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = TotalSOC, linetype = Condition)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("Total SOC")+
  labs(x = "Time (years)",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0.5,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
SOC_escMan_100y


jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "SOC_ESCMAN100Y.jpeg"),
     width = 190,height = 190,units = "mm",res = 1000)
SOC_escMan_100y  
dev.off()

#21. Diseño plots manejo escenarios: ----------------------------------------------
ESCMAN_DPM_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = DPM*3, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("DPM")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        axis.text.y = element_text(size = 11),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )
ESCMAN_DPM_FINAL

ESCMAN_RPM_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = RPM, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("RPM")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

ESCMAN_RPM_FINAL


jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "ESC_MAN_DPM_RPM.jpeg"),
     width = 190,height = 220,units = "mm",res = 1000)
DPM_RPM_ESCMAN_final=ggarrange(ESCMAN_DPM_FINAL,
                         ESCMAN_RPM_FINAL,
                         ncol = 1,nrow = 2,widths = c(2, 1),
                         common.legend = TRUE,
                         legend = "bottom")
annotate_figure(DPM_RPM_ESCMAN_final, 
                left = textGrob(Mgha_expression, rot = 90, vjust = 0.5,gp = gpar(cex = 1,fontfamily="A")))

dev.off()


ESCMAN_BIO_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = BIO+1, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("BIO")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.margin = margin(t = 20),
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

ESCMAN_BIO_FINAL


ESCMAN_HUM_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = HUM, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("HUM")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  #scale_y_continuous(limits = c(0,0.30),breaks = pretty_breaks(n = 3)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2035,2060,2085,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "none",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

ESCMAN_HUM_FINAL


ESCMAN_IOM_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = IOM, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  #geom_smooth(aes(x = Year, y = SOC, color = `Initial Stand Age`), 
  #            se = FALSE, size = 0.7) +
  theme_bw()+
  ggtitle("IOM")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(8,20),breaks = pretty_breaks(n = 5)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2035,2060,2085,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "none",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

ESCMAN_IOM_FINAL

ESCMAN_HUM_IOM_FINAL=ggarrange(ESCMAN_HUM_FINAL,
          ESCMAN_IOM_FINAL,widths = c(2, 1),
          ncol = 2,nrow = 1)
ESCMAN_HUM_IOM_FINAL


jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "ESC_MAN_BIO_HUM_IOM.jpeg"),
     width = 190,height = 220,units = "mm",res = 1000)
BIO_HUM_IOM_ESCMAN_final=ggarrange(ESCMAN_BIO_FINAL,
                                   ESCMAN_HUM_IOM_FINAL,
                                   ncol = 1,nrow = 2,
                                   heights = c(2, 1),
                                   common.legend = TRUE,
                                   legend = "bottom")

annotate_figure(BIO_HUM_IOM_ESCMAN_final, 
                bottom = textGrob("Projection",vjust = -6,gp = gpar(cex = 1,fontfamily="A")),
                left = textGrob(Mgha_expression, rot = 90, vjust = 0.5,gp = gpar(cex = 1,fontfamily="A")))

dev.off()

Observed_SOC2=Observed_SOC
names(Observed_SOC2)=c("TotalSOC","model","Year","Initial Stand Age")
Observed_SOC2$Condition=c("Managed","Managed","Managed","UnManaged","Managed","Managed","Managed","UnManaged","Managed","Managed","Managed","UnManaged")


ESCMAN_SOC_FINAL=ggplot(Litterfal_ESCENARIOS_MANEJO100Y, aes(x = Year, y = TotalSOC, linetype = Condition,color=`Initial Stand Age`)) +
  geom_line(size=0.7)+
  theme_bw()+
  ggtitle("Total SOC")+
  labs(x = "Projection",
       color = "Initial stand age:",
       linetype = "Condition:") +
  ylab(Mgha_expression)+
  #facet_wrap(~`Initial Stand Age`,ncol = 1)+
  #scale_color_manual(values = c("#16325B","#006BFF","#FC8F54","#FB4141"))+
  scale_linetype_manual(values = c(1,2))+
  scale_y_continuous(limits = c(30,75),breaks = pretty_breaks(n = 6)) +  
  scale_x_continuous(limits = c(2014,2114),breaks = c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100,2110)) + 
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        strip.text = element_blank(),
        panel.spacing = unit(0.8, "lines"),
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.x = element_blank(),
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        #plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 2, nrow = 1,  title.position = "top",title.hjust = 0) # Model en otra fila
  )

ESCMAN_SOC_FINAL

jpeg(filename = here("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/1. R analysis/3. Final model parameterization/Final figures", "ESC_MAN_SOCTOTAL.jpeg"),
     width = 190,height = 140,units = "mm",res = 1000)
ESCMAN_SOC_FINAL
dev.off()

#22. EQUILIBRIUM SCENARIO ----------------------------------
years500=seq(1/12,500,by=1/12)
xi.frame_F38_500=data.frame(years500,rep(fT*fW_F38,length.out=length(years)))

Model3_F38_500=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F38,RPMptf_F38,BIOptf_F38,HUMptf_F38,FallIOM_F38),
                      In=Cinputs_F38_2014_LITTERFAL*0.47,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38_500) #Loads the model
Ct3_F38_500=getC(Model3_F38_500) #Calculates stocks for each pool per month
TotalC3_F38_500=rowSums(Ct3_F38_500)#Calculates total (sum of all pools)  
Rt3_F38_500=getReleaseFlux(Model3_F38_500)#CO2 released for all pools 
?getReleaseFlux



matplot(years500, Ct3_F38_500, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

Ct3_F38_df <- data.frame(years,as.data.frame(Ct3_F38),TotalC3_F38,TotalRt3_F38)
names(Ct3_F38_df)=c("Time","DPM", "RPM", "BIO", "HUM", "IOM","TotalSOC","CO2")
poolSize3_F38=tail(Ct3_F38_df,1)
poolSize3_F38$Stand_age="07 years"
poolSize3_F38$Harvest_year=2005
poolSize3_F38 #Reported data
sum(Ct3_F38_df$"CO2", na.rm = TRUE)


Ct3_long_F38 <- Ct3_F38_df %>%
  pivot_longer(cols = -1, names_to = "Fraction", values_to = "SOC")
Ct3_long_F38$PSP=38
Ct3_long_F38$Stand_age=c("07 years")
Ct3_long_F38$Harvest_year=2005
View(Ct3_long_F38)