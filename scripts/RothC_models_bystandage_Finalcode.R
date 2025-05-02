library(SoilR)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(here)
library(ggpubr)
library(scales)

setwd("D:/zaira/Documents/01. ACTIVE PROJECTS/A. PhD - Colpos/A. Thesis/Chapter 3/01. RothC models in R")
windowsFonts(A = windowsFont("Times New Roman"))



# 1. INICIALIZACIÓN DEL MODELO - FOLIO 38 -----------------------------------------------------

# 1.1. Datos generales de inicialización 
Temp=data.frame(Month=1:12,Temp=c(11.5,13,15,17.1,17.8,17.4,16.7,16.8,16.4,15.1,13,12.4))
Precip=data.frame(Month=1:12,Precip=c(40.6,33.2,36,47.3,52.2,173.4,158.5,206.9,313.5,211.2,100.7,29.5))
Evp=data.frame(Month=1:12,Evp=c(63.6,79.2,107.1,124.1,128.2,112.8,100.5,106.6,86.8,79.9,64,61.6))

soil.thick=30 #Soil thickness (organic layer topsoil), in cm
years=seq(1/12,100,by=1/12)


# 1.2. Datos Conglomerado 38 (7 años)
SOC_F38_2014= 129.6895    #Soil organic carbon in Mg/ha 
clay_F38_2014= 48.5       #Percent clay
Cinputs_F38_2014= 11.68727333 #Annual C inputs to soil in Mg/ha/yr


# 1.3. Efecto de temperatura y humedad
fT=fT.RothC(Temp[,2]) #Temperature effects per month
plot(fT)


fW_F38=fW.RothC(P=(Precip[,2]), E=(Evp[,2]), 
                S.Thick = soil.thick, pClay = clay_F38_2014, 
                pE = 0.75, bare = FALSE)$b #Moisture effects per month  (Only change clay)

xi.frame_F38=data.frame(years,rep(fT*fW_F38,length.out=length(years)))


# 1.4. Estimacion IOM

FallIOM_F38=0.049*SOC_F38_2014^(1.139) #IOM using Falloon method
FallIOM_F38

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F38=0.0236*SOC_F38_2014^(1.223)


# 2. FUNCIONES DE PEDOTRANSFERENCIA PARA VALORES TEORICOS INICIALES DE LAS FRACCIONES ------------

#Weihermueller et al. (2013) proposed a set of functions (pedotransfer functions) to obtain 
#the initial values of RothC pool sizes using data on total carbon clay contents. 
#Their functions are given by:

RPMptf_F38=(0.1847*SOC_F38_2014 + 0.1555)*((clay_F38_2014 + 1.275)^(-0.1158))
HUMptf_F38=(0.7148*SOC_F38_2014 + 0.5069)*((clay_F38_2014 + 0.3421)^(0.0184))
BIOptf_F38=(0.014*SOC_F38_2014 + 0.0075)*((clay_F38_2014 + 8.8473)^(0.0567))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
DPMptf_F38=SOC_F38_2014-(FallIOM_F38+RPMptf_F38+HUMptf_F38+BIOptf_F38)
c(DPMptf_F38, RPMptf_F38, BIOptf_F38, HUMptf_F38, FallIOM_F38)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F38_forest=SOC_F38_2014-(IOM_FOREST_F38+RPMptf_F38+HUMptf_F38+BIOptf_F38)
c(DPMptf_F38_forest, RPMptf_F38, BIOptf_F38, HUMptf_F38, IOM_FOREST_F38)


# 3. ROTHC MODELS F38 ----------------------------------------------------------
?RothCModel

## 3.1. Model 1: con pools teoricos iniciales ----------------------------------
Model_F38=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F38, RPMptf_F38, BIOptf_F38, HUMptf_F38, FallIOM_F38),
                     In=Cinputs_F38_2014,
                     clay=clay_F38_2014,
                     DR=0.25,
                     xi=xi.frame_F38) #Loads the model

# content of the pools as function of time
Ct_F38=getC(Model_F38) #Calculates stocks for each pool per month
TotalC_F38=rowSums(Ct_F38)
View(TotalC_F38)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F38=getReleaseFlux(Model_F38) 

?getReleaseFlux
matplot(years, Ct_F38, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F38=as.numeric(tail(Ct_F38,1))
names(poolSize_F38)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F38 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F38_df <- as.data.frame(Ct_F38)
Ct_F38_df$Time <- years  # Añadir la columna de tiempo
Ct_F38_df$SOC_total=TotalC_F38
names(Ct_F38_df)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F38=tail(Ct_F38_df,1)
poolSize_F38$Stand_age="07 años"
poolSize_F38$Harvest_year="2005"


# Convertir a formato largo (long format)
Ct2_long_F38 <- Ct_F38_df %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38$PSP=38
Ct2_long_F38$Stand_age=c("07 years")
Ct2_long_F38$Harvest_year=2005

View(Ct2_long_F38)
#Datos a graficar
write.csv(Ct2_long_F38,"RothC_fractions_F38_100años_Modelo1.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F38, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  #scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )

## 3.2. Model 2: con pools teoricos iniciales especificos de bosque ----------------------------------
Model2_F38=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F38_forest, RPMptf_F38, BIOptf_F38, HUMptf_F38, IOM_FOREST_F38),
                     In=Cinputs_F38_2014,
                     clay=clay_F38_2014,
                     DR=0.25,
                     xi=xi.frame_F38) #Loads the model

# content of the pools as function of time
Ct_F38_model2=getC(Model2_F38) #Calculates stocks for each pool per month
TotalC_F38_model2=rowSums(Ct_F38_model2)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F38_model2=getReleaseFlux(Model2_F38) 

matplot(years, Ct_F38_model2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F38_model2=as.numeric(tail(Ct_F38_model2,1))
names(poolSize_F38_model2)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F38_model2 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F38_df_model2 <- as.data.frame(Ct_F38_model2)
Ct_F38_df_model2$Time <- years  # Añadir la columna de tiempo
names(Ct_F38_df_model2)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F38_model2 <- Ct_F38_df_model2 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38_model2$PSP=38
Ct2_long_F38_model2$Stand_age=c("7 years")

#Datos a graficar
write.csv(Ct2_long_F38_model2,"RothC_fractions_F38_100años_Modelo2.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F38_model2, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 3.3. Model 3: con pools teoricos iniciales en 0 (ecuaciones tradicionales) ----------------------------------
Model3_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F38),
                      In=Cinputs_F38_2014,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model

# content of the pools as function of time
Ct_F38_model3=getC(Model3_F38) #Calculates stocks for each pool per month
TotalC_F38_model3=rowSums(Ct_F38_model3)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F38_model3=getReleaseFlux(Model3_F38) 

matplot(years, Ct_F38_model3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F38_model3=as.numeric(tail(Ct_F38_model3,1))
names(poolSize_F38_model3)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F38_model3 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F38_df_model3 <- as.data.frame(Ct_F38_model3)
Ct_F38_df_model3$Time <- years  # Añadir la columna de tiempo
names(Ct_F38_df_model3)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F38_model3 <- Ct_F38_df_model3 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38_model3$PSP=38
Ct2_long_F38_model3$Stand_age=c("7 years")

#Datos a graficar
write.csv(Ct2_long_F38_model3,"RothC_fractions_F38_100años_Modelo3.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F38_model3, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 3.4. Model 4: con pools teoricos iniciales en 0 (ecuaciones de bosque)----------------------------------
Model4_F38=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F38),
                      In=Cinputs_F38_2014,
                      clay=clay_F38_2014,
                      DR=0.25,
                      xi=xi.frame_F38) #Loads the model

# content of the pools as function of time
Ct_F38_model4=getC(Model4_F38) #Calculates stocks for each pool per month
TotalC_F38_model4=rowSums(Ct_F38_model4)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F38_model4=getReleaseFlux(Model4_F38) 

matplot(years, Ct_F38_model4, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F38_model4=as.numeric(tail(Ct_F38_model4,1))
names(poolSize_F38_model4)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F38_model4 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F38_df_model4 <- as.data.frame(Ct_F38_model4)
Ct_F38_df_model4$Time <- years  # Añadir la columna de tiempo
names(Ct_F38_df_model4)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F38_model4 <- Ct_F38_df_model4 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38_model4$PSP=38
Ct2_long_F38_model4$Stand_age=c("7 years")

#Datos a graficar
write.csv(Ct2_long_F38_model4,"RothC_fractions_F38_100años_Modelo4.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F38_model4, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )



# 4. INICIALIZACIÓN DEL MODELO - FOLIO 32 -----------------------------------------------------

# 4.1. Datos Conglomerado 32 (18 años)
SOC_F32_2014= 151.69147 #Soil organic carbon in Mg/ha 
clay_F32_2014= 39.76       #Percent clay
Cinputs_F32_2014= 18.26673333 #Annual C inputs to soil in Mg/ha/yr


fW_F32=fW.RothC(P=(Precip[,2]), E=(Evp[,2]), 
                S.Thick = soil.thick, pClay = clay_F32_2014, 
                pE = 0.75, bare = FALSE)$b #Moisture effects per month  (Only change clay)

xi.frame_F32=data.frame(years,rep(fT*fW_F32,length.out=length(years)))


# 4.2. Estimacion IOM

FallIOM_F32=0.049*SOC_F32_2014^(1.139) #IOM using Falloon method
FallIOM_F32

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F32=0.0236*SOC_F32_2014^(1.223)


# 5. FUNCIONES DE PEDOTRANSFERENCIA PARA VALORES TEORICOS INICIALES DE LAS FRACCIONES ------------

#Weihermueller et al. (2013) proposed a set of functions (pedotransfer functions) to obtain 
#the initial values of RothC pool sizes using data on total carbon clay contents. 
#Their functions are given by:

RPMptf_F32=(0.1847*SOC_F32_2014 + 0.1555)*((clay_F32_2014 + 1.275)^(-0.1158))
HUMptf_F32=(0.7148*SOC_F32_2014 + 0.5069)*((clay_F32_2014 + 0.3421)^(0.0184))
BIOptf_F32=(0.014*SOC_F32_2014 + 0.0075)*((clay_F32_2014 + 8.8473)^(0.0567))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
DPMptf_F32=SOC_F32_2014-(FallIOM_F32+RPMptf_F32+HUMptf_F32+BIOptf_F32)
c(DPMptf_F32, RPMptf_F32, BIOptf_F32, HUMptf_F32, FallIOM_F32)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F32_forest=SOC_F32_2014-(IOM_FOREST_F32+RPMptf_F32+HUMptf_F32+BIOptf_F32)
c(DPMptf_F32_forest, RPMptf_F32, BIOptf_F32, HUMptf_F32, IOM_FOREST_F32)


# 6. ROTHC MODELS F32 ----------------------------------------------------------
?RothCModel

## 6.1. Model 1: con pools teoricos iniciales ----------------------------------
Model_F32=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F32, RPMptf_F32, BIOptf_F32, HUMptf_F32, FallIOM_F32),
                     In=Cinputs_F32_2014,
                     clay=clay_F32_2014,
                     DR=0.25,
                     xi=xi.frame_F32) #Loads the model

# content of the pools as function of time
Ct_F32=getC(Model_F32) #Calculates stocks for each pool per month
TotalC_F32=rowSums(Ct_F32)
View(TotalC_F32)


#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F32=getReleaseFlux(Model_F32) 

matplot(years, Ct_F32, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F32=as.numeric(tail(Ct_F32,1))
names(poolSize_F32)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F32 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F32_df <- as.data.frame(Ct_F32)
Ct_F32_df$Time <- years  # Añadir la columna de tiempo
Ct_F32_df$SOC_total=TotalC_F32
names(Ct_F32_df)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F32=tail(Ct_F32_df,1)
poolSize_F32$Stand_age="18 años"
poolSize_F32$Harvest_year="1995"

# Convertir a formato largo (long format)
Ct2_long_F32 <- Ct_F32_df %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32$PSP=32
Ct2_long_F32$Stand_age=c("18 years")
Ct2_long_F32$Harvest_year=1995

#Datos a graficar
write.csv(Ct2_long_F32,"RothC_fractions_F32_100años_Modelo1.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F32, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 180)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )



## 6.2. Model 2: con pools teoricos iniciales especificos de bosque ----------------------------------
Model2_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F32_forest, RPMptf_F32, BIOptf_F32, HUMptf_F32, IOM_FOREST_F32),
                      In=Cinputs_F32_2014,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model

# content of the pools as function of time
Ct_F32_model2=getC(Model2_F32) #Calculates stocks for each pool per month
TotalC_F32_model2=rowSums(Ct_F32_model2)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F32_model2=getReleaseFlux(Model2_F32) 

matplot(years, Ct_F32_model2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F32_model2=as.numeric(tail(Ct_F32_model2,1))
names(poolSize_F32_model2)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F32_model2 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F32_df_model2 <- as.data.frame(Ct_F32_model2)
Ct_F32_df_model2$Time <- years  # Añadir la columna de tiempo
names(Ct_F32_df_model2)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F32_model2 <- Ct_F32_df_model2 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32_model2$PSP=32
Ct2_long_F32_model2$Stand_age=c("18 years")

#Datos a graficar
write.csv(Ct2_long_F32_model2,"RothC_fractions_F32_100años_Modelo2.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F32_model2, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 150)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 6.3. Model 3: con pools teoricos iniciales en 0 (ecuaciones tradicionales) ----------------------------------
Model3_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F32),
                      In=Cinputs_F32_2014,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model

# content of the pools as function of time
Ct_F32_model3=getC(Model3_F32) #Calculates stocks for each pool per month
TotalC_F32_model3=rowSums(Ct_F32_model3)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F32_model3=getReleaseFlux(Model3_F32) 

matplot(years, Ct_F32_model3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F32_model3=as.numeric(tail(Ct_F32_model3,1))
names(poolSize_F32_model3)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F32_model3 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F32_df_model3 <- as.data.frame(Ct_F32_model3)
Ct_F32_df_model3$Time <- years  # Añadir la columna de tiempo
names(Ct_F32_df_model3)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F32_model3 <- Ct_F32_df_model3 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32_model3$PSP=32
Ct2_long_F32_model3$Stand_age=c("18 years")

#Datos a graficar
write.csv(Ct2_long_F32_model3,"RothC_fractions_F32_100años_Modelo3.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F32_model3, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 3",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 6.4. Model 4: con pools teoricos iniciales en 0 (ecuaciones de bosque)----------------------------------
Model4_F32=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F32),
                      In=Cinputs_F32_2014,
                      clay=clay_F32_2014,
                      DR=0.25,
                      xi=xi.frame_F32) #Loads the model

# content of the pools as function of time
Ct_F32_model4=getC(Model4_F32) #Calculates stocks for each pool per month
TotalC_F32_model4=rowSums(Ct_F32_model4)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F32_model4=getReleaseFlux(Model4_F32) 

matplot(years, Ct_F32_model4, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F32_model4=as.numeric(tail(Ct_F32_model4,1))
names(poolSize_F32_model4)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F32_model4 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F32_df_model4 <- as.data.frame(Ct_F32_model4)
Ct_F32_df_model4$Time <- years  # Añadir la columna de tiempo
names(Ct_F32_df_model4)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F32_model4 <- Ct_F32_df_model4 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32_model4$PSP=32
Ct2_long_F32_model4$Stand_age=c("18 years")

#Datos a graficar
write.csv(Ct2_long_F32_model4,"RothC_fractions_F32_100años_Modelo4.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F32_model4, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 4",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


# 7. INICIALIZACIÓN DEL MODELO - FOLIO 26 -----------------------------------------------------

# 7.1. Datos Conglomerado 26 (30 años)
SOC_F26_2014=187.6272  #Soil organic carbon in Mg/ha 
clay_F26_2014= 49.54       #Percent clay
Cinputs_F26_2014=16.0028 #Annual C inputs to soil in Mg/ha/yr

fW_F26=fW.RothC(P=(Precip[,2]), E=(Evp[,2]), 
                S.Thick = soil.thick, pClay = clay_F26_2014, 
                pE = 0.75, bare = FALSE)$b #Moisture effects per month  (Only change clay)

xi.frame_F26=data.frame(years,rep(fT*fW_F26,length.out=length(years)))


# 7.2. Estimacion IOM

FallIOM_F26=0.049*SOC_F26_2014^(1.139) #IOM using Falloon method
FallIOM_F26

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F26=0.0236*SOC_F26_2014^(1.223)


# 8. FUNCIONES DE PEDOTRANSFERENCIA PARA VALORES TEORICOS INICIALES DE LAS FRACCIONES ------------

#Weihermueller et al. (2013) proposed a set of functions (pedotransfer functions) to obtain 
#the initial values of RothC pool sizes using data on total carbon clay contents. 
#Their functions are given by:

RPMptf_F26=(0.1847*SOC_F26_2014 + 0.1555)*((clay_F26_2014 + 1.275)^(-0.1158))
HUMptf_F26=(0.7148*SOC_F26_2014 + 0.5069)*((clay_F26_2014 + 0.3421)^(0.0184))
BIOptf_F26=(0.014*SOC_F26_2014 + 0.0075)*((clay_F26_2014 + 8.8473)^(0.0567))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
DPMptf_F26=SOC_F26_2014-(FallIOM_F26+RPMptf_F26+HUMptf_F26+BIOptf_F26)
c(DPMptf_F26, RPMptf_F26, BIOptf_F26, HUMptf_F26, FallIOM_F26)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F26_forest=SOC_F26_2014-(IOM_FOREST_F26+RPMptf_F26+HUMptf_F26+BIOptf_F26)
c(DPMptf_F26_forest, RPMptf_F26, BIOptf_F26, HUMptf_F26, IOM_FOREST_F26)


# 9. ROTHC MODELS F26 ----------------------------------------------------------
?RothCModel

## 9.1. Model 1: con pools teoricos iniciales ----------------------------------
Model_F26=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F26, RPMptf_F26, BIOptf_F26, HUMptf_F26, FallIOM_F26),
                     In=Cinputs_F26_2014,
                     clay=clay_F26_2014,
                     DR=0.25,
                     xi=xi.frame_F26) #Loads the model

# content of the pools as function of time
Ct_F26=getC(Model_F26) #Calculates stocks for each pool per month
TotalC_F26=rowSums(Ct_F26)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F26=getReleaseFlux(Model_F26) 

matplot(years, Ct_F26, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F26=as.numeric(tail(Ct_F26,1))
names(poolSize_F26)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F26 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F26_df <- as.data.frame(Ct_F26)
Ct_F26_df$Time <- years  # Añadir la columna de tiempo
Ct_F26_df$SOC_total=TotalC_F26
names(Ct_F26_df)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F26=tail(Ct_F26_df,1)
poolSize_F26$Stand_age="30 años"
poolSize_F26$Harvest_year="1983"


# Convertir a formato largo (long format)
Ct2_long_F26 <- Ct_F26_df %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26$PSP=26
Ct2_long_F26$Stand_age=c("30 years")
Ct2_long_F26$Harvest_year=1983

#Datos a graficar
write.csv(Ct2_long_F26,"RothC_fractions_F26_100años_Modelo1.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F26, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 190)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )



## 9.2. Model 2: con pools teoricos iniciales especificos de bosque ----------------------------------
Model2_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F26_forest, RPMptf_F26, BIOptf_F26, HUMptf_F26, IOM_FOREST_F26),
                      In=Cinputs_F26_2014,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model

# content of the pools as function of time
Ct_F26_model2=getC(Model2_F26) #Calculates stocks for each pool per month
TotalC_F26_model2=rowSums(Ct_F26_model2)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F26_model2=getReleaseFlux(Model2_F26) 

matplot(years, Ct_F26_model2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F26_model2=as.numeric(tail(Ct_F26_model2,1))
names(poolSize_F26_model2)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F26_model2 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F26_df_model2 <- as.data.frame(Ct_F26_model2)
Ct_F26_df_model2$Time <- years  # Añadir la columna de tiempo
names(Ct_F26_df_model2)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F26_model2 <- Ct_F26_df_model2 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26_model2$PSP=26
Ct2_long_F26_model2$Stand_age=c("30 years")

#Datos a graficar
write.csv(Ct2_long_F26_model2,"RothC_fractions_F26_100años_Modelo2.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F26_model2, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 150)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 9.3. Model 3: con pools teoricos iniciales en 0 (ecuaciones tradicionales) ----------------------------------
Model3_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F26),
                      In=Cinputs_F26_2014,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model

# content of the pools as function of time
Ct_F26_model3=getC(Model3_F26) #Calculates stocks for each pool per month
TotalC_F26_model3=rowSums(Ct_F26_model3)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F26_model3=getReleaseFlux(Model3_F26) 

matplot(years, Ct_F26_model3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F26_model3=as.numeric(tail(Ct_F26_model3,1))
names(poolSize_F26_model3)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F26_model3 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F26_df_model3 <- as.data.frame(Ct_F26_model3)
Ct_F26_df_model3$Time <- years  # Añadir la columna de tiempo
names(Ct_F26_df_model3)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F26_model3 <- Ct_F26_df_model3 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26_model3$PSP=26
Ct2_long_F26_model3$Stand_age=c("30 years")

#Datos a graficar
write.csv(Ct2_long_F26_model3,"RothC_fractions_F26_100años_Modelo3.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F26_model3, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 3",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 9.4. Model 4: con pools teoricos iniciales en 0 (ecuaciones de bosque)----------------------------------
Model4_F26=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F26),
                      In=Cinputs_F26_2014,
                      clay=clay_F26_2014,
                      DR=0.25,
                      xi=xi.frame_F26) #Loads the model

# content of the pools as function of time
Ct_F26_model4=getC(Model4_F26) #Calculates stocks for each pool per month
TotalC_F26_model4=rowSums(Ct_F26_model4)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F26_model4=getReleaseFlux(Model4_F26) 

matplot(years, Ct_F26_model4, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F26_model4=as.numeric(tail(Ct_F26_model4,1))
names(poolSize_F26_model4)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F26_model4 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F26_df_model4 <- as.data.frame(Ct_F26_model4)
Ct_F26_df_model4$Time <- years  # Añadir la columna de tiempo
names(Ct_F26_df_model4)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F26_model4 <- Ct_F26_df_model4 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26_model4$PSP=26
Ct2_long_F26_model4$Stand_age=c("30 years")

#Datos a graficar
write.csv(Ct2_long_F26_model4,"RothC_fractions_F26_100años_Modelo4.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F26_model4, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 4",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


# 10. INICIALIZACIÓN DEL MODELO - FOLIO 23 -----------------------------------------------------

# 10.1. Datos Conglomerado 23 (80 años)
SOC_F23_2014=98.3563  #Soil organic carbon in Mg/ha 
clay_F23_2014= 52.92       #Percent clay
Cinputs_F23_2014=20.6255 #Annual C inputs to soil in Mg/ha/yr

fW_F23=fW.RothC(P=(Precip[,2]), E=(Evp[,2]), 
                S.Thick = soil.thick, pClay = clay_F23_2014, 
                pE = 0.75, bare = FALSE)$b #Moisture effects per month  (Only change clay)

xi.frame_F23=data.frame(years,rep(fT*fW_F23,length.out=length(years)))


# 10.2. Estimacion IOM

FallIOM_F23=0.049*SOC_F23_2014^(1.139) #IOM using Falloon method
FallIOM_F23

#Utilizando la ecuación del Fallon 2001 (pag 94) para bosques:
IOM_FOREST_F23=0.0236*SOC_F23_2014^(1.223)


# 11. FUNCIONES DE PEDOTRANSFERENCIA PARA VALORES TEORICOS INICIALES DE LAS FRACCIONES ------------

#Weihermueller et al. (2013) proposed a set of functions (pedotransfer functions) to obtain 
#the initial values of RothC pool sizes using data on total carbon clay contents. 
#Their functions are given by:

RPMptf_F23=(0.1847*SOC_F23_2014 + 0.1555)*((clay_F23_2014 + 1.275)^(-0.1158))
HUMptf_F23=(0.7148*SOC_F23_2014 + 0.5069)*((clay_F23_2014 + 0.3421)^(0.0184))
BIOptf_F23=(0.014*SOC_F23_2014 + 0.0075)*((clay_F23_2014 + 8.8473)^(0.0567))

#The DPM fraction is therefore calculated as the remainder of the sum of these fractions and
DPMptf_F23=SOC_F23_2014-(FallIOM_F23+RPMptf_F23+HUMptf_F23+BIOptf_F23)
c(DPMptf_F23, RPMptf_F23, BIOptf_F23, HUMptf_F23, FallIOM_F23)

#Estimando DPM usando ecuacion de IOM de bosque
DPMptf_F23_forest=SOC_F23_2014-(IOM_FOREST_F23+RPMptf_F23+HUMptf_F23+BIOptf_F23)
c(DPMptf_F23_forest, RPMptf_F23, BIOptf_F23, HUMptf_F23, IOM_FOREST_F23)


# 12. ROTHC MODELS F23 ----------------------------------------------------------
?RothCModel

## 12.1. Model 1: con pools teoricos iniciales ----------------------------------
Model_F23=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F23, RPMptf_F23, BIOptf_F23, HUMptf_F23, FallIOM_F23),
                     In=Cinputs_F23_2014,
                     clay=clay_F23_2014,
                     DR=0.25,
                     xi=xi.frame_F23) #Loads the model

# content of the pools as function of time
Ct_F23=getC(Model_F23) #Calculates stocks for each pool per month
TotalC_F23=rowSums(Ct_F23)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F23=getReleaseFlux(Model_F23) 

matplot(years, Ct_F23, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F23=as.numeric(tail(Ct_F23,1))
names(poolSize_F23)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F23 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F23_df <- as.data.frame(Ct_F23)
Ct_F23_df$Time <- years  # Añadir la columna de tiempo
Ct_F23_df$SOC_total=TotalC_F23

names(Ct_F23_df)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F23=tail(Ct_F23_df,1)
poolSize_F23$Stand_age="80 años"
poolSize_F23$Harvest_year="1933"


# Convertir a formato largo (long format)
Ct2_long_F23 <- Ct_F23_df %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23$PSP=23
Ct2_long_F23$Stand_age=c("80 years")
Ct2_long_F23$Harvest_year=1933

#Datos a graficar
write.csv(Ct2_long_F23,"RothC_fractions_F23_100años_Modelo1.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F23, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 160)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 12.2. Model 2: con pools teoricos iniciales especificos de bosque ----------------------------------
Model2_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(DPMptf_F23_forest, RPMptf_F23, BIOptf_F23, HUMptf_F23, IOM_FOREST_F23),
                      In=Cinputs_F23_2014,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model

# content of the pools as function of time
Ct_F23_model2=getC(Model2_F23) #Calculates stocks for each pool per month
TotalC_F23_model2=rowSums(Ct_F23_model2)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F23_model2=getReleaseFlux(Model2_F23) 

matplot(years, Ct_F23_model2, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F23_model2=as.numeric(tail(Ct_F23_model2,1))
names(poolSize_F23_model2)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F23_model2 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F23_df_model2 <- as.data.frame(Ct_F23_model2)
Ct_F23_df_model2$Time <- years  # Añadir la columna de tiempo
names(Ct_F23_df_model2)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F23_model2 <- Ct_F23_df_model2 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23_model2$PSP=23
Ct2_long_F23_model2$Stand_age=c("80 years")

#Datos a graficar
write.csv(Ct2_long_F23_model2,"RothC_fractions_F23_100años_Modelo2.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F23_model2, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 2",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 150)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 12.3. Model 3: con pools teoricos iniciales en 0 (ecuaciones tradicionales) ----------------------------------
Model3_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, FallIOM_F23),
                      In=Cinputs_F23_2014,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model

# content of the pools as function of time
Ct_F23_model3=getC(Model3_F23) #Calculates stocks for each pool per month
TotalC_F23_model3=rowSums(Ct_F23_model3)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F23_model3=getReleaseFlux(Model3_F23) 

matplot(years, Ct_F23_model3, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F23_model3=as.numeric(tail(Ct_F23_model3,1))
names(poolSize_F23_model3)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F23_model3 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F23_df_model3 <- as.data.frame(Ct_F23_model3)
Ct_F23_df_model3$Time <- years  # Añadir la columna de tiempo
names(Ct_F23_df_model3)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F23_model3 <- Ct_F23_df_model3 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23_model3$PSP=23
Ct2_long_F23_model3$Stand_age=c("80 years")

#Datos a graficar
write.csv(Ct2_long_F23_model3,"RothC_fractions_F23_100años_Modelo3.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F23_model3, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 3",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 12.4. Model 4: con pools teoricos iniciales en 0 (ecuaciones de bosque)----------------------------------
Model4_F23=RothCModel(t=years,
                      ks=c(10,0.3,0.66,0.02,0),
                      C0=c(0, 0, 0, 0, IOM_FOREST_F23),
                      In=Cinputs_F23_2014,
                      clay=clay_F23_2014,
                      DR=0.25,
                      xi=xi.frame_F23) #Loads the model

# content of the pools as function of time
Ct_F23_model4=getC(Model4_F23) #Calculates stocks for each pool per month
TotalC_F23_model4=rowSums(Ct_F23_model4)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F23_model4=getReleaseFlux(Model4_F23) 

matplot(years, Ct_F23_model4, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F23_model4=as.numeric(tail(Ct_F23_model4,1))
names(poolSize_F23_model4)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F23_model4 #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F23_df_model4 <- as.data.frame(Ct_F23_model4)
Ct_F23_df_model4$Time <- years  # Añadir la columna de tiempo
names(Ct_F23_df_model4)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time")

# Convertir a formato largo (long format)
Ct2_long_F23_model4 <- Ct_F23_df_model4 %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23_model4$PSP=23
Ct2_long_F23_model4$Stand_age=c("80 years")

#Datos a graficar
write.csv(Ct2_long_F23_model4,"RothC_fractions_F23_100años_Modelo4.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F23_model4, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 4",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  scale_y_continuous(limits = c(0, 120)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


# 13. FRACCIONES POR EDAD ------------------------------------------

Model1_outputs=rbind(Ct2_long_F38,
           Ct2_long_F32,
           Ct2_long_F26,
           Ct2_long_F23)
View(Model1_outputs)
head(Model1_outputs,12)

library(dplyr)

Model1_outputs <- Model1_outputs %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "SOCtotal" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))

Model1_outputs$SOC <- ifelse(Model1_outputs$SOC < 0, 0, Model1_outputs$SOC)
Model1_outputs$modeledtime=Model1_outputs$Time+2014
Model1_outputs$modelname="Model 2: Litterfall + root turnover"
View(Model1_outputs)

Mgha_expression=expression("SOC (Mg ha"^"-1"~")")

modelo2=ggplot(Model1_outputs, aes(x = modeledtime, y = SOC, color = Stand_age)) +
  geom_line(data = Model1_outputs %>% filter(Fraction== "IOM"), )+
  geom_smooth(data = Model1_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.5) +
  theme_bw()+
  labs(title = "b) Model 2: Litterfall and root turnover",
    x = "Time (years)",
    color = "Stand age:",
  ) +
  ylab(Mgha_expression)+
  facet_wrap(~reorder(Fraction,ORDER_FRACTION),scales = "free_y")+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0.5,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
modelo2

jpeg(filename = here("Final figures", "Modelo2_RothC.jpeg"),
     width = 190,height = 140,units = "mm",res = 1000)
modelo2
dev.off()

# 14. TODAS LAS FRACCIONES -----------------------------------------------------
Pools_equilibrio_100años=rbind(data.frame(poolSize_F38,row.names = poolSize_F38$Harvest_year),
                               data.frame(poolSize_F32,row.names = poolSize_F32$Harvest_year),
                               data.frame(poolSize_F26,row.names = poolSize_F26$Harvest_year),
                               data.frame(poolSize_F23,row.names = poolSize_F23$Harvest_year))
View(Pools_equilibrio_100años)



# 15. INTEGRANDO SOLO LA HOJARASCA ANUAL ------------------------------------------
## 15.1. Model 2 - FOLIO 38 ----------------------------------------------------
Cinputs_F38_2014_litter=2.338273333  #Annual C inputs to soil in Mg/ha/yr

Model_F38_LITTER=RothCModel(t=years,
                     ks=c(10,0.3,0.66,0.02,0),
                     C0=c(DPMptf_F38, RPMptf_F38, BIOptf_F38, HUMptf_F38, FallIOM_F38),
                     In=Cinputs_F38_2014_litter,
                     clay=clay_F38_2014,
                     DR=0.25,
                     xi=xi.frame_F38) #Loads the model

# content of the pools as function of time
Ct_F38_litter=getC(Model_F38_LITTER) #Calculates stocks for each pool per month
TotalC_F38_litter=rowSums(Ct_F38_litter)
View(TotalC_F38_litter)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F38_litter=getReleaseFlux(Model_F38_LITTER) 

?getReleaseFlux
matplot(years, Ct_F38_litter, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F38_litter=as.numeric(tail(Ct_F38_litter,1))
names(poolSize_F38_litter)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F38_litter #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F38_df_litter <- as.data.frame(Ct_F38_litter)
Ct_F38_df_litter$Time <- years  # Añadir la columna de tiempo
Ct_F38_df_litter$SOC_total=TotalC_F38_litter
names(Ct_F38_df_litter)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F38_litter=tail(Ct_F38_df_litter,1)
poolSize_F38_litter$Stand_age="07 años"
poolSize_F38_litter$Harvest_year="2005"


# Convertir a formato largo (long format)
Ct2_long_F38_litter <- Ct_F38_df_litter %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F38_litter$PSP=38
Ct2_long_F38_litter$Stand_age=c("07 years")
Ct2_long_F38_litter$Harvest_year=2005

View(Ct2_long_F38_litter)
#Datos a graficar
write.csv(Ct2_long_F38_litter,"RothC_fractions_F38_100años_Modelo1_litter.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F38_litter, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  #scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 15.2. Model 2 - FOLIO 32 ----------------------------------------------------
Cinputs_F32_2014_litter=4.500733333  #Annual C inputs to soil in Mg/ha/yr

Model_F32_LITTER=RothCModel(t=years,
                            ks=c(10,0.3,0.66,0.02,0),
                            C0=c(DPMptf_F32, RPMptf_F32, BIOptf_F32, HUMptf_F32, FallIOM_F32),
                            In=Cinputs_F32_2014_litter,
                            clay=clay_F32_2014,
                            DR=0.25,
                            xi=xi.frame_F32) #Loads the model

# content of the pools as function of time
Ct_F32_litter=getC(Model_F32_LITTER) #Calculates stocks for each pool per month
TotalC_F32_litter=rowSums(Ct_F32_litter)
View(TotalC_F32_litter)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F32_litter=getReleaseFlux(Model_F32_LITTER) 

?getReleaseFlux
matplot(years, Ct_F32_litter, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F32_litter=as.numeric(tail(Ct_F32_litter,1))
names(poolSize_F32_litter)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F32_litter #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F32_df_litter <- as.data.frame(Ct_F32_litter)
Ct_F32_df_litter$Time <- years  # Añadir la columna de tiempo
Ct_F32_df_litter$SOC_total=TotalC_F32_litter
names(Ct_F32_df_litter)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F32_litter=tail(Ct_F32_df_litter,1)
poolSize_F32_litter$Stand_age="18 años"
poolSize_F32_litter$Harvest_year=1995


# Convertir a formato largo (long format)
Ct2_long_F32_litter <- Ct_F32_df_litter %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F32_litter$PSP=32
Ct2_long_F32_litter$Stand_age=c("18 years")
Ct2_long_F32_litter$Harvest_year=1995

View(Ct2_long_F32_litter)
#Datos a graficar
write.csv(Ct2_long_F32_litter,"RothC_fractions_F32_100años_Modelo1_litter.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F32_litter, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  #scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 15.3. Model 2 - FOLIO 26 ----------------------------------------------------
Cinputs_F26_2014_litter=5.812783333  #Annual C inputs to soil in Mg/ha/yr

Model_F26_LITTER=RothCModel(t=years,
                            ks=c(10,0.3,0.66,0.02,0),
                            C0=c(DPMptf_F26, RPMptf_F26, BIOptf_F26, HUMptf_F26, FallIOM_F26),
                            In=Cinputs_F26_2014_litter,
                            clay=clay_F26_2014,
                            DR=0.25,
                            xi=xi.frame_F26) #Loads the model

# content of the pools as function of time
Ct_F26_litter=getC(Model_F26_LITTER) #Calculates stocks for each pool per month
TotalC_F26_litter=rowSums(Ct_F26_litter)
View(TotalC_F26_litter)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F26_litter=getReleaseFlux(Model_F26_LITTER) 

?getReleaseFlux
matplot(years, Ct_F26_litter, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F26_litter=as.numeric(tail(Ct_F26_litter,1))
names(poolSize_F26_litter)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F26_litter #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F26_df_litter <- as.data.frame(Ct_F26_litter)
Ct_F26_df_litter$Time <- years  # Añadir la columna de tiempo
Ct_F26_df_litter$SOC_total=TotalC_F26_litter
names(Ct_F26_df_litter)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F26_litter=tail(Ct_F26_df_litter,1)
poolSize_F26_litter$Stand_age="30 años"
poolSize_F26_litter$Harvest_year=1983


# Convertir a formato largo (long format)
Ct2_long_F26_litter <- Ct_F26_df_litter %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F26_litter$PSP=26
Ct2_long_F26_litter$Stand_age=c("30 years")
Ct2_long_F26_litter$Harvest_year=1983

View(Ct2_long_F26_litter)
#Datos a graficar
write.csv(Ct2_long_F26_litter,"RothC_fractions_F26_100años_Modelo1_litter.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F26_litter, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  #scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


## 15.4. Model 2 - FOLIO 23 ----------------------------------------------------
Cinputs_F23_2014_litter=6.798916667  #Annual C inputs to soil in Mg/ha/yr

Model_F23_LITTER=RothCModel(t=years,
                            ks=c(10,0.3,0.66,0.02,0),
                            C0=c(DPMptf_F23, RPMptf_F23, BIOptf_F23, HUMptf_F23, FallIOM_F23),
                            In=Cinputs_F23_2014_litter,
                            clay=clay_F23_2014,
                            DR=0.25,
                            xi=xi.frame_F23) #Loads the model

# content of the pools as function of time
Ct_F23_litter=getC(Model_F23_LITTER) #Calculates stocks for each pool per month
TotalC_F23_litter=rowSums(Ct_F23_litter)
View(TotalC_F23_litter)

#Generic Function to obtain the vector of release fluxes out of the pools for all times.
Rt_F23_litter=getReleaseFlux(Model_F23_LITTER) 

?getReleaseFlux
matplot(years, Ct_F23_litter, type="l", lty=1, col=1:5,
        xlab="Time (years)", ylab="SOC (Mg/ha)")
legend("topright", c("DPM", "RPM", "BIO", "HUM", "IOM"),
       lty=1, col=1:5, bty="n")

poolSize_F23_litter=as.numeric(tail(Ct_F23_litter,1))
names(poolSize_F23_litter)<-c("DPM", "RPM", "BIO", "HUM", "IOM")
poolSize_F23_litter #Reported data


# Convertir Ct2 a un data frame para facilitar el manejo
Ct_F23_df_litter <- as.data.frame(Ct_F23_litter)
Ct_F23_df_litter$Time <- years  # Añadir la columna de tiempo
Ct_F23_df_litter$SOC_total=TotalC_F23_litter
names(Ct_F23_df_litter)=c("DPM", "RPM", "BIO", "HUM", "IOM","Time","SOCtotal")
poolSize_F23_litter=tail(Ct_F23_df_litter,1)
poolSize_F23_litter$Stand_age="80 años"
poolSize_F23_litter$Harvest_year=1933


# Convertir a formato largo (long format)
Ct2_long_F23_litter <- Ct_F23_df_litter %>%
  pivot_longer(cols = -6, names_to = "Fraction", values_to = "SOC")
Ct2_long_F23_litter$PSP=23
Ct2_long_F23_litter$Stand_age=c("80 years")
Ct2_long_F23_litter$Harvest_year=1933

View(Ct2_long_F23_litter)
#Datos a graficar
write.csv(Ct2_long_F23_litter,"RothC_fractions_F23_100años_Modelo1_litter.csv",row.names = TRUE)


# Crear la gráfica con ggplot2
ggplot(Ct2_long_F23_litter, aes(x = Time, y = SOC, color = Fraction)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Dynamics of Carbon Fractions Modelo 1",
    x = "Time (years)",
    y = "SOC (Mg/ha)",
    color = "Fraction:"
  ) +
  #scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom"
  )


# 16. FRACCIONES POR EDAD _ MODELO LITTER ------------------------------------------

Model2_outputs=rbind(Ct2_long_F38_litter,
                     Ct2_long_F32_litter,
                     Ct2_long_F26_litter,
                     Ct2_long_F23_litter)
View(Model2_outputs)
head(Model2_outputs,12)

library(dplyr)

Model2_outputs <- Model2_outputs %>%
  mutate(ORDER_FRACTION = case_when(
    Fraction == "DPM" ~ 1,
    Fraction == "RPM" ~ 2,
    Fraction == "BIO" ~ 3,
    Fraction == "HUM" ~ 4,
    Fraction == "IOM" ~ 5,
    Fraction == "SOCtotal" ~ 6,
    TRUE ~ NA_real_  # Maneja cualquier valor no contemplado
  ))

Model2_outputs$SOC <- ifelse(Model2_outputs$SOC < 0, 0, Model2_outputs$SOC)
Model2_outputs$modeledtime=Model2_outputs$Time+2014
Model2_outputs$modelname="Model 1: Litterfall"
View(Model2_outputs)

modelo1=ggplot(Model2_outputs, aes(x = modeledtime, y = SOC, color = Stand_age)) +
  geom_line(data = Model2_outputs %>% filter(Fraction== "IOM"), )+
  geom_smooth(data = Model2_outputs %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.5) +
  theme_bw()+
  labs(title = "a) Model 1: Litterfall",
    x = "Time (years)",
       color = "Stand age:",
  ) +
  ylab(Mgha_expression)+
  facet_wrap(~reorder(Fraction,ORDER_FRACTION),scales = "free_y")+
  theme(text = element_text(size=11,family = "A"),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.background = element_rect(fill='white', colour='black'))
modelo1



jpeg(filename = here("Final figures", "Modelo2_RothC_LITTERFALL.jpeg"),
     width = 190,height = 140,units = "mm",res = 1000)
modelo2
dev.off()

# 17. TODAS LAS FRACCIONES -----------------------------------------------------
bothmodels=rbind(
Model1_outputs,
Model2_outputs)


# Suponiendo que tu dataframe se llama bothmodels
bothmodels <- bothmodels %>%
  mutate(Fraction = ifelse(Fraction == "SOCtotal", "Total SOC", Fraction))

head(bothmodels)

bothmodels_plot=ggplot(bothmodels, aes(x = modeledtime, y = SOC, color = Stand_age,linetype = modelname)) +
  geom_line(data = bothmodels %>% filter(Fraction== "IOM"), )+
  geom_smooth(data = bothmodels %>% filter(Fraction != "IOM"), 
              aes(x = modeledtime, y = SOC, color = Stand_age), 
              se = FALSE, size = 0.5) +
  theme_bw()+
  labs(
       x = "Time (years)",
       color = "Stand age:",
       linetype = "Model:"
  ) +
  ylab(Mgha_expression)+
  facet_wrap(~reorder(Fraction, ORDER_FRACTION), scales = "free_y") +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +  
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
    color = guide_legend(order = 2, nrow = 2, title.position = "top", title.hjust = 0), # Stand age en una fila
    linetype = guide_legend(order = 1, nrow = 2, title.position = "top", title.hjust = 0) # Model en otra fila
  )
bothmodels_plot

jpeg(filename = here("Final figures", "Both models RothC.jpeg"),
     width = 190,height = 160,units = "mm",res = 1000)
bothmodels_plot
dev.off()

# 18. SOC TOTAL -------------------------------

Observed_SOC=data.frame(Measured_SOC=c(129.6895,151.69147,187.6272,98.3563,126.3739497,159.3813843,170.1253768,120.7096722,119.3739497,159.3813843,148.1253768,133.7096722),
                        modelname=c("Model 1: Litterfall"),
                        modeledtime=c(2014,2014,2014,2014,2019,2019,2019,2019,2024,2024,2024,2024),
                        Stand_age=c("07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years"))

Observed_SOC$Stand_age <- factor(Observed_SOC$Stand_age)

Observed_SOC2=data.frame(Measured_SOC=c(129.6895,151.69147,187.6272,98.3563,126.3739497,159.3813843,170.1253768,120.7096722,119.3739497,159.3813843,148.1253768,133.7096722),
                         modelname=c("Model 2: Litterfall + root turnover"),
                         modeledtime=c(2014,2014,2014,2014,2019,2019,2019,2019,2024,2024,2024,2024),
                         Stand_age=c("07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years","07 years","18 years","30 years","80 years"))

Observed_SOC2$Stand_age <- factor(Observed_SOC$Stand_age)

Observed_SOC_plot=rbind(Observed_SOC,Observed_SOC2)

SOC_plot=ggplot(bothmodels %>% filter(Fraction == "Total SOC"), aes(x = modeledtime, y = SOC, color = Stand_age)) +
  geom_point(data = Observed_SOC_plot, 
             aes(x = modeledtime, y = Measured_SOC, color = Stand_age, shape = Stand_age), 
             size = 2) +
   geom_smooth(aes(x = modeledtime, y = SOC, color = Stand_age), 
               se = FALSE, size = 0.5) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 200, by = 20))+
  scale_shape_manual(values = c("07 years" = 15, "18 years" = 16, "30 years" = 17, "80 years" = 9)) +
  labs(
    x = "Time (years)",
    color = "Stand age:",
    linetype = "Model:"
  ) +
  ylab(Mgha_expression) +
  facet_wrap(~modelname)+
  theme(text = element_text(size=11,family = "A"),
        strip.text = element_text(size = 10, face = "bold"),
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
SOC_plot

jpeg(filename = here("Final figures", "SOC_plot RothC.jpeg"),
     width = 190,height = 120,units = "mm",res = 1000)
SOC_plot
dev.off()


# 19. FRACCIONES FINALES ---------------------------

# Model 2: Litterfall 
Pools_equilibrio_100años_mod1=rbind(data.frame(poolSize_F38_litter,row.names = poolSize_F38_litter$Stand_age),
                               data.frame(poolSize_F32_litter,row.names = poolSize_F32_litter$Stand_age),
                               data.frame(poolSize_F26_litter,row.names = poolSize_F26_litter$Stand_age),
                               data.frame(poolSize_F23_litter,row.names = poolSize_F23_litter$Stand_age))
View(Pools_equilibrio_100años_mod1)
Pools_equilibrio_100años_mod1$Model="Model 1: Litterfall"

# Model 2: Litterfall + root turnover
Pools_equilibrio_100años_mod2=rbind(data.frame(poolSize_F38,row.names = poolSize_F38$Stand_age),
                               data.frame(poolSize_F32,row.names = poolSize_F32$Stand_age),
                               data.frame(poolSize_F26,row.names = poolSize_F26$Stand_age),
                               data.frame(poolSize_F23,row.names = poolSize_F23$Stand_age))
View(Pools_equilibrio_100años_mod2)
Pools_equilibrio_100años_mod2$Model="Model 2: Litterfall + root turnover"

FINALFRACTION_ESTIMATION=rbind(Pools_equilibrio_100años_mod1,
                               Pools_equilibrio_100años_mod2)
write.csv(FINALFRACTION_ESTIMATION, "FINALFRACTION_ESTIMATION.csv", row.names = FALSE)


# 20. DIAGRAMA DE HOJARASCA ---------------------------
Litterfall_4stands <- read_excel("Litterfall_4stands.xlsx")
names(Litterfall_4stands)
Litterfall_4stands$Stand_age=as.factor(Litterfall_4stands$Stand_age)

library(ggplot2)
library(dplyr)

# Asegúrate de que "Month" tenga el orden correcto (de enero a diciembre)
Litterfall_4stands <- Litterfall_4stands %>%
  mutate(
    Month = factor(Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
    YearMonth = paste(Year, Month, sep = "-") # Crear una columna combinando Año y Mes
  )

Mgha2_expression=expression("Mg ha"^"-1")

# Calcular la variación promedio mensual y la desviación estándar
Litterfall_monthly_stats <- Litterfall_4stands %>%
  group_by(Month,Stand_age) %>%
  summarise(
    Avg_Litterfall = mean(Litterfall, na.rm = TRUE),
    SD_Litterfall = sd(Litterfall, na.rm = TRUE)
  )

# Graficar la variación promedio mensual con desviación estándar
# Graficar la variación promedio mensual con desviación estándar por Stand_age
Litterfall_month=ggplot(Litterfall_monthly_stats, aes(x = Month, y = Avg_Litterfall, color = Stand_age, group = Stand_age)) +
  geom_line(size = 0.7) + # Línea de la media por Stand_age
  #geom_point(size = 3) + # Puntos de la media por Stand_age
  geom_errorbar(
    aes(ymin = Avg_Litterfall - SD_Litterfall, ymax = Avg_Litterfall + SD_Litterfall), 
    width = 0.1, size = 0.5
  ) + # Líneas de la desviación estándar
  labs(
    title = "a) Monthly variation",
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
        legend.title = element_text(hjust = 0,size = 11,face = "bold",family = "A"),
        legend.text = element_text(size = 11),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 12,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 11,hjust = 0,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour='black'))
Litterfall_month

# Crear el gráfico
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
    axis.text.x = element_text(angle = 45, hjust = 1), # Inclinar etiquetas del eje X
    legend.position = "bottom",
    legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
    legend.text = element_text(size = 11),
    legend.box = "horizontal",
    legend.key = element_blank(),
    plot.title = element_text(size = 14,hjust = 0,face = "bold"),
    plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
    panel.grid.minor = element_blank(),
  panel.background = element_rect(fill='white', colour='black'))+
  scale_x_discrete(breaks = Litterfall_4stands$YearMonth[seq(1, nrow(Litterfall_4stands), by = 12)])

Litterfall_20142020
      
# Calcular la variación promedio mensual y la desviación estándar
Litterfall_monthly_stats <- Litterfall_4stands %>%
  group_by(Month,Stand_age) %>%
  summarise(
    Avg_Litterfall = mean(Litterfall, na.rm = TRUE),
    SD_Litterfall = sd(Litterfall, na.rm = TRUE)
  )

# Graficar la variación promedio mensual con desviación estándar
# Graficar la variación promedio mensual con desviación estándar por Stand_age
Litterfall_month=ggplot(Litterfall_monthly_stats, aes(x = Month, y = Avg_Litterfall, color = Stand_age, group = Stand_age)) +
  geom_line(size = 0.5) + # Línea de la media por Stand_age
  #geom_point(size = 3) + # Puntos de la media por Stand_age
  geom_errorbar(
    aes(ymin = Avg_Litterfall - SD_Litterfall, ymax = Avg_Litterfall + SD_Litterfall), 
    width = 0.1, size = 0.5
  ) + # Líneas de la desviación estándar
  labs(
    title = "a) Monthly variation",
    x = "Month",
    y = Mgha2_expression,
    color = "Stand age"
  ) +
  theme_bw() +
  scale_x_discrete(
    limits = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), # Asegura que los meses se muestren en orden
    breaks = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    labels = c("J", "F", "M", "A", "M", "J","J", "A", "S", "O", "N", "D")
  )+
  theme(axis.text = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size=11,family = "A"),
        axis.text.x = element_text(hjust = 1), # Inclinar etiquetas del eje X
        legend.position = "bottom",
        legend.title = element_text(hjust = 0,size = 12,face = "bold",family = "A"),
        legend.text = element_text(size = 11),
        legend.box = "horizontal",
        legend.key = element_blank(),
        plot.title = element_text(size = 14,hjust = 0,face = "bold"),
        plot.subtitle = element_text(size = 13,hjust = 0,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='white', colour='black'))


jpeg(filename = here("Final figures", "litterfall_plot RothC.jpeg"),
     width = 190,height = 190,units = "mm",res = 1000)
Litterfall_production=ggarrange(Litterfall_month,Litterfall_20142020,
                           ncol = 1,nrow = 2,
                           common.legend = TRUE,
                           legend = "bottom")
Litterfall_production
dev.off()


# 21. CLIMOGRAMA ----------------------------

# Supongamos que tu dataframe se llama climogram_data
# Asegúrate de tener las columnas 'Date', 'Precipitation', 'Evaporation', 'Temperature'

# Mostrar el gráfico
print(climogram_plot)

#meses=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#tmax=c(19,	21,	22.9,	25.4,	25.6,	23.9,	22.8,	22.9,	21.9,	21.4,	19.6,	19.9)
#tmin=c(4,	5,	7,	8.9,	10.1,	11,	10.6,	10.7,	10.8,	8.7,	6.5,	4.9)
#WEATHER=data.frame(meses,Temp,Precip[,2],Evp[,2],tmax,tmin)
#names(WEATHER)=c("Month","Month_number","Temperature", "Precipitation","Evaporation","TempMax","Te")


# Crear el climograma con el dataframe WEATHER

climogram_plot <- ggplot(WEATHER, aes(x = Month_number)) +
  # Línea para Precipitación y evaporación
  geom_bar(aes(y = Evaporation, fill = "Evaporation"), stat = "identity", alpha = 0.5, width = 0.6) +  # Barras para evaporación
  geom_line(aes(y = Precipitation, color = "Precipitation"), size = 0.8, linetype = 2) +  # Línea para precipitación
  
  # Eje secundario para Temperatura
  scale_y_continuous(
    name = "Rainfall and evaporation (mm)", 
    sec.axis = sec_axis(~ . / 10, name = "Temperature (°C)")  # Ajuste de escala para temperatura
  ) +
  
  # Líneas de temperatura
  geom_line(aes(y = Temperature * 10, color = "Temperature"), size = 0.7, linetype = 1) +  # Temperatura media (Escala ajustada)
  geom_line(aes(y = TempMax * 10, color = "Temperature Max"), size = 0.7, linetype = 1) +  # Temperatura máxima
  geom_line(aes(y = TempMin * 10, color = "Temperature Min"), size = 0.7, linetype = 1) +  # Temperatura mínima
  
  labs(title = "a)",
    x = "Month",
    y = "Rainfall and evaporation (mm)",
    color = "Parameters:",
    fill = ""
  ) +
  scale_color_manual(values = c(
    "Precipitation" = "#09122C", 
    "Temperature" = "red", 
    "Temperature Max" = "orange", 
    "Temperature Min" = "blue"
  ),
  labels = c(
    "Precipitation" = "Rainfall", 
    "Temperature" = "MAT", 
    "Temperature Max" = "TMAX", 
    "Temperature Min" = "TMIN") )+
  scale_fill_manual(values = c("Evaporation" = "#48A6A7")) +
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
    panel.background = element_rect(fill='white', colour='black')) +
  scale_x_continuous(
    breaks = 1:12, 
    labels = c("J", "F", "M", "A", "M", "J", 
               "J", "A", "S", "O", "N", "D")
  )+
  guides(
    color = guide_legend(order = 1, nrow = 2, title.hjust = 0), # Stand age en una fila
    fill = guide_legend(order = 2, nrow = 1, title.hjust = 0) # Model en otra fila
  )
# Mostrar el gráfico
print(climogram_plot)

jpeg(filename = here("Final figures", "climograma.jpeg"),
     width = 140,height = 90,units = "mm",res = 1000)
climogram_plot
dev.off()

# 22. ROOT TURNOVER

root_turnover=c(9.349,13.766,10.19,13.819)
anual_litterfall=c(2.3383,4.5007,5.8128,6.7989)
Stand_age=c(07,18,30,80)

Cinputs_frame=data.frame(root_turnover,anual_litterfall,Stand_age)

# Convertir el dataframe a formato largo para ggplot
Cinputs_long <- reshape2::melt(Cinputs_frame, id.vars = "Stand_age", variable.name = "Variable", value.name = "Mg_per_ha")

# Crear el gráfico de barras acumuladas
Cinputs_barplot=ggplot(Cinputs_long, aes(x = as.factor(Stand_age), y = Mg_per_ha, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack",width = 0.4) +
  labs(
    x = "Stand age",
    y = "Mg/ha",
    fill = "Plant residues:",
    title = "b)"
  ) +
  scale_fill_manual(values = c("root_turnover" = "#3E5879", "anual_litterfall" = "#D39D55"),
                    labels = c(
                      "root_turnover" = "Root turnover", 
                      "anual_litterfall" = "Litterfall")) +
  theme_minimal() +
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
        panel.background = element_rect(fill='white', colour='black'))+
  guides(
    fill = guide_legend(order = 2, nrow = 2, title.hjust = 0) # Model en otra fila
  )
  
jpeg(filename = here("Final figures", "Modelinputs.jpeg"),
     width = 190,height = 100,units = "mm",res = 1000)
Modelinputs=ggarrange(climogram_plot,Cinputs_barplot,
                                ncol = 2,nrow = 1,widths = c(2, 1),
                                common.legend = FALSE,
                                legend = "bottom")
Modelinputs
dev.off()

# 22. EFECTOS DE TEMPERATURA ----------------------------------------------------

## 22.1. Efectos de temperatura y humedad - SMN (Both curves)-----------------
months <- 1:12
efectos_38=data.frame(Month=1:12,
                      fT_38=fT,
                      fW_F38=fW_F38)
efectos_38_long <- efectos_38 %>%
  pivot_longer(cols = c(fT_38, fW_F38), names_to = "Factor", values_to = "Value")

ggplot(efectos_38_long, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 1.2) +  # Línea para mostrar la tendencia
  geom_point(size = 3) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  labs(
    title = "Efectos de Temperatura (fT) y Humedad (fW) por Mes",
    x = "Mes",
    y = "Efecto (fT y fW)",
    color = "Factor"
  ) +
  theme_minimal(base_size = 14) +  # Tema limpio
  theme(legend.position = "top")   # Ubicar la leyenda arriba


## 22.2. Efectos de temperatura y humedad - datos meli - Both curves ------------------

Temp_MELI=data.frame(Month=1:12,Temp=c(10.1,7.4,12.4,13.5,14.4,15.6,15,
                                       14.7,14.1,14.5,12.8,11.8))

Precip_MELI=data.frame(Month=1:12,Precip=c(83.3,35.6,44.8,39.6,29.3,
                                           100.3,76.3,124.1,89.8,294.9,21.1,13.8))
Evp_MELI=data.frame(Month=1:12,Evp=c(84.67,96.53,125.33,124.00,
                                     108.93,94.80,127.33,113.73,109.20,124.40,106.67,91.33))

fT_MELI=fT.RothC(Temp_MELI[,2]) #Temperature effects per month
fW_F38_MELI=fW.RothC(P=(Precip_MELI[,2]), E=(Evp_MELI[,2]), 
                S.Thick = soil.thick, pClay = clay_F38_2014, 
                pE = 0.75, bare = FALSE)$b #Moisture effects per month  (Only change clay)

efectos_38_MELI=data.frame(Month=1:12,
                      fT_38=fT_MELI,
                      fW_F38=fW_F38_MELI)
efectos_38_long_MELI <- efectos_38_MELI %>%
  pivot_longer(cols = c(fT_38, fW_F38), names_to = "Factor", values_to = "Value")

ggplot(efectos_38_long_MELI, aes(x = Month, y = Value, color = Factor, group = Factor)) +
  geom_line(size = 1.2) +  # Línea para mostrar la tendencia
  geom_point(size = 3) +   # Puntos para resaltar los valores mensuales
  scale_x_continuous(breaks = months, labels = month.abb) +  # Etiquetas de los meses
  labs(
    title = "Efectos de Temperatura (fT) y Humedad (fW) por Mes",
    x = "Mes",
    y = "Efecto (fT y fW)",
    color = "Factor"
  ) +
  theme_minimal(base_size = 14) +  # Tema limpio
  theme(legend.position = "top")   # Ubicar la leyenda arriba


fT
fT_MELI
