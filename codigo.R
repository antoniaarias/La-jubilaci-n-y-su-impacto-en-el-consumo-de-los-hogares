rm(list=ls())
# carpeta de trabajo
setwd("C:/Users/djara/Desktop/tarea2analisis")

#leer csv

library(readr)
df <- read.csv2("df_tarea2.csv")
library(dplyr)

# Paso 1: Construir la base de datos colapsada por a?o y diferencia de edades
datos_colapsados <- df %>%
  group_by(anno, esse_m) %>%
  summarize(
    promedio_lnc = mean(lnc),
    promedio_lncn = mean(lncn),
    promedio_lnjconsal = mean(lnjconsal),
    
    promedio_yr = mean(y_real),#1
    
    promedio_ar = mean(ar), #2
    
    promedio_educ = mean(educ_m),#3
    
    promedio_children = mean(children), #4
    
    .groups = 'drop'
  )



# Paso 2: Construir la variable "elegibilidad"
datos_colapsados <- datos_colapsados %>%
  mutate(elegibilidad = ifelse(esse_m >= 0, 1, 0))

# Paso 3: Construir la variable "porcentaje_retirados"
datos1 <- df %>%
  group_by(anno, esse_m) %>%
  summarize(porcentaje_retirados = mean(qu_m))


#Paso 4:
datos_colapsados <- datos_colapsados %>%
  merge(datos1)

# Verificar la base de datos final
print(datos_colapsados)

df <- datos_colapsados

#SACAR NA 
df <- na.omit(df)
 
#SE PIDE FILTRAR esse_m
df <- subset(df, esse_m > -10 & esse_m < 10)


#PARTE REGRESION DISCONTINUA
#running variable es esse_m, la variable dependiente es lnc y 
#el tratamiento es la elegibilidad

library(ggplot2)#Graficos

#1. Discontinuidad tratamiento en torno a variable running
ggplot(df)+
  aes(y= elegibilidad, x=esse_m)+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  labs(x='Variable running centrada en 0',# agrega etiqueta al eje x
       y='Probabilidad de ser tratado',# agrega etiqueta al eje y
       tittle='Probabilidad de ser tratado segun puntaje centrado')#agrega titulo


#2. NO MANIPULACION VARIABLE RUNNING, si es continuo, no hay manipulacion

### De manera grafica
df %>% ggplot(aes(x=esse_m))+
  geom_density()+
  geom_vline(xintercept =0, size=1, color='red')+
  labs(x='Variable running centrada en 0',# agrega etiqueta al eje x
       y='Densidad',# agrega etiqueta al eje y
       tittle='Grafico de densidad de variable running')#agrega titulo

#3 Continuidad de covariables


ggplot(df)+aes(x=esse_m, y=promedio_lncn, fill=factor(elegibilidad))+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  geom_smooth(method='lm',formula = y~x, se=TRUE)+
  labs(x='Running: Puntaje centrada en 0',# agrega etiqueta al eje x
       y='variable dependiente',# agrega etiqueta al eje y
       tittle='Distribucion de la variable dependiente segun running',
       subtitle = 'Con ajuste lineal')#agrega titulo

#4. CONTINUIDAD COVARIABLES CUANDO RUNNING ENTORNO A PUNTO DE CORTE
#los que estan cerca del ptje de corte son similiares

#De manera grafica

# Polinomio orden 1
#si las rectas est?n juntas es pq son continuas
ggplot(df)+aes(x=esse_m, y=promedio_yr, fill=factor(elegibilidad))+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  geom_smooth(method='lm',formula = y~x, se=TRUE)+
  labs(x='Running: Puntaje centrada en 0',# agrega etiqueta al eje x
       y='Covariable',# agrega etiqueta al eje y
       tittle='Distribucion de la covariable segun running',
       subtitle = 'Con ajuste lineal')#agrega titulo

# Polinomio orden 2
ggplot(df)+aes(x=esse_m, y=promedio_yr, fill=factor(elegibilidad))+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  geom_smooth(method='lm',formula = y~poly(x,2), se=TRUE)+
  labs(x='Running: Puntaje centrada en 0',# agrega etiqueta al eje x
       y='Covariable',# agrega etiqueta al eje y
       tittle='Distribucion de la covariable segun running',
       subtitle = 'Con ajuste lineal')#agrega titulo

# Polinomio orden 3
ggplot(df)+aes(x=esse_m, y=promedio_yr, fill=factor(elegibilidad))+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  geom_smooth(method='lm',formula = y~poly(x,3), se=TRUE)+
  labs(x='Running: Puntaje centrada en 0',# agrega etiqueta al eje x
       y='Covariable',# agrega etiqueta al eje y
       tittle='Distribucion de la covariable segun running',
       subtitle = 'Con ajuste lineal')#agrega titulo

# Polinomio orden 4
ggplot(df)+aes(x=esse_m, y=promedio_yr, fill=factor(elegibilidad))+
  geom_point()+
  geom_vline(xintercept =0, size=1, color='red')+
  geom_smooth(method='lm',formula = y~poly(x,4), se=TRUE)+
  labs(x='Running: Puntaje centrada en 0',# agrega etiqueta al eje x
       y='Covariable',# agrega etiqueta al eje y
       tittle='Distribucion de la covariable segun running',
       subtitle = 'Con ajuste lineal')#agrega titulo

#MODELOS DE RD

m1 = lm(promedio_lncn~ esse_m + elegibilidad, data=df) #sin controles
m2 = lm(promedio_lncn ~ esse_m + elegibilidad + promedio_yr + promedio_ar + promedio_educ + promedio_children, data=df)#con controles

size_ventana8<-8
df_ventana_8<-df %>% subset(esse_m > -size_ventana8 & esse_m < size_ventana8)
m3 = lm(promedio_lncn ~ esse_m + elegibilidad + promedio_yr + promedio_ar + promedio_educ + promedio_children,data=df_ventana_8)


size_ventana6<-6
df_ventana_6<-df %>% subset(esse_m > -size_ventana6 & esse_m < size_ventana6)
m4 = lm(promedio_lncn ~ esse_m + elegibilidad + promedio_yr + promedio_ar + promedio_educ + promedio_children,data=df_ventana_6)

size_ventana3<-3
df_ventana_3<-df %>% subset(esse_m > -size_ventana3 & esse_m < size_ventana3)
m5 = lm(promedio_lncn ~ esse_m + elegibilidad + promedio_yr + promedio_ar + promedio_educ + promedio_children,data=df_ventana_3)

library(stargazer)

stargazer(m1,m2,m3,m4,m5, title = 'Tabla de Regresiones', type= 'text', out= 'tabla_regs.html')

#PARTE VARIABLES INSTRUMENTALES
#Para esto, asuman que la variable end?gena es porcentaje_retirados, 
#el instrumento "elegibilidad" y la variable dependiente es lncn.

X <- df$porcentaje_retirados 
Z <- df$elegibilidad
Y <- df$promedio_var1

#RELEVANCIA DEL INSTRUMENTO: Relevancia de la variable instrumental
summary(lm(X~Z, data = df))
cov(X,Z)

#SUPUESTO DE INDEPENDENCIA: Eligiendo Covariables, cualquier wea dijo la carla

Q1 <- df$promedio_var1
Q2 <- df$promedio_var5
Q3 <- df$promedio_var7
#m1 = summary(lm(Q1~ Z, data = df))
#m2 = summary(lm(Q2~ Z, data = df))
#m3 = summary(lm(Q3~ Z, data = df))

t.test(Q1[Z == 1], Q1[Z == 0])


#MODELOS DE VARIABLES INSTRUMENTALES
library(ivreg)
mo1 = summary(ivreg(Y~X|Z, data =df)) #mediante ivreg sin controles
mo2 = summary(ivreg(Y~X + Q1 + Q2 + Q3|Z + Q1 + Q2 + Q3, data=df)) #mediante ivreg con controles
mo3 = summary(lm(Y~Z,data = df)) #Con ITT sin controles
mo4 = summary(lm(Y~Z + Q1 + Q2 + Q3,data = df)) #Con ITT con controles
 
#2SL2 sin controles
first_stage = lm(X~Z, data = df)
df$X_hat = predict(first_stage, newdata = df)
mo5 = summary(lm(Y~X_hat, data = df)) 
print(mo5)


#2SLS con controles
first_stage = lm(X~Z + Q1 + Q2 +Q3, data = df)
df$X_hat = predict(first_stage, newdata = df)
mo6 = summary(lm(Y~X_hat + Q1+ Q2 + Q3, data = df))


library(stargazer)
stargazer(mo1,mo2,mo3,mo4,mo5,mo6, title = 'Tabla de Regresiones', type= 'Text')
stargazer(mo1,mo2,mo3,mo4,mo5,mo6, title = 'Tabla de Regresiones', out= 'tabla_regs.html')










