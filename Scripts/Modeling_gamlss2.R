
# Loading the modified data -----------------------------------------------
datos <- readRDS(file="Data/datos")

# Filtering ---------------------------------------------------------------
library(dplyr)

# To select the variables
datos |> select(ingresos, 
                sexo, edu, est_civ,
                edad, experiencia,
                profesion) -> dt

# To drop rows with NA
dt <- na.omit(dt)

# To eliminate 0 from ingresos
ind <- dt$ingresos > 0
dt <- dt[ind, ]


# To see professions
table(dt$profesion)

########################## Ing Civil ##########################

subdat <- filter(dt, profesion=="Ingenieros civiles")

# Para organizar los niveles de las variables cuali
table(subdat$edu)
subdat$edu <- as.factor(subdat$edu)
subdat$edu <- relevel(subdat$edu, ref="Universitaria")
table(subdat$edu)

table(subdat$est_civ)
subdat$est_civ<- as.factor(subdat$est_civ)
subdat$est_civ <- relevel(subdat$est_civ, ref="Soltero")
table(subdat$est_civ)

# Para eliminar 10% de las observaciones extremas
loc_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ind <- x >= lower & x <= upper
  ind
}

ind <- loc_outliers_iqr(subdat$ingresos)
subdat <- subdat[ind, ]

# Graficos para incluir en los slides
library(ggplot2)

p0 <- ggplot(subdat, aes(x=ingresos)) + 
  geom_density(color = "#F5A623", fill = "#F5A623") + 
  xlab("Ingreso [millones de pesos]") +
  ylab("Densidad")

p0

p1 <- ggplot(subdat, aes(x=sexo, y=ingresos, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Sexo", y = "Ingreso [millones de pesos]", fill = "Sexo")

p1

p2 <- ggplot(subdat, aes(x=ingresos, fill = sexo)) +
  geom_density( alpha = 0.7, color = NA) +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Ingreso [millones de pesos]", fill = "Sexo", y = "Densidad")

p2

# Save the plot
ggsave("densi_ingreso_ing_civ.png", plot=p0, width=6, height=4, dpi=300)
ggsave("ingreso_sexo_ing_civ.png", plot=p1, width=6, height=4, dpi=300)

# Modeling ----------------------------------------------------------------

# Para instalar gamlss2 visitar el link
# https://gamlss-dev.github.io/gamlss2/

library(gamlss)
library(gamlss2)

# Mejor distribución
distri <- fitDist(y = subdat$ingresos, k = 2, type = c("realplus"))
distri$fits

# Entrenando el modelo
# BCTo      GB2        BCPE      exGAUS       BCCGo   GG      LOGNO 
# GIG       GA         IG        WEI          IGAMMA 
# EXP       GP         PARETO2o   

# Formula 
f1 <- ingresos ~ edad + experiencia + sexo + edu + est_civ 
f2 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .
f3 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .
f4 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .| .

# Modelos 
mod <- gamlss2(f4, data=subdat, family=BCTo, K=2)

# Para ver la tabla resumen del modelo
summary(mod)

# Estimación de la media 
#library("model")
nuevos_datosa <- data.frame(sexo = c("Mujer", "Hombre"),
                           edu = c("Universitaria", "Universitaria"),
                           est_civ = c("Soltero", "Soltero"),
                           edad = c(30, 30), 
                           experiencia = c(10, 10))

a <- est_param_2(mod = mod, fun = "median", m = 100000, 
                 data = subdat,
                 newdata = nuevos_datosa)
length(a)
a

IngresoM <- c(2.516951, 2.649647, 2.788539, 2.937363, 
              3.095378, 3.263794, 3.435392)
IngresoH <- c(2.670154, 2.791213, 2.923481, 3.071668, 
              3.245445, 3.410966, 3.593166)
EdadM <- c(30,35,40,45,50,55,60)
Edad <- rep(EdadM, 2)
Ingreso <- c(IngresoM, IngresoH)
Sexo <- c(rep("Mujer", 7), rep("Hombre",7))

datos_fig <- data.frame(Ingreso, Sexo, Edad)
datos_fig

library(ggplot2)
ggplot(datos_fig, aes(x = Edad, y = Ingreso, color = Sexo)) + 
  geom_line() +
  labs(x = "Edad",y = "Ingreso [millones de pesos]")  +
  scale_color_manual(values = c("Mujer" = "#6A0572", "Hombre" = "#00B8A9")) +  

# Para ver analisis de residuales junto
plot(mod)

# Para ver analisis de residuales por separado
plot(mod, which = "hist-resid")
plot(mod, which = "qq-resid")
plot(mod, which = "wp-resid")
plot(mod, which = "scatter-resid")

# Dos metricas importantes. 
# Entre mayor Rsq mejor
# Entre menor AIC mejor
Rsq(mod)
AIC(mod)

# Para obtener la correlacion entre y y y_hat (prediccion)
new <- subdat
pre <- predict(mod, type="parameter", newdata=new)

# Cuidado al calcular la correlacion, leer los mensajes de abajo
cor(pre[, 1], new$ingresos) # Para dist con 2 o mas parametros
cor(pre, new$ingresos)      # Para dist con 1 parametro

plot(x=pre[, 1], y=new$ingresos,# Para dist con 2 o mas parametros
     xlim=c(0, 10), ylim=c(0, 10))

plot(x=pre, y=new$ingresos, # Para ist con 1 parametro
     xlim=c(0, 10), ylim=c(0, 10))

abline(a=0, b=1, lty="dashed", col="red", lwd=2)

########################## Medicos generales ##########################

subdat <- filter(dt, profesion=="Médicos generales")

# Para organizar los niveles de las variables cuali
table(subdat$edu)
subdat$edu <- as.factor(subdat$edu)
subdat$edu <- relevel(subdat$edu, ref="Universitaria")
table(subdat$edu)

table(subdat$est_civ)
subdat$est_civ<- as.factor(subdat$est_civ)
subdat$est_civ <- relevel(subdat$est_civ, ref="Soltero")
table(subdat$est_civ)

# Para eliminar 10% de las observaciones extremas
loc_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ind <- x >= lower & x <= upper
  ind
}

ind <- loc_outliers_iqr(subdat$ingresos)
subdat <- subdat[ind, ]

# Graficos para incluir en los slides
library(ggplot2)

p0 <- ggplot(subdat, aes(x=ingresos)) + 
  geom_density(color = "#F5A623", fill = "#F5A623") + 
  xlab("Ingreso [millones de pesos]") +
  ylab("Densidad")

p0

p1 <- ggplot(subdat, aes(x=sexo, y=ingresos, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Sexo", y = "Ingreso [millones de pesos]", fill = "Sexo")

p1

p2 <- ggplot(subdat, aes(x=ingresos, fill = sexo)) +
  geom_density( alpha = 0.7, color = NA) +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Ingreso [millones de pesos]", fill = "Sexo", y = "Densidad")

p2

# Save the plot
ggsave("densi_ingreso_medicina.png", plot=p0, width=6, height=4, dpi=300)
ggsave("ingreso_sexo_medicina.png", plot=p1, width=6, height=4, dpi=300)

# Modeling ----------------------------------------------------------------

# Para instalar gamlss2 visitar el link
# https://gamlss-dev.github.io/gamlss2/

library(gamlss)
library(gamlss2)

# Mejor distribución
distri <- fitDist(y = subdat$ingresos, k = 2, type = c("realplus"))
distri$fits

# Distribuciones
# BCPEo     GB2        BCTo      exGAUS       BCCG   GG      GA 
# GIG       LOGNO        IG     WEI     IGAMMA    EXP        GP    

# Entrenando el modelo
# Formula 
f1 <- ingresos ~ edad + experiencia + sexo + edu + est_civ 
f2 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .
f3 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .
f4 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .| .

# Modelos 
mod <- gamlss2(f4, data=subdat, family=BCPEo, K=2)

# Para ver la tabla resumen del modelo
summary(mod)

# Estimación de la media 
#library("model")
nuevos_datosa <- data.frame(sexo = c("Mujer", "Hombre"),
                            edu = c("Universitaria", "Universitaria"),
                            est_civ = c("Soltero", "Soltero"),
                            edad = c(60, 60), 
                            experiencia = c(10, 10))

a <- est_param_2(mod = mod, fun = "median", m = 100000, 
                 data = subdat,
                 newdata = nuevos_datosa)
length(a)
a

IngresoM <- c(3.556385, 3.515536, 3.490768, 3.453001,
              3.423379, 3.389678, 3.358224)
IngresoH <- c(3.951132, 3.907769, 3.854291, 3.830561,
              3.784542, 3.752005, 3.712122)
EdadM <- c(30,35,40,45,50,55,60)
Edad <- rep(EdadM, 2)
Ingreso <- c(IngresoM, IngresoH)
Sexo <- c(rep("Mujer", 7), rep("Hombre",7))

datos_fig <- data.frame(Ingreso, Sexo, Edad)
datos_fig

library(ggplot2)
ggplot(datos_fig, aes(x = Edad, y = Ingreso, color = Sexo)) + 
  geom_line() +
  labs(x = "Edad",y = "Ingreso [millones de pesos]")  +
  scale_color_manual(values = c("Mujer" = "#6A0572", "Hombre" = "#00B8A9")) 
  
# Para ver analisis de residuales junto
plot(mod)

# Para ver analisis de residuales por separado
plot(mod, which = "hist-resid")
plot(mod, which = "qq-resid")
plot(mod, which = "wp-resid")
plot(mod, which = "scatter-resid")

# Dos metricas importantes. 
# Entre mayor Rsq mejor
# Entre menor AIC mejor
Rsq(mod)
AIC(mod)

# Para obtener la correlacion entre y y y_hat (prediccion)
new <- subdat
pre <- predict(mod, type="parameter", newdata=new)

# Cuidado al calcular la correlacion, leer los mensajes de abajo
cor(pre[, 1], new$ingresos) # Para dist con 2 o mas parametros
cor(pre, new$ingresos)      # Para dist con 1 parametro

plot(x=pre[, 1], y=new$ingresos,# Para dist con 2 o mas parametros
     xlim=c(0, 10), ylim=c(0, 10))

plot(x=pre, y=new$ingresos, # Para ist con 1 parametro
     xlim=c(0, 10), ylim=c(0, 10))

abline(a=0, b=1, lty="dashed", col="red", lwd=2)

########################## Enfermeria ##########################

subdat <- filter(dt, profesion=="Profesionales en enfermeria")

# Para organizar los niveles de las variables cuali
table(subdat$edu)
subdat$edu <- as.factor(subdat$edu)
subdat$edu <- relevel(subdat$edu, ref="Universitaria")
table(subdat$edu)

table(subdat$est_civ)
subdat$est_civ<- as.factor(subdat$est_civ)
subdat$est_civ <- relevel(subdat$est_civ, ref="Soltero")
table(subdat$est_civ)

# Para eliminar 10% de las observaciones extremas
loc_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.05, na.rm = TRUE)
  q3 <- quantile(x, 0.95, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  ind <- x >= lower & x <= upper
  ind
}

ind <- loc_outliers_iqr(subdat$ingresos)
subdat <- subdat[ind, ]

# Graficos para incluir en los slides
library(ggplot2)

p0 <- ggplot(subdat, aes(x=ingresos)) + 
  geom_density(color = "#F5A623", fill = "#F5A623") + 
  xlab("Ingreso [millones de pesos]") +
  ylab("Densidad")

p0

p1 <- ggplot(subdat, aes(x=sexo, y=ingresos, fill = sexo)) +
  geom_boxplot(alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Sexo", y = "Ingreso [millones de pesos]", fill = "Sexo")

p1

p2 <- ggplot(subdat, aes(x=ingresos, fill = sexo)) +
  geom_density( alpha = 0.7, color = NA) +
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("#00B8A9", "#6A0572")) + 
  labs(x = "Ingreso [millones de pesos]", fill = "Sexo", y = "Densidad")

p2

# Save the plot
ggsave("densi_ingreso_enfermeria.png", plot=p0, width=6, height=4, dpi=300)
ggsave("ingreso_sexo_enfermeria.png", plot=p1, width=6, height=4, dpi=300)

# Modeling ----------------------------------------------------------------

# Para instalar gamlss2 visitar el link
# https://gamlss-dev.github.io/gamlss2/

library(gamlss)
library(gamlss2)

# Mejor distribución
distri <- fitDist(y = subdat$ingresos, k = 2, type = c("realplus"))
distri$fits

# Distribuciones
# BCT   GB2      BCPEo     exGAUS   BCCG     
# GG    LOGNO    GIG       GA       IG               
# WEI   IGAMMA   EXP       GP       PARETO2 

# Entrenando el modelo
# Formula 
f1 <- ingresos ~ edad + experiencia + sexo + edu + est_civ 
f2 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .
f3 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .
f4 <- ingresos ~ edad + experiencia + sexo + edu + est_civ | .| .| .

# Modelos 
mod <- gamlss2(f4, data=subdat, family=BCT, K=2)

# Para ver la tabla resumen del modelo
summary(mod)

# Estimación de la media 
#library("model")
nuevos_datosa <- data.frame(sexo = c("Mujer", "Hombre"),
                            edu = c("Universitaria", "Universitaria"),
                            est_civ = c("Soltero", "Soltero"),
                            edad = c(60, 60), 
                            experiencia = c(10, 10))

a <- est_param_2(mod = mod, fun = "median", m = 100000, 
                 data = subdat,
                 newdata = nuevos_datosa)
length(a)
a

IngresoM <- c(2.161783,2.171453,2.192319, 2.216355, 
              2.237586, 2.248916, 2.272096)
IngresoH <- c(3.078828,3.099221,3.108537, 3.137864,
              3.150733, 3.178446, 3.229779)
EdadM <- c(30,35,40,45,50,55,60)
Edad <- rep(EdadM, 2)
Ingreso <- c(IngresoM, IngresoH)
Sexo <- c(rep("Mujer", 7), rep("Hombre",7))

datos_fig <- data.frame(Ingreso, Sexo, Edad)
datos_fig

library(ggplot2)
ggplot(datos_fig, aes(x = Edad, y = Ingreso, color = Sexo)) + 
  geom_line() +
  labs(x = "Edad",y = "Ingreso [millones de pesos]")  +
  scale_color_manual(values = c("Mujer" = "#6A0572", "Hombre" = "#00B8A9"))  
  
# Para ver analisis de residuales junto
plot(mod)

# Para ver analisis de residuales por separado
plot(mod, which = "hist-resid")
plot(mod, which = "qq-resid")
plot(mod, which = "wp-resid")
plot(mod, which = "scatter-resid")

# Dos metricas importantes. 
# Entre mayor Rsq mejor
# Entre menor AIC mejor
Rsq(mod)
AIC(mod)

# Para obtener la correlacion entre y y y_hat (prediccion)
new <- subdat
pre <- predict(mod, type="parameter", newdata=new)

# Cuidado al calcular la correlacion, leer los mensajes de abajo
cor(pre[, 1], new$ingresos) # Para dist con 2 o mas parametros
cor(pre, new$ingresos)      # Para dist con 1 parametro

plot(x=pre[, 1], y=new$ingresos,# Para dist con 2 o mas parametros
     xlim=c(0, 10), ylim=c(0, 10))

plot(x=pre, y=new$ingresos, # Para ist con 1 parametro
     xlim=c(0, 10), ylim=c(0, 10))

abline(a=0, b=1, lty="dashed", col="red", lwd=2)

