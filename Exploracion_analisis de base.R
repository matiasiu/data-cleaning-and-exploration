
setwd("C:/Users/Matias/Desktop/R/Proyectos/aurelio") #defino un path de trabajo en mi pc

#Cargo las librerías a utilizar

library(tidyverse)
library(ggplot2)
library (janitor)
library (writexl)

#cargo la base
base <- read.csv2 ("base.csv") 

#visualizo la base rápidamente
glimpse (base) 
  
#Solicito que los espacios en blanco aparezcan como "NA" (perdidos)
base[base == ""] <- NA 

#exploro los nombres de las columnas. Busco la que refiera a género
names (base) 

#trabajo sobre la variable Género#
#Encontré Género. Me interesó ver cuáleras eran sus variables y observar una tabla de frecuencias.

  Género <- base %>%
    group_by(Género)%>%
    count %>%
    print
  
  #Es necesario recodificar para organizar la variable y las frecuencias"
  
  base <- base%>%
    mutate (Género = recode(Género,
                            "Mujer" = "Mujer",
                            "MUJER" = "Mujer",
                            "MUJER TRANS" = "Trans",
                            "Varon" = "Varón",
                            "VARON"  = "Varón",
                             "Varón" = "Varón"))
  
#Vuelvo a chequear   
  Género <- base %>%
    group_by(Género)%>%
    count%>%
    print

  #preparo archivo en formato xls con información sobre la variable género. Para eso, voy a crear una tabla que sume los casos perdidos y una que no los sume. Luego las fundo en una sobla tabla.
  
  gen<- base %>%
    group_by(Género)%>%
    count ()%>%
    na.omit%>% #pido que no cuente los perdidos.
    arrange(desc(n))%>%  #Ordena en forma descendente en función a n
    adorn_totals("row", name = "Total", na.rm = TRUE) #Me agrega una  fila con el total.
  
  gen1<- base %>%
    group_by(Género)%>%
    count ()%>%
    arrange(desc(n))%>% 
    adorn_totals("row", name = "Total sumando perdidos", na.rm = TRUE) "Agrea una fila con total sumando perdidos"
  
  #combino en una tabla
    gen_combinado <- bind_rows(gen, gen1) %>%
    distinct(Género, .keep_all = TRUE)
  
#Agrego columna con porcentajes 
gen_porcentaje <- gen_combinado%>%
    group_by (Género)%>%
    mutate(Porcentaje = ifelse(Género %in% c("Varón", "Mujer","Trans", "Total"), (n / 1621) * 100, NA))%>%  #pido que sume una columna llamada "Porcentaje" que agregue porcentaje relativos 
    mutate(Porcentaje = round(Porcentaje, 1))  #Le pido que los porcentajes me lo redondee en una cifra
      
#Guardo en un archivo xls
write_xlsx(gen_porcentaje,"gen1.xlsx")
      

#Grafico género
#PAra graficar preparo la tabla de forma tal que excluye (filtre) filas totales y los casos perdidos

genPlot <- gen_porcentaje %>%
  filter(!(Género %in% c("Total", "Total sumando perdidos")))%>%
  na.omit

#Reordeno las columnas para mejorar el gráfico
genPlot$Género <- factor(genPlot$Género, levels = c("Mujer", "Varón", "Trans"))


ggplot(genPlot) +
  geom_bar(aes(x=(Género), y= Porcentaje, fill= Género), stat = "identity")+
  theme(legend.position = "none")+
  labs(
    title = "Género",
    x = "Género", 
    y = "Porcentaje",
    caption = "Fuente: Secretaría de Tierras. PBA.") +
  geom_text(aes(x= Género, y= Porcentaje, label = Porcentaje),
            position = position_stack(vjust = 0.4),
            hjust = 0.5,
            size = 4,
            color = "black")+
  theme(plot.caption = element_text(hjust = 0.1))




# Género y Situación laboral#
#Interesó conocer luego sobre la situación laboral de varones y mujeres.

#Para ello exploramos la base, ordenamos la variable Situación laboral y comenzamos a explorarla" 

names (base)

base <- base %>%
  rename("SL" = "Situación.laboral")

base <- base %>%
  mutate (SL= recode(SL,
                     "AMA DE CASA" ="Servicio doméstico",
                     "AMA DE CASA " ="Servicio doméstico",
                     "CHANGA" = "Trabajador informal",
                     "CHANGAS" = "Trabajador informal",
                     "CHANGAS " = "Trabajador informal",
                     "COOPERATIVISTA" = "Trabajador informal",
                     "CUENTAPROPISTA" = "Cuenta Propia",
                     "DESOCUPADA" = "Desocupade",
                     "DESOCUPADO" = "Desocupade",
                     "EMPLEADO RURAL TRANSITORIO" = "Trabajador rural",
                     "JORNALERA" = "Trabajador informal",
                     "NO CONTESTA" = "Ns/nc",
                     "Ns/nc" = "Ns/nc",
                     "Ns/Nc" = "Ns/nc",
                     "OBRERO" = "Trabajador formal",
                     "OCUPADO" = "Ns/nc",
                     "Pensionado/a - Jubilado/a" = "Pensionade/Jubilade",
                     "SERVICIO DOMESTICO" = "Servicio doméstico",
                     "Servicio doméstico" = "Servicio doméstico",                   
                     "Servicio Doméstico"  = "Servicio doméstico",                      
                     "SERVICIO DOMESTICO " = "Servicio doméstico",
                     "Trabajador/a autónomo (por cuenta propia CON monotributo)" = "Cuenta Propia",
                     "Trabajador/a desocupado/a" = "Desocupade",
                     "Trabajador/a en relación de dependencia (formal)" = "Trabajador formal",
                     "Trabajador/a informal (changas, cartoneo, etc.)" = "Trabajador informal"))

# Extraemos la variable SL de la base y Creamos un objeto nuevo (sit1) para trabajar más comodamente. 
#Vizualizamos SL y frecuencias (n), porcentajes relativos redondeados en 1 decimal, ordenar de modo descendente y agregar fila con "total, 

sit1 <- base %>%
  group_by(SL)%>%
  count ()%>%
  na.omit%>%
  mutate(Porcentaje = n / 1364*100)%>%
  mutate(Porcentaje = round(Porcentaje, 1))%>%
  arrange(desc(n))%>%
  adorn_totals()


#genero y Situación laboral


GenSL <- base %>%
  group_by(SL, Género) %>%
  count() %>%
  na.omit() %>%
  group_by(Género)%>%
  filter (Género != "Trans")%>%
  group_by(SL) %>%
  mutate(porcentajeSL = (n / sum(n)) * 100)%>%
  mutate(porcentajeSL = round (porcentajeSL, 1))

write_xlsx(GenSL4,"genSL4.xlsx")

ggplot(GenSL, aes(x = reorder (SL, -n), y = porcentajeSL, fill = Género))  +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  labs(x = "Situación Laboral", y = "Porcentaje", fill = "Género") +
  theme(legend.position = "none")+
  scale_fill_manual(values = c(Mujer ="green", Varón= "black")) +
  ggtitle("Género y Situación Laboral") +
  theme_minimal()+
  geom_text(aes(x= SL, label = porcentajeSL),
            position = position_stack(vjust = 0.4),
            hjust = 0.5,
            size = 4,
            color = "white")+
  guides(x = guide_axis(angle = 45))


#Entre primeras conclusiones del gráfico, podemos notar que:
# hay una importante proporción de desacoupadas mujeres con respecto a varones.  
#Que las trabajadoras domésticas se encuentra femenizada (no hay varones en el sector).
#Que por el contrario, en esta población los trabajadores rurales son varones.


#A partir de esta primer visualización, se podría plantear continuar trabajando sobre estos datos para pulirlos y explorarlos con mayor detalle y precisión.
#Asi por ejemplo, se podría aunar la categorización de la Situación laboral con las estadísticas nacionales y recategorizar la variable. Se podría observar a su vez las edades, género, nivel educativos y situación 

