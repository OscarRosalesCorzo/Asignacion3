# Caracas, 07/31 del 2022
# Materia: Data Science
# Profesor: Ricardo Benzecry
# Alumnos: Oscar Rosales Corzo, David Alvarez


#Dudo haber usado todas estas librerias, pero por si acaso.

library(HotDeckImputation)
library(readr)
library(tidyverse)
library(stargazer)
library(readxl)
library(haven)
library(data.table)
library(sjlabelled)
library(sjPlot)
library(mice)
library(survey)
library(skimr)
library(ggthemes)
library(ggplot2)

# Cargar la data, el directorio relativo asume que vuestro directorio
# es la carpeta donde se encuentra la data.



personas <- read_sav("encovi_personas2017_ds.sav") 


view_df(personas)
View(personas)

# Cambiar nombre de las columnas a uno manejable

new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar
personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
  # Convierte los identificadores a caracteres
  mutate(across(.cols = c("id_hogar", "id_per"),
                .fns = as.character))

# Me familiarizo con el contenido de las preguntas de las encuentas

personas$sit_econo %>% attr('label')
personas$sit_econo %>% attr('labels')

personas$trab_remun %>% attr('label')
personas$trab_remun %>% attr('labels')


personas$ing_laboral %>% attr('label')
personas$ing_laboral %>% attr('labels')

personas$grp_edad %>% attr('label')
personas$grp_edad %>% attr('labels')

# Al correr las siguentes lineas de codigo nos damos cuenta de que no hay 
# valores negativos para la variable ing_laboal

#personas %>% 
#  filter(ing_laboral <0)


# Creo conjunto de valores donantes. Escojo la variable nivel educativo
# porque hay estudios que señalan que nivel edicativo es un excelente
# predictor de ingresos, asi que logro obtener segmentos 
# relativamente precisos y balancear el tradeoff


donantes <- personas %>% 
  filter(sit_econo %in% c(1,2) & trab_remun == 1) %>% 
  filter(!ing_laboral %in% c(99,98,0)) %>% 
  group_by(sexo, grp_edad, nivel_edu) %>% 
  summarise(media_donante =  weighted.mean(ing_laboral, pesop, na.rm = T),
            n_imp = length(sexo))   #No Deberia haber missing values, pero bueno.

View(donantes)

# Variable que genera el tamano de grupo de donantes

respuesta2 <- personas %>% 
  filter(sit_econo %in% c(1,2) & trab_remun == 1) %>% 
  filter(!ing_laboral %in% c(99,98,0)) %>% 
  nrow()



# De mi grupo de donantes, tengo un ingreso promedio y el numero de observaciones
# del del grupo de donantes de cada receptor, de acuerdo a los 3 criterios
# utilizados para aproximarnos al valor real del missing value
 
# Creo la tabla que es necesaria crear y que el es objetivo de todo este trabajo

ing_laboral_imp <- personas %>% 
  left_join(donantes, by = c("sexo", "grp_edad", "nivel_edu")) %>% 
  mutate(ing_laboral = ifelse( ing_laboral %in% c(99,98,0),
                               yes = media_donante,
                               no = ing_laboral)) %>% 
  select(- media_donante)
# Un vistazo

# View(ing_laboral_imp)


# Acontinuacion, en caso de que lo que hayamos hecho sea correcto,
# las siguientes lineas de codigo deberian arrojar un dataframe con
# 0 observaciones

prueba1<- ing_laboral_imp %>% 
  filter(sit_econo %in% c(1,2) & trab_remun == 1 
         & ing_laboral %in% c(99,98,0))

View(prueba1)

# Intente hacer las cosas agarrando un valor aleatorio, no supe como, 
# probablemente no entiendo en su totalidad como usar los samples 


# skim_without_charts(prueba1)


# Ahora calculare el porcentaje de valores faltantes imputados exitosamente
# Esta pregunta puede tener 2 repuestas. Como lo revela el dataframe prueba1,
# todos los valores que debian imputarse se imputaron. Asi que en primera instancia 
# diria que la respuesta es 100%. Sin embargo, puede que la pregunta haga referencia
# a el peso relativo de los valores imputados exitosamente con respecto a la base de
# datos de la encovi. Para eso:

prueba2 <- personas %>% 
  filter(sit_econo %in% c(1,2) & trab_remun == 1 
         & ing_laboral %in% c(99,98,0))


respuesta3 <- paste(round(nrow(prueba2)/nrow(personas) * 100), "%")

# la repuesta puede ser o 100% o 5%, no estoy seguro de como interpretar la pregunta
