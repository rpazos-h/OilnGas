### OIL AND GAS DATA

# This code works with public Oil and Gas Production data at the id level
# It plots some graphs by basin, resource and id

###

rm(list = ls())
gc()

setwd("/Users/usuario/Desktop/OilandGas")   

#Packages

libraries <- c('tidyverse','readxl','lmtest',
               'StatMeasures', 'sampleSelection', 
               'sandwich', 'kableExtra', 'pglm',
               'texreg','reshape','data.table', 'readr', 'zoo',
               'survival', 'stargazer','rqdatatable', 'plm')

new.packages <- libraries[!(libraries %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(libraries, require, character.only=TRUE)

select <- dplyr::select
rename <- dplyr::rename

#### Main databases ####

source('https://raw.githubusercontent.com/rpazos-h/OilnGas/main/getog.R')
source('https://raw.githubusercontent.com/rpazos-h/OilnGas/main/plot_by_group.R')
source('https://raw.githubusercontent.com/rpazos-h/OilnGas/main/barplot_bygroup.R')


datos <- getOGdata(dir='/Users/usuario/Desktop/APE/Antoine Boiron/OilandGas')

datos <- tibble(datos) %>% 
  mutate(date = as.Date(paste(ANIO,MES,1,sep="-"), format = "%Y-%m-%d"))

#### Tidy by tables ####

# Production by RECURSO
#detach(package:plyr)

oil_by_RECURSO <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(date, PROD_PET, TIPO_DE_RECURSO, SUB_TIPO_RECURSO) %>% 
  #filter(SUB_TIPO_RECURSO!="") %>% 
  group_by(date,TIPO_DE_RECURSO, SUB_TIPO_RECURSO) %>% 
  summarise(PROD_PET = sum(PROD_PET)) %>%
  mutate(PROD_PET = ((PROD_PET*6.2891)/30)/1000) %>% 
  ungroup()

gas_by_RECURSO <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(date, PROD_GAS, TIPO_DE_RECURSO, SUB_TIPO_RECURSO) %>%
  #filter(TIPO_DE_RECURSO!="") %>% 
  group_by(TIPO_DE_RECURSO, SUB_TIPO_RECURSO, date) %>% 
  summarise(PROD_GAS = sum(PROD_GAS)) %>%
  mutate(PROD_GAS = (PROD_GAS/1000)/30) %>% 
  ungroup()


# Production by cuenca

oil_by_cuenca <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(date, PROD_PET, CUENCA) %>%
  #filter(SUB_TIPO_RECURSO!="") %>% 
  group_by(CUENCA,date) %>% 
  summarise(PROD_PET = sum(PROD_PET)) %>%
  mutate(PROD_PET = ((PROD_PET*6.2891)/30)/1000) %>% 
  ungroup()

gas_by_cuenca <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(date, PROD_GAS, CUENCA) %>%
  #filter(TIPO_DE_RECURSO!="") %>% 
  group_by(CUENCA,date) %>% 
  summarise(PROD_GAS = sum(PROD_GAS)) %>%
  mutate(PROD_GAS = (PROD_GAS/1000)/30) %>% 
  ungroup()


# Production by empresa

oil_by_EMPRESA <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(ANIO,date, PROD_PET, EMPRESA) %>%
  #filter(SUB_TIPO_RECURSO!="") %>% 
  group_by(ANIO, EMPRESA,date) %>% 
  summarise(PROD_PET = sum(PROD_PET)) %>%
  mutate(PROD_PET = ((PROD_PET*6.2891)/30)/1000) %>% 
  ungroup()

gas_by_EMPRESA <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(ANIO,date, PROD_GAS, EMPRESA) %>%
  #filter(TIPO_DE_RECURSO!="") %>% 
  group_by(ANIO,EMPRESA,date) %>% 
  summarise(PROD_GAS = sum(PROD_GAS)) %>%
  mutate(PROD_GAS = (PROD_GAS/1000)/30) %>% 
  ungroup()

# Production by provincia

oil_by_PROVINCIA <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(ANIO,date, PROD_PET, PROVINCIA) %>%
  #filter(SUB_TIPO_RECURSO!="") %>% 
  group_by(ANIO,PROVINCIA,date) %>% 
  summarise(PROD_PET = sum(PROD_PET)) %>%
  mutate(PROD_PET = ((PROD_PET*6.2891)/30)/1000) %>% 
  ungroup()

gas_by_PROVINCIA <-  datos %>% 
  filter(CLASIFICACION == 'EXPLOTACION') %>% 
  select(ANIO,date, PROD_GAS, PROVINCIA) %>%
  #filter(TIPO_DE_RECURSO!="") %>% 
  group_by(ANIO,PROVINCIA,date) %>% 
  summarise(PROD_GAS = sum(PROD_GAS)) %>%
  mutate(PROD_GAS = (PROD_GAS/1000)/30) %>% 
  ungroup()


#### Variations ####

oil_by_RECURSO_var <- oil_by_RECURSO %>%
  group_by(TIPO_DE_RECURSO, SUB_TIPO_RECURSO) %>% 
  mutate(
    MoM = (PROD_PET - lag(PROD_PET)) / lag(PROD_PET),
    YoY = (PROD_PET - lag(PROD_PET, 12)) / lag(PROD_PET, 12)
  )

gas_by_RECURSO_var <- gas_by_RECURSO %>%
  group_by(TIPO_DE_RECURSO, SUB_TIPO_RECURSO) %>% 
  mutate(
    MoM = (PROD_GAS - lag(PROD_GAS)) / lag(PROD_GAS),
    YoY = (PROD_GAS - lag(PROD_GAS, 12)) / lag(PROD_GAS, 12)
  )

oil_by_cuenca_var <-  oil_by_cuenca %>% 
  group_by(CUENCA) %>% 
  mutate(
    MoM = (PROD_PET - lag(PROD_PET)) / lag(PROD_PET),
    YoY = (PROD_PET - lag(PROD_PET, 12)) / lag(PROD_PET, 12)
  )

gas_by_cuenca_var <-  gas_by_cuenca %>% 
  group_by(CUENCA) %>% 
  mutate(
    MoM = (PROD_GAS - lag(PROD_GAS)) / lag(PROD_GAS),
    YoY = (PROD_GAS - lag(PROD_GAS, 12)) / lag(PROD_GAS, 12)
  )

#### TREND PLOTS ####

# NO Convencional Oil
plot_by_group(oil_by_RECURSO %>% filter(TIPO_DE_RECURSO=='NO CONVENCIONAL'), 'date', 
              'SUB_TIPO_RECURSO', 'PROD_PET', points=TRUE)$plot + 
  labs(title="Producción OIL No Convencional \n mbb/day")+
  theme_classic()
  

# NO Convencional Oil YoY
plot_by_group(oil_by_RECURSO_var %>% filter(TIPO_DE_RECURSO=='NO CONVENCIONAL'), 'date', 
              'SUB_TIPO_RECURSO','YoY', points=F)$plot +
  labs(title="OIL No Convencional \n YoY %")+
  theme_classic() +
  geom_line(aes(linetype=SUB_TIPO_RECURSO, color=SUB_TIPO_RECURSO), size=1.5)+
  scale_y_continuous(labels = scales::percent)
  
# NO Convencional Gas
plot_by_group(gas_by_RECURSO %>% filter(TIPO_DE_RECURSO=='NO CONVENCIONAL'), 'date', 
              'SUB_TIPO_RECURSO', 'PROD_GAS', points=TRUE)$plot + 
  labs(title="Producción GAS No Convencional \n mn M3/day")+
  theme_classic()

# NO Convencional Gas YoY
plot_by_group(gas_by_RECURSO_var %>% filter(TIPO_DE_RECURSO=='NO CONVENCIONAL') %>% filter(date>'2009-12-01'), 'date', 
              'SUB_TIPO_RECURSO','YoY', points=F)$plot +
  labs(title="GAS No Convencional \n YoY %")+
  theme_classic() +
  geom_line(aes(linetype=SUB_TIPO_RECURSO, color=SUB_TIPO_RECURSO), size=1.5)+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept=0)


# Cuenca Gas YoY

plot_by_group(oil_by_cuenca_var %>% filter(date>'2009-12-01'), 'date', 
              'CUENCA','YoY', points=F)$plot +
  labs(title="Oil segun Cuenca \n YoY %")+
  theme_classic() +
  geom_line(aes(linetype=CUENCA, color=CUENCA), size=1.5)+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept=0)


plot_by_group(gas_by_cuenca_var %>% filter(date>'2009-12-01'), 'date', 
              'CUENCA','YoY', points=F)$plot +
  labs(title="GAS segun Cuenca \n YoY %")+
  theme_classic() +
  geom_line(aes(linetype=CUENCA, color=CUENCA), size=1.5)+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept=0)


### BARPLOTS ####

barplot_bygroup(oil_by_EMPRESA %>% filter(PROD_PET>20) %>% filter(ANIO==2012), by_var = 'EMPRESA', var='PROD_PET', 
                stat_fun = mean, color = 'darkgrey',order_by_stat = TRUE)$plot+
  theme_minimal()+
  labs(title="Producción de Petróleo 2012 \n mbb/day")
  
barplot_bygroup(gas_by_EMPRESA %>% filter(PROD_GAS>2) %>% filter(ANIO==2012), by_var = 'EMPRESA', var='PROD_GAS', 
                stat_fun = mean, color = 'darkgrey',order_by_stat = TRUE)$plot+
  theme_minimal()+
  labs(title="Producción de Gas 2012 \n Mn M3/day")


barplot_bygroup(oil_by_PROVINCIA %>% filter(PROD_PET>20) %>% filter(ANIO==2012) ,by_var = 'PROVINCIA', var='PROD_PET', 
                stat_fun = sum, color = 'darkorange',order_by_stat = TRUE)$plot+
  theme_minimal()+
  labs(title="Producción de Petróleo 2012 \n mbb/day")

barplot_bygroup(gas_by_PROVINCIA %>% filter(PROD_GAS>2) %>% filter(ANIO==2012) ,by_var = 'PROVINCIA', var='PROD_GAS', 
                stat_fun = sum, color = 'darkorange',order_by_stat = TRUE)$plot+
  theme_minimal()+
  labs(title="Producción de Gas 2012 \n Mn M3/day")
