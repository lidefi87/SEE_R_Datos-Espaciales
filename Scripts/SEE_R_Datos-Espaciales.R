## Accediendo a datos satelitales marinos con R
## Denisse Fierro Arcos
## 2022-09-04


# Cargando librerias ------------------------------------------------------
#Recuerda que puedes instalar librerias usando 
#install.packages("nombre_paquete")
library(tidyverse)
library(rerddap)
library(raster)
library(stars)

# Revisando servidores disponibles ----------------------------------------
#La funcion servers() nos devuelve una lista de servidores que podemos 
#acceder
servidores <- servers()
head(servidores)

#Podemos hacer una busqueda de estos servidores usando stringr y dplyr
servidores %>% 
  filter(str_detect(str_to_lower(name), "coastwatch"))

#Para este ejemplo usaremos Coastwatch West Coast Node (CSWC)
#Guardamos el URL de este set de datos
coastwatch_url <- servidores %>% 
  filter(short_name == "CSWC") %>% 
  select(url) %>% 
  pull()
#Verifiquemos el URL
coastwatch_url

# Buscando datos en CSWC --------------------------------------------------
#Vamos a buscar informacion en forma de grilla sobre temperatura de 
#la superficie del mar (SST por sus siglas en ingles) en CSWC
temp_sup_res <- ed_search(query = "SST", 
          which = "griddap",
          url = coastwatch_url)
#Veamos los primeros resultados
head(temp_sup_res$info)

#Podemos revisar mas informacion sobre el dataset de nuestro interes
info(temp_sup_res$info$dataset_id[1])


# Busqueda avanzada -------------------------------------------------------
#En la busqueda simple tuvimos una gran cantidad de resultados
#Podemos contarlos facilmente
temp_sup_res$info %>% 
  count()

#Con la funcion ed_search_adv() podemos hacer una busqueda mas
#especifica que nos da resultados mas relevantes
#Definamos limites espaciales y temporales para Ecuador
temp_sup_res <- ed_search_adv(query = "SST",
              #Datos en grilla
              protocol = "griddap",
              #Servidor CSWC
              url = coastwatch_url,
              #Datos mensuales
              keywords = "monthly",
              #Limites espaciales
              maxLat = 2,
              minLat = -5.2,
              maxLon = -74.9,
              minLon = -92,
              #Limites temporales
              minTime = "2010",
              maxTime = "2020-06")
head(temp_sup_res$info)

#Busquemos mas infomarcion sobre los datos del Aqua MODIS
info(temp_sup_res$info$dataset_id[5])


# Accediendo a los datos desde ERDDAP -------------------------------------
#Una vez que identificamos el set de datos que necesitamos,
#vamos a accederlos de manera remota 
temp_sup_ecu <- griddap(temp_sup_res$info$dataset_id[5],
                        #Limites temporales
                        time = c("2019-01-01", "2019-12-31"),
                        #Limites espaciales
                        latitude = c(-5.2, 2),
                        longitude = c(-92, -74.9),
                        #Acceder a la informacion en formato netcdf
                        fmt = "nc",
                        #Guardar informacion en disco
                        store = disk(path = "Data/"))
#Verifiquemos el contenido de los datos que acabamos de bajar
temp_sup_ecu


# Graficando datos --------------------------------------------------------
#Obtengamos la ruta del archivo que bajamos
archivo <- list.files("Data/", full.names = T)

#Carguemos el netcdf con raster
temp_sup_ec <- raster(archivo)

#Grafiquemos el primer mes
raster::plot(temp_sup_ec, col = RColorBrewer::brewer.pal(9, "YlOrRd"))

#Calculemos un promedio de todo el anio
temp_prom_anual <- raster::mean(temp_sup_ec, 12)
raster::plot(temp_prom_anual, col = RColorBrewer::brewer.pal(9, "YlOrRd"))


# Manipulacion de datos ---------------------------------------------------
#Capa de continentes
tierra <- rnaturalearth::ne_countries(returnclass = "sf")

temp_sup_ecu$data %>% 
  filter(lubridate::month(time) <= 4 | lubridate::month(time) == 12) %>% 
  group_by(lat, lon) %>% 
  summarise(temp_prom = mean(sstMasked, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat))+
  geom_contour_filled(aes(z = temp_prom), binwidth = 1.5)+
  scale_fill_brewer(palette = "YlOrRd")+
  geom_sf(data = tierra, inherit.aes = F)+
  lims(x = c(-92, -74.9), y = c(-5.2, 2))+
  theme_bw()
