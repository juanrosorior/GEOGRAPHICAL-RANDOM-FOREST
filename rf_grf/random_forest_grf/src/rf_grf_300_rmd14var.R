
library(openxlsx)
library(GWmodel)      
library(plyr)         
library(sp)           
library(spdep)        
library(RColorBrewer) 
library(classInt)     
library(raster)       
library(grid)         
library(gridExtra)    
library(ggplot2)    
library(tidyverse)   
library(SpatialML)
library(sf)
library(stars)
library(openxlsx)
library(dplyr)

# # Ruta datos 
dataFolder <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_grf/"


# # Carga de shapefile, Carga de raster (variables para el modelo)

ptos <- st_read(paste0(dataFolder,"input/ESC_300/random_points_300_70.shp"))
ptos_v <- st_read(paste0(dataFolder,"input/ESC_300/random_points_300_30.shp"))
ptos_tot<-st_read("D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/SHP/grf_pts_pred_14var_label.shp")
bands <- c("Blue", "Green", "Red", "NIR", "EVI", "SAVI", "VV", "VH", "sVVmVH", "VVdVH", "MEAN", "ENTRO", "CONTR","DSM")
x <- read_stars(paste0(dataFolder,"input/MERGED_RASTER_100_Resample_CEDA_14_BANDS.tif"), RasterIO = list(bands))
(x.spl <- split(x, "band"))
names(x.spl) <- bands

# # Selección varible dependiente
nptos <- dplyr::select(ptos, RASTERVALU)
nptos_v <- dplyr::select(ptos_v, RASTERVALU)

nptos
# # Igualar sistemas de coordenadas
st_crs(x.spl) <- st_crs(nptos)
st_crs(x.spl) <- st_crs(nptos_v)


# # Extraer valores de pixel a los puntos de entrenamiento
st_extract(x.spl, nptos) %>% st_as_sf() -> datasample
datasample$RASTERVALU <- nptos$RASTERVALU


# # Extraer valores de pixel a los puntos de validación
st_extract(x.spl, nptos_v) %>% st_as_sf() -> datasample_v
datasample_v$RASTERVALU <- nptos_v$RASTERVALU



###### PREPARACION DE LOS DATOS ENTRENAMIENTO
tract_data<-datasample
tract_data <- na.omit(tract_data)
tract_data <- tract_data[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM","RASTERVALU")]
tract_data <- tract_data %>% 
  mutate(E = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         N = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
tract_data <- st_drop_geometry(tract_data)
final_data <- as.data.frame(tract_data[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM","RASTERVALU","E","N")])
Coords <- final_data[,16:17]
final_data_df <- final_data


###### PREPARACION DE LOS DATOS VALIDACION
tract_data_v<-datasample_v
tract_data_v <- na.omit(tract_data_v)
tract_data_v <- tract_data_v[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM","RASTERVALU")]
tract_data_v <- tract_data_v %>% 
  mutate(E = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         N = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
tract_data_v <- st_drop_geometry(tract_data_v)
final_data_v <- as.data.frame(tract_data_v[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM","RASTERVALU","E","N")])
Coords_v <- final_data_v[,16:17]
final_data_df_v <- final_data_v

# # OPTIMIZACION PARAMETROS GRF
results_mtry_optim <- rf.mtry.optim(RASTERVALU ~ Blue + Green + Red + NIR + EVI + SAVI + VV + VH + sVVmVH + VVdVH + MEAN + ENTRO + CONTR + DSM, final_data)


# # GENERACION DE MODELO
model_1 <- grf(RASTERVALU ~ Blue + Green + Red + NIR + EVI + SAVI + VV + VH + sVVmVH + VVdVH + MEAN + ENTRO + CONTR+ DSM,
               dframe = final_data_df, kernel = "adaptive", bw = 50, coords = Coords, 
               ntree = 1000, mtry= 1, forests = TRUE)






##### IMPORTANCIA DE LAS VARIABLES

model_1$Local.Variable.Importance

# Definir path
file_path <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_grf/output/imp_grf_14var_300pt_ntree1000_mtry1.xlsx"

#Escribir Excel con la importancia de variables
write.xlsx(model_1$Local.Variable.Importance, file_path, rowNames = FALSE)

# Mensaje de confirmación de la exportanción
cat("Dataframe exported to Excel successfully:", file_path, "\n")



# # Guarda resultados del modelo
capture.output(model_1, file = paste0(dataFolder,"output/results_model_grf.txt")) 



# # predicciones usando el modelo anterior (validation data)
valid_data <- final_data_df_v

valid_data$pred_biomasa <- predict.grf(model_1, final_data_df_v, x.var.name="E", y.var.name="N", local.w=1, global.w=0)
valid_data$pred_biomasa

valid_data

##### EVALUACION DE ERRORES
squared_residuals <- (valid_data$RASTERVALU - valid_data$pred_biomasa)^2
total_sum_squares <- sum((valid_data$RASTERVALU - mean(valid_data$RASTERVALU))^2)
r_squared <- 1 - sum(squared_residuals) / total_sum_squares

mse_validation <- mean((valid_data$RASTERVALU - valid_data$pred_biomasa)^2)
mae_validation <- mean(abs(valid_data$RASTERVALU - valid_data$pred_biomasa))


cat("R-squared:", r_squared, "\n")
cat("MSE :", mse_validation, "\n")
cat("MAE :", mae_validation, "\n")




###### PREDICCION SOBRE PUNTOS QUE CUBREN EL ÁREA DE ESTUDIO
st_crs(x.spl) <- st_crs(ptos_tot)

st_crs(x.spl)==st_crs(ptos_tot)

st_extract(x.spl, ptos_tot) %>% st_as_sf() -> datasample_tot

datasample_tot

tract_data_tot<-datasample_tot
tract_data_tot <- na.omit(tract_data_tot)
tract_data_tot <- tract_data_tot[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM")]
tract_data_tot <- tract_data_tot %>% 
  mutate(E = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         N = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
tract_data_tot <- st_drop_geometry(tract_data_tot)
final_data_tot <- as.data.frame(tract_data_tot[,c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR","DSM","E","N")])
Coords_tot <- final_data_tot[,15:16]
final_data_df_tot <- final_data_tot

final_data_df_tot

final_data_df_tot$pred_biomasa <- predict.grf(model_1, final_data_df_tot, x.var.name="E", y.var.name="N", local.w=1, global.w=0)

final_data_df_tot



pts_sf <- st_as_sf(final_data_df_tot, coords = c("E", "N"), crs = 3116)

# Ruta y nombre del archivo de salida
output_shapefile <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_grf/output/ptos_var14_ptos300_ntree1000_mtry1_.shp"

# Guardar como shapefile
st_write(pts_sf, output_shapefile)




##### GRAFICO IMPORTANCIA VARIABLES

library(ggplot2)
library(tidyr)
library(openxlsx)
library(dplyr)

# Cargar los datos desde el archivo Excel
file_path <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_grf/output/imp_grf_14var_300pt_ntree1000_mtry1.xlsx"
data <- read.xlsx(file_path)

# Convertir los datos a formato largo para ggplot2
data_long <- gather(data, key = "Variable", value = "Importance")

# Normalizar las importancias para compararlas con porcentajes
total_importance <- sum(data_long$Importance)
data_long <- data_long %>%
  mutate(Normalized_Importance = (Importance / total_importance) * 100)

# Crear un gráfico de barras de las importancias normalizadas de las variables
ggplot(data_long, aes(x = reorder(Variable, Normalized_Importance), y = Normalized_Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Establecer el color de las barras
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Importancia Normalizada de las Variables", x = "Variables", y = "Importancia (%)") +
  guides(fill = FALSE)  # Ocultar la leyenda de colores si no es necesaria

# Guardar el gráfico
ggsave("grafico_barras_importancia_normalizada.png", width = 10, height = 6)















# Verificar el resultado
if (file.exists(output_shapefile)) {
  cat("Shapefile guardado correctamente:", output_shapefile, "\n")
} else {
  cat("Error al guardar el shapefile:", output_shapefile, "\n")
}











