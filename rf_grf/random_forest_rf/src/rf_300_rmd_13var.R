
library(sf)
library(stars)
library(leaflet)
library(leafem)
library(dplyr)
library(raster)
library(RColorBrewer)
library(randomForest)
library(rfUtilities)
library(tidyr)
library(caret)
library(corrplot)
library(ggplot2)


# # path de 
dataFolder <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/"


##### Carga de shapefile, Carga de raster (variables para el modelo)
ptos <- st_read(paste0(dataFolder,"input/ESC_300/random_points_300.shp"))
dplyr::select(ptos, RASTERVALU) -> nptos
bands = c("Blue","Green","Red","NIR","EVI","SAVI","VV","VH","sVVmVH","VVdVH","MEAN","ENTRO","CONTR")
x <- read_stars(paste0(dataFolder,"input/MERGED_RASTER_100_Resample_CEDA_13_BANDS.tif"), RasterIO = list(bands))
(x.spl = split(x, "band"))
names(x.spl) <- bands
st_crs(x.spl)<- st_crs(nptos)

# Extraer valores a los puntos
st_extract(x.spl, nptos) %>% st_as_sf() -> datasample

# Creación del atributo y carga de datos 
datasample$AGB_Kg= nptos$RASTERVALU 
(datasample = as.data.frame(datasample))
head(datasample)
datasample %>% drop_na() -> nsample

#### Correlación

corsample <- nsample[, !names(nsample) %in% "geometry"]
correlation_matrix <- cor(corsample)
cor_with_AGB <- correlation_matrix["AGB_Kg", ]
cor_with_AGB <- cor_with_AGB[order(abs(cor_with_AGB), decreasing = TRUE)]

cor_with_AGB_rounded <- round(cor_with_AGB, digits = 2)

cor_table <- as.table(cor_with_AGB_rounded)

print(cor_table, main = "Correlación con AGB", col = ifelse(cor_with_AGB > 0, "blue", "red"))

barplot(cor_with_AGB, main = "Correlación con AGB", col = ifelse(cor_with_AGB > 0, "blue", "red"))

rownames(correlation_matrix) <- ifelse(rownames(correlation_matrix) == "AGB_Kg", "AGB", rownames(correlation_matrix))
colnames(correlation_matrix) <- ifelse(colnames(correlation_matrix) == "AGB_Kg", "AGB", colnames(correlation_matrix))

##### Graficar la matriz de correlación con corrplot
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addrect = 3, # añade números
         text.col = "black", # color del texto
         number.cex = 0.7, # tamaño del número
         tl.cex = 0.7, # tamaño de las etiquetas
         addCoef.col = "black", # color de los coeficientes
         col = colorRampPalette(c("blue", "white", "red"))(100)) # paleta de colores


##### Optimización de parametros RF
results_mtry_optim <- rf.mtry.optim(AGB_Kg ~ Blue + Green + Red + NIR + EVI + SAVI + VV + VH + sVVmVH + VVdVH + MEAN + ENTRO + CONTR, datasample)

##### Dividir los datos en conjunto de entrenamiento (70%) y conjunto de validación (30%)
set.seed(123)  # Para reproducibilidad
train_indices <- createDataPartition(nsample$AGB_Kg, p = 0.7, list = FALSE)
train_data <- nsample[train_indices, ]
validation_data <- nsample[-train_indices, ]

##### Generacion modelo
BA.rf <- randomForest(AGB_Kg ~ Blue + Green + Red + NIR + EVI + SAVI + VV + VH + sVVmVH + VVdVH + MEAN + ENTRO + CONTR, data = train_data, importance = TRUE, ntree = 1000, mtry = 2 )

##### Importancia variables
varImpPlot(BA.rf)
BA.rf$importance


importance_df <- data.frame(
  Variable = rownames(BA.rf$importance),
  PercentIncMSE = BA.rf$importance[, "IncNodePurity"] / sum(BA.rf$importance[, "IncNodePurity"]) * 100
)

# Ordenar por importancia (opcional)
importance_df <- importance_df %>% arrange(desc(PercentIncMSE))

# Crear el gráfico de barras con etiquetas de %IncMSE
ggplot(importance_df, aes(x = reorder(Variable, PercentIncMSE), y = PercentIncMSE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(PercentIncMSE, 2)), vjust = -0.5, size = 3.5) +  # Agregar etiquetas con %IncMSE
  labs(
    title = "Importancia de Variables en el Modelo Random Forest",
    x = "Variable",
    y = "% de Importancia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


       



# # Evaluación errores
predictions_validation_BA.rf <- predict(BA.rf, newdata = validation_data)

mse_validation_BA.rf <- mean((validation_data$AGB_Kg - predictions_validation_BA.rf)^2)

mae_validation_BA.rf <- mean(abs(validation_data$AGB_Kg - predictions_validation_BA.rf))


r_squared_validation_BA.rf <- 1 - sum((validation_data$AGB_Kg - predictions_validation_BA.rf)^2) / sum((validation_data$AGB_Kg - mean(validation_data$AGB_Kg))^2)

print(paste("Validation MSE:", mse_validation_BA.rf))
print(paste("Validation MAE:", mae_validation_BA.rf))
print(paste("Validation R-squared:", r_squared_validation_BA.rf))

# # Predicciones del modelo
new_validation_data <- validation_data
new_validation_data$predictions_AGB_Kg <- predict(BA.rf, newdata = validation_data)
predictions_validation_BA.rf <- predict(BA.rf, newdata = validation_data)

pr<- predict(x.spl,BA.rf)

leaflet(data=ptos, options = leafletOptions(maxZoom=1000000)) %>%
  addProviderTiles("OpenStreetMap") %>%
  addStarsImage(pr, band=1, project = FALSE, maxBytes = 10000000) %>% 
  addAwesomeMarkers(~E, ~N, popup = ~as.character(T), label = ~as.character(T))


write_stars(pr, "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/output/RF_PT300_VAL13_NTREE1000_MTRY2.tif")

##### Cálculo de AGB

# Rutas de los archivos
raster_path <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/output/RF_PT300_VAL13_NTREE1000_MTRY2.tif"
shp_path <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/input/shp/pmf.shp"
output_path <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/output/RF_PT300_VAL13_NTREE1000_MTRY2_clip.tif"
output_txt <- "D:/JUAN/UN/TESIS/DATOS_FINALES_TESIS/R/random_forest_rf/output/RF_PT300_VAL13_NTREE1000_MTRY2_clip_AGB.txt"

# Cargar el raster y el shapefile
raster_data <- raster(raster_path)
shp_data <- shapefile(shp_path)

# Crear una máscara a partir del shapefile
raster_masked <- mask(raster_data, shp_data)

# Guardar el raster recortado
writeRaster(raster_masked, filename=output_path, format="GTiff", overwrite=TRUE)

# Calcular la suma de los valores de píxel
sum_pixels <- sum(values(raster_masked), na.rm=TRUE)

# Guardar la suma en un archivo de texto
writeLines(as.character(sum_pixels), con=output_txt)



# # Generacion de coordenadas para export 
new_validation_data <- new_validation_data %>% 
  mutate(E = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         N = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
new_validation_data <- st_drop_geometry(new_validation_data)
new_validation_data$geometry = NULL 

# # guarda resultados de la prediccion
write.csv(new_validation_data, file=paste0(dataFolder,'output/predict_BA_rf.csv'))


























