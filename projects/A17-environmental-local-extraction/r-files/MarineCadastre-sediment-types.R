#' Preliminary exploration into Marine Cadastre Sediment Types and how it might have been used in 
#' A17 analysis.
#' 
#' Most of this isn't really working, it was more of a scratch document for figuring some of this
#' stuff out, but didn't really develop into anything for A17.

# Setup ----
library(tidyverse)

sediment <- read_csv("../../../GIS/bottom-type/MCadastre-sediment-texture.csv", guess_max = 1e6) %>%
  rename(OID = `OID_`,
         CLASS = `classifica`,
         OBJID = `objectid`,
         X = POINT_X,
         Y = POINT_Y,
         LON = Longitude,
         LAT = Latitude)

table(sediment$CLASS)

sediments <- sediment %>%
  separate_rows(CLASS, sep = ",")

table(sediments$CLASS)

sediments %>%
  mutate(CLAY = ifelse(grepl("clay", CLASS, ignore.case = T), TRUE, FALSE),
         SAND = ifelse(grepl("sand", CLASS, ignore.case = T), TRUE, FALSE),
         SILT = ifelse(grepl("silt", CLASS, ignore.case = T), TRUE, FALSE),
         GRAVEL = ifelse(grepl("gravel", CLASS, ignore.case = T), TRUE, FALSE),
         MUD = ifelse(grepl("mud", CLASS, ignore.case = T), TRUE, FALSE),
         SEDIMENT = ifelse(grepl("sediment", CLASS, ignore.case = T), TRUE, FALSE)) %>%
  arrange(CLASS) %>% select(CLASS, CLAY:SEDIMENT) %>% distinct() %>% print(n = 50)


ggplot(sediments, aes(LON, LAT, col = CLASS)) +
  geom_point(size = 0.5) +
  annotation_map(map_data("world"), col = "gray60", fill = "gray90") +
  theme_bw()

extent = data.frame(ymin = 15, ymax = 45, xmin = -98, xmax = -63)

ggplot(sediments, aes(LON, LAT, col = CLASS)) +
  geom_point(size = 0.5) +
  geom_rect(data = extent, 
            mapping = aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), 
            fill = NA, col = "red", inherit.aes = FALSE) +
  annotation_map(map_data("world"), col = "gray60", fill = "gray90") +
  theme_bw()




# K Nearest Neighbors
library(caret)
library(class)

knn_data <- sediments %>%
  select(CLASS, LON, LAT)

set.seed(100)

trainIndex <- createDataPartition(knn_data$CLASS,
                                  times = 1,
                                  p = 0.8,
                                  list = FALSE)

train <- knn_data[trainIndex,]
test  <- knn_data[-trainIndex,]

# Using KNN to interpolate based on the K Nearest Neighbors (Sediment Type)
knnModel <- train(CLASS ~ .,
                  data = train,
                  method = "knn",
                  trControl = trainControl(method = "cv"),
                  tuneGrid = data.frame(k = c(7:11)))
knnFinal <- knn3(CLASS ~ ., data = knn_data, k = knnModel$bestTune$k)
knn3 <- knn3(CLASS ~ ., data = knn_data, k = 3)

plot(knnFinal)

grid <- expand.grid(LON = seq(extent$xmin, extent$xmax, by = .1),
                    LAT = seq(extent$ymin, extent$ymax, by = .1)) %>% data.frame()

predictions <- predict(knnFinal, grid)
predictionsk3 <- predict(knn3, grid)

ggplot(predictions, aes(LON, LAT, col = ))
ggplot(predictionsk3, aes(LON, LAT, col = ))

data.frame(predictions)

grid %>%
  mutate(class = names(data.frame(predictions))[max.col(predictions)],
         class = gsub("[.]"," ",class)) %>%
  ggplot(aes(LON,LAT, col = class)) +
  geom_point() +
  annotation_map(map_data("world"), col = "gray70", fill = "gray90") +
  theme_bw()

grid %>%
  mutate(class = names(data.frame(predictionsk3))[max.col(predictionsk3)],
         class = gsub("[.]"," ",class)) %>%
  ggplot(aes(LON,LAT, col = class)) +
  geom_point() +
  annotation_map(map_data("world"), col = "gray70", fill = "gray90") +
  theme_bw()

library(terra)
library(tidyterra)

EEZ <- vect("../../../GIS/USEEZ-AtlCarib.shp")
plot(EEZ)
vgrid <- grid %>%
  mutate(class.opt = names(data.frame(predictions))[max.col(predictions)],
         class.opt = gsub("[.]"," ",class.opt),
         class.k3 = names(data.frame(predictionsk3))[max.col(predictionsk3)],
         class.k3 = gsub("[.]"," ",class.k3))

gridvect <- vect(vgrid, geom = c("LON", "LAT"))

gridEEZ <- intersect(gridvect, EEZ)

# Full Extent
gridEEZ %>%
  ggplot(aes(col = class.k3)) +
  geom_spatvector(size = 0.5, shape = 15) +
  annotation_map(map_data("world"), col = "black", fill = "gray90") +
  coord_sf() +
  theme_bw() + theme(panel.grid = element_blank())

# Atlantic
gridEEZ %>%
  ggplot(aes(col = class.k3)) +
  geom_spatvector(size = 0.75, shape = 15) +
  annotation_map(map_data("world"), col = "black", fill = "gray90") +
  coord_sf(xlim = c(-85, -65), ylim = c(25, 45)) +
  labs(col = "Sediment Type") +
  theme_bw() + theme(panel.grid = element_blank())

# Gulf
gridEEZ %>%
  ggplot(aes(col = class.k3)) +
  geom_spatvector(size = 0.75, shape = 15) +
  annotation_map(map_data("world"), col = "black", fill = "gray90") +
  coord_sf(xlim = c(-98, -80), ylim = c(24, 31)) +
  labs(col = "Sediment Type") +
  theme_bw() + theme(panel.grid = element_blank())

# Caribbean
gridEEZ %>%
  ggplot(aes(col = class.k3)) +
  geom_spatvector(size = 2.75, shape = 15) +
  annotation_map(map_data("world"), col = "black", fill = "gray90") +
  coord_sf(xlim = c(-69, -64), ylim = c(15, 22)) +
  labs(col = "Sediment Type") +
  theme_bw() + theme(panel.grid = element_blank())
