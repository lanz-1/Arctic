library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)





#create a big boxplot: Arctic, Tundra, Boreal Forest


#load data
arctic_slopes <- readRDS("data/variables/df_metrics.rds")
tundra_slopes <- readRDS("data/variables/tundra_slopes.rds")
boreal_slopes <- readRDS("data/variables/boreal_slopes.rds")


#add a column specifying the biome
arctic_slopes <- arctic_slopes |> dplyr::mutate(biome = "Arctic", MAE = NULL, RMSE = NULL)
tundra_slopes <- tundra_slopes |> dplyr::mutate(biome = "Tundra")
boreal_slopes <- boreal_slopes |> dplyr::mutate(biome = "Boreal")

#combine to one dataframe
all_slopes <- rbind(arctic_slopes, tundra_slopes, boreal_slopes)


#plot
ggplot(all_slopes, aes(x = biome, y = slope)) + 
  geom_boxplot(outlier.shape = NA, fill = "grey90", width = 0.3) +
  #geom_jitter(aes(color = model), width = 0.05, size = 2) +
  scale_color_manual(values = model_colors) +
  labs(title = "Modelled LAI trend slopes per biome") +
  theme_bw()






