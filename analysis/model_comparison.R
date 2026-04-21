library(ncdf4)
library(metR)
library(ggplot2)
library(dplyr)
library(tidyterra)

source("./R/compare_function.R")


df_metrics <- tibble(Model = character(), MAE = numeric(), RMSE = numeric())

df_metrics <- rbind(df_metrics, compare("CABLE-POP_S3_lai"))
df_metrics
