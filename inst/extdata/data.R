# Initialization
## load packages
library(reshape2) #To use the melt function
library(sp) #To use the spplot function
library(raster)

## load shapefile data
path <- "inst/extdata/"
mit_pu <- raster::shapefile(paste0(path, "/Mitchell.shp"))

## load .csv data
pu_data <- data.table::fread(file = paste0(path, "Mitchell_pu.csv"),
                             data.table = FALSE)
features_data <- data.table::fread(file = paste0(path, "Mitchell_features.csv"),
                                   data.table = FALSE)
dist_features_data <- data.table::fread(file = paste0(path, "Mitchell_dist_features.csv"),
                                        data.table = FALSE)
threats_data <- data.table::fread(file = paste0(path, "Mitchell_threats.csv"),
                                  data.table = FALSE)
dist_threats_data <- data.table::fread(file = paste0(path, "Mitchell_dist_threats.csv"),
                                       data.table = FALSE)
bound_data <- data.table::fread(file = paste0(path, "Mitchell_boundary.csv"),
                                data.table = FALSE)
sensitivity_data <- data.table::fread(file = paste0(path, "Mitchell_sensibility.csv"),
                                      data.table = FALSE)


## create stack with features
r <- raster(ncol=500, nrow=500)
extent(r) <- extent(mit_pu)

for(i in 1:40){
  if(i == 1){
    mit_raster <- rasterize(mit_pu, r, field = colnames(mit_pu@data)[7 + i])
    mit_raster@data@names <- colnames(mit_pu@data)[7 + i]
    mit_features <- stack(mit_raster)
  }
  else{
    mit_features[[i]] <- rasterize(mit_pu, r, field = colnames(mit_pu@data)[7 + i])
    mit_features[[i]]@data@names <- colnames(mit_pu@data)[7 + i]
  }
}

## create stack with threats
dist_threats <- reshape2::dcast(dist_threats_data,
                                pu~threat,
                                value.var = "amount",
                                fill = 0)

mit_pu$threat_1 <- dist_threats[, "1"]
mit_pu$threat_2 <- dist_threats[, "2"]
mit_pu$threat_3 <- dist_threats[, "3"]
mit_pu$threat_4 <- dist_threats[, "4"]

for(i in 1:4){
  if(i == 1){
    mit_raster <- rasterize(mit_pu, r, field = colnames(mit_pu@data)[length(colnames(mit_pu@data)) - 4 + i])
    mit_raster@data@names <- colnames(mit_pu@data)[length(colnames(mit_pu@data)) - 4 + i]
    mit_threats <- stack(mit_raster)
  }
  else{
    mit_threats[[i]] <- rasterize(mit_pu, r, field = colnames(mit_pu@data)[length(colnames(mit_pu@data)) - 4 + i])
    mit_threats[[i]]@data@names <- colnames(mit_pu@data)[length(colnames(mit_pu@data)) - 4 + i]
  }
}

plot(mit_threats)

# Exports
## save data
save(mit_pu, file = "data/mit_pu.rda", compress = "xz")
save(mit_features, file = "data/mit_features.rda", compress = "xz")
save(mit_threats, file = "data/mit_threats.rda", compress = "xz")
