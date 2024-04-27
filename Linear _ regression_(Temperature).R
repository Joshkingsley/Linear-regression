#This is a linear regression model for minimum temperatures for 2 months(Jan\Feb)

library(raster)
library(ggplot2)

Tif_file<-"C:/Users/kings/Downloads/wc2.1_10m_tmin_01.tif"
Tif_2_file<-"C:/Users/kings/Downloads/wc2.1_10m_tmin/wc2.1_10m_tmin_02.tif"

imported_ras<-raster(Tif_file)
imported_2_ras<-raster(Tif_2_file)

Vec_ras<-as.vector(imported_ras)
Vec_2<-as.vector(imported_2_ras)

mod<-lm(Vec_ras~Vec_2,na.rm=TRUE)

summary(mod)
