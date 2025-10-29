library(sf)
library(tmap) 
library(here)
#library(raster)
library(sp)
library(tmaptools)
library(readxl)
library(tidyverse)
library(terra)


#set directory to directories where I've stored US, CA and SF Bay shapefiles
US<-sf::st_read(here("data","mapping","nolakes","nolakes.shp"))

#set directory and pull in csv with all moored sensor coordinates
coords<-read.csv(here("data","mapping","petersonlocations.csv"))%>%
  dplyr::select(-Location)%>%
  mutate(DataType = "Cruise data")%>%
  rename(Lon=Long)%>%
  filter(Station %in% c("18","22","27","32"))#create a station coordinates index data frame

mussellocs<-readxl::read_xlsx(here("data","mapping","musselstations_v2.xlsx"))%>%
  filter(InStudy=="y")%>%
  rename(Station = SiteCode2)%>%
  dplyr::select(Station,Lon,Lat)%>%
  mutate(DataType = "Mussel data")%>%
  bind_rows(coords)%>%
  st_as_sf(coords= c(x="Lon",y="Lat"),crs=4269)%>%
  filter(!Station %in% c("21","36"),
         is.na(DataType)==FALSE)


# input the location where you've stored the Bay Bathymetry file
#download from here https://data.ca.gov/dataset/san-francisco-bay-and-sacramento-san-joaquin-delta-dem-for-modeling-version-4-1-superseded/resource/8c12169a-9a14-4dcf-b2a0-63abb199b31f?inner_span=True
nc_url<- " your bathymetry path here"
dem_rast <- rast(nc_url)

# Inspect or plot
#plot(dem_rast)




#function to create bounding box for map (coordinates of top left, bottom right corners)
generatebb<-function(y1,x1,y2,x2){
  xy<-data.frame(Long =c(x1,x2,x2,x1),
                 Lat = c(y1,y1,y2,y2),
                 Verts = c("A","A","A","A"))
  newbb<-sfheaders::sf_polygon(obj=xy,x="Long",y="Lat",polygon_id = "Verts")
  sf::st_crs( newbb ) <- 4269
  newbb
}


station.bb<-generatebb(38,-122.6,37.41,-121.95) #south bay bounding box
depthbreaks<-seq(-40,0,by=5)
littlemap<-tm_shape(dem_rast, bbox = tmaptools::bb(station.bb))+
  tm_raster(col.scale=tm_scale_continuous(values=c("royalblue","white"),limits = c(-40,0),outliers.trunc = c(-40,0)),
            col.legend = tm_legend(title="Bathymetry (m)"))+
  tm_shape(US)+ #pulll in bathymetry data, set bounding box
  tm_polygons(fill="lightgray")+ #fill that shapefile with gray
  tm_shape(mussellocs)+ #pull in station coords
  tm_symbols(shape=21,fill="DataType",
             fill.scale = tm_scale(values=c("purple", "darkorange"),levels.drop=TRUE),
             fill.legend = tm_legend(title = "Data type"),
             size = 1.5)+ 
  tm_shape(mussellocs)+
  tm_text(text = "Station",size=0.8,col="white")+
  tm_graticules(lines=FALSE) +  # Add lat/lon grid
  tm_scalebar(position = c("left", "bottom"),bg=TRUE,bg.color = "white")     

tiff(here("Figures","Fig1.tiff"),width=8,height=10,units="in",res=600)
littlemap
dev.off()

