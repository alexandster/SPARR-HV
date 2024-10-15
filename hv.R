#install.packages("gt")
#install.packages("gtExtras")
library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(tmap)
library(ggplot2)
library(spatstat)
library(spatstat.utils)


# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read Hopi reservation boundary
geom <- st_read("hopi_buf_v2.shp") %>%
  st_transform(., crs = 4326) %>%
  st_coordinates(.) %>%
  as.data.frame(.)

#read table
df <- read.csv("HV_alex.csv")

#frequency
table(df$Status)

#separate 
df_0 <- subset(df, Status == 0) # control
df_1 <- subset(df, Status == 1) # case


#ppp object window
w <- owin(poly=list(x=rev(geom$X),y=rev(geom$Y))) #window
gHV <- ppp(x = df$longs, y = df$lats, window = w, marks = df$Status)

#ppp
df_0_ppp <- ppp(df_0[, "longs"], df_0[, "lats"], window = w)
df_1_ppp <- ppp(df_1[, "longs"], df_1[, "lats"], window = w)

#risk surface
rho <- risk(f = df_1_ppp, g = df_0_ppp, h0=OS(df_0_ppp), adapt=TRUE, tolerate=TRUE,
               hp=OS(df_0_ppp), pilot.symmetry="pooled", davies.baddeley=0.05)

plot(rho, tol.show = TRUE)

#risk surface to raster
r <- raster(rho$rr)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(r, "rho", format = "GTiff", overwrite=TRUE)

#classify points
rho.class <- tol.classify(rho, cutoff = 0.05)

#plot
plot(rho)
points(rho.class$fin,col=2)
points(rho.class$fout)

#cluster report table
ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
Cases <- lengths(rho.class[["finsplit"]])     #case count
Controls <- lengths(rho.class[["ginsplit"]])  #control count
N <- Cases + Controls                         #point count
Risk <- Cases/N                               #

#contours to sf
pcpolys <- rho.class$pcpolys %>%
  lapply(., FUN = st_as_sf) %>%
  do.call(rbind, .) %>%
  st_set_crs(26912)
pcpolys$ID <- 1:length(rho.class[["finsplit"]])
st_write(pcpolys,"pcpolys.shp")

Area <- st_area(pcpolys) #Take care of units
Case_density <- Cases/Area                    #

#style it
df_res <- data.frame(ID, N, Cases, Controls, Risk, Case_density, Area) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Clusters of HV in Hopi reservation")
df_res

