require(ggmap)
require(dplyr)

### Mapping station data
stations <- read.csv('Data/all_stations_CSA.csv')

my.map <- get_map(location=c(-90.5, 
                             29, 
                             max(stations$Longitude),
                             30))
ggmap(my.map) + geom_point(aes(x=Longitude, y=Latitude, col=factor(Office_Id_Ref)), 
                           data=stations) +
  geom_text(aes(x=Longitude, y=Latitude, label=Station_Code),
            data=stations)

my.map <- get_map(location=c(-90.5, 29, -89.5, 29.5))
ggmap(my.map) + geom_point(aes(x=Longitude, y=Latitude,
                               col=factor(Office_Id_Ref)), data=stations) +
  geom_text(aes(x=Longitude, y=Latitude, label=Station_Code),
            data=stations)

head(stations)

stations$include <- TRUE
stations$include[stations$Longitude < -90.18] <- FALSE
stations$include[stations$Latitude +
                   0.58*stations$Longitude >
                   -22.52] <- FALSE
stations$include[stations$Station_Code %in%
                   c(1078, 1016, 1064, 1005, 1004,
                     1026, 1077)] <- FALSE


### Biological data
seines <- read.csv('Data/Marine_Finfish_Biological_Data_2003_to_Present_20131118_clean.csv')
# Seines: 50' x 100' = 5000 ft2 = 464.5152 m2

trawls <- read.csv('Data/Shrimp_Groundfish_Biological_Data_2003_to_Present_20131118_clean.csv')

bb.seines <- seines %>%
  # filter(Basin_Name=='Barataria') %>%
  mutate(lat.deg = floor(Latitude/10^4), 
         lat.min = floor((Latitude-lat.deg*10^4)/10^2),
         lat.sec = Latitude-lat.deg*10^4-lat.min*10^2,
         lat.dec = ifelse(Latitude > 180,
                          lat.deg + lat.min/60 + lat.sec/3600,
                          Latitude),
         lon.deg = floor(Longitude/10^4), 
         lon.min = floor((Longitude-lon.deg*10^4)/10^2),
         lon.sec = Longitude-lon.deg*10^4-lon.min*10^2,
         lon.dec = ifelse(abs(Longitude) > 180,
                          -abs(lon.deg) + lon.min/60 + lon.sec/3600,
                          -abs(Longitude)),
         unique.id = paste(lat.dec, lon.dec)) %>%
  filter(Gear_Desc == "50' seine, 1/4 in. delta mesh", YYYY < 2010, YYYY > 2004) %>%
  group_by(unique.id) %>%
  summarize(lat = first(lat.dec), lon=first(lon.dec), #gear=first(Gear_Desc),
            basin = first(Basin_Name), station = first(Station_Name),
            station_code = first(Station_Code), year=first(YYYY))
# Lat/Lon are unique identifiers for station, not different between sampling occasions

bb.trawls <- trawls %>%
  # filter(Basin_Name=='Barataria') %>%
  mutate(lat.deg = floor(Latitude/10^4), 
         lat.min = floor((Latitude-lat.deg*10^4)/10^2),
         lat.sec = Latitude-lat.deg*10^4-lat.min*10^2,
         lat.dec = ifelse(Latitude > 180,
                          lat.deg + lat.min/60 + lat.sec/3600,
                          Latitude),
         lon.deg = floor(Longitude/10^4), 
         lon.min = floor((Longitude-lon.deg*10^4)/10^2),
         lon.sec = Longitude-lon.deg*10^4-lon.min*10^2,
         lon.dec = ifelse(abs(Longitude) > 180,
                          -abs(lon.deg) + lon.min/60 + lon.sec/3600,
                          -abs(Longitude)),
         unique.id = paste(lat.dec, lon.dec)) %>%
  filter(Gear_Desc == "16' flat otter trawl", YYYY < 2010, YYYY > 2004) %>%
  group_by(unique.id) %>%
  summarize(lat = first(lat.dec), lon=first(lon.dec), #gear=first(Gear_Desc),
            basin = first(Basin_Name), station = first(Station_Name),
            station_code = first(Station_Code), year=first(YYYY))
# Lat/Lon are unique identifiers for station, not different between sampling occasions





my.map <- get_map(location=c(lat=29.5, lon=-90), zoom=8)
ggmap(my.map) + 
  geom_point(aes(x=lon, y=lat, col=basin), pch=16, data=bb.seines) +
  geom_point(aes(x=lon, y=lat, col=basin), pch=1, data=bb.trawls)





geom_text(aes(x=Longitude, y=Latitude, label=Station_Code),
            data=stations)


# Gulf Menhaden as example
# find unique seine occasions
# was species caught? if no, biomass = 0
# if yes, sizes measured for each fish?
# if no, count number of fish at each size, divide by number measured,
### multiply by number caught
# take number at size, convert to number at mass, and then sum


# need to make this flexible with weight-length conversion coefficients
calc.biomass <- function(Common_Name, Length_Meas, Total_Num, target_spp) {
  
  if(!(target_spp %in% Common_Name)) { # if target species not caught in seine
    biomass <- 0
  } else {
    this.spp.ind <- which(Common_Name == target_spp & Length_Meas < 96.46)
    if(length(this.spp.ind) == 0) { # if only adults caught (measuring biomass of juveniles)
      biomass <- 0
    } else if(length(Total_Num[this.spp.ind])==Total_Num[this.spp.ind[1]]) { # if lengths measured for each fish caught
      biomass <- sum(.0051*(Length_Meas[this.spp.ind]/10)^3.267) # gives biomass in g
    } else {
      meas.num.at.size <- table(Length_Meas)
      pred.num.at.size <- meas.num.at.size/length(this.spp.ind)*Total_Num[this.spp.ind[1]]
      bin.biomasses <- .0051 * (sort(unique(Length_Meas))/10) ^ 3.267
      biomass <- sum(bin.biomasses * pred.num.at.size)
    }
  }
  return(biomass)
}

Linf <- 29.2574
K <- .4
stanza.break <- 1 #year
len.break <- 29.2574*(1-exp(-.4)) * 10 # VBGF, length in mm

test <- bb.seines %>% 
  mutate(unique.sample = paste(Measure_Date, Mil_Time)) %>% 
  group_by(unique.sample) %>%
  summarize(menhad.biomass = 
              calc.biomass(Common_Name=Common_Name, Length_Meas=Length_Meas, 
                           Total_Num=Total_Num, target_spp = 'Gulf Menhaden'),
            year = first(YYYY), lat=first(Latitude), lon=first(Longitude),
            station=first(Station_Name))

test %>%
  group_by(year) %>%
  summarize(bio = mean(menhad.biomass)/464.5152) %>%
  with(mean(bio))

# Plot the stations

filter(seines, Gear_Desc=="50' seine, 1/4 in. delta mesh", Office_Id_Ref==2, Station_Name=='')
