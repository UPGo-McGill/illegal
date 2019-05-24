######## POSSIBLY LEGAL #############

source("R/01 helper_functions.R")

## Properties within St Laurent & St Denis Buffers
plateau_property_CRS <- plateau_property%>%
  st_transform(26918)

st_laurent_prop <- plateau_property_CRS[lengths(st_within(plateau_property_CRS, st_laurent_buff))>0,]
st_denis_prop <- plateau_property_CRS[lengths(st_within(plateau_property_CRS, st_denis_buff))>0,]


## Maps
tm_shape(candidate_streets)+
  tm_lines(col = "grey") +
  tm_shape(st_laurent_buff[])+
  tm_fill(col="red")+
  tm_shape(st_denis_buff[])+
  tm_fill(col="green")+
  tm_shape(st_denis_prop[])+
  tm_dots(size = 0.05)+
  tm_shape(st_laurent_prop[])+
  tm_dots(size = 0.05)

