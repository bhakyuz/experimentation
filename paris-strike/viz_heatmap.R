library(ggplot2)
library(maps)
m <- maps::map.cities(country = 'France')
ggplot(m, aes(x=long, y=lat)) +
  geom_polygon() +
  coord_map() 
data(franceMapEnv)

t <- map('France')
         
ggplot(t)


map.cities(x = world.cities[world.cities$name=='Paris' & world.cities$country.etc=='France',])

ggplot(t, aes(x=long, y=lat)) +
  geom_sf() +
  ggplot2::coord_sf(xlim =c(2.34, 2.54  ), ylim = c(48.86, 50.86))

world.cities[world.cities$name=='Paris' & world.cities$country.etc=='France',]
