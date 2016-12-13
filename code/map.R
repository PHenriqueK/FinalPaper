ChicagoMap <- qmap("Chicago", zoom = 10, maptype = "toner", source = "stamen")

FinalMap <- ChicagoMap +
  geom_point(aes(x = Longitude, y = Latitude, colour = Primary.Type),
             data = Crime_Data) +
  #geom_point(aes(x = stations$coords.x1, y = stations$coords.x2), data = stations) +
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) + 
  guides(size = guide_legend(title = 'Crime Type'),
         colour = guide_legend(title = 'Crime Type'))
print(FinalMap)