

dados_raw <- read_excel("Solar_Brazil_Database.xlsx")

dn <- dados_raw %>% 
  group_by(uf) %>% 
  summarise(n = sum(potencia)) %>% 
  as.data.frame()

shp <- readOGR("./BRUFE250GC_SIR.shp", stringsAsFactors=FALSE, encoding="UTF-8")



shp$NM_ESTADO <- c('AC','AL','AP','BA','CE','DF','ES','GO','MA','MG','MS','MG','PA','PB','PE','PI','RJ','RN','RS','RR','SC','SP','SE','TO','PR','RO','AM')

shp = merge(shp, dn, by.y = "uf", by.x = "NM_ESTADO")

shp$n <- as.numeric(shp$n)


shapeData <- spTransform(shp, CRS("+proj=longlat +ellps=GRS80"))
popupas = paste("<strong>", shp$NM_ESTADO, "</strong>", "<br>", shp$n)

pal <- colorBin(heat.colors(6), domain = c(0, max(shp$n)), bins = 6) #cores do mapa

leaflet()  %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data=shapeData, weight = 2, col = 'blue', popup = popupas,opacity = 0.1,fillColor = ~pal((n))) %>% 
addLegend("topleft", pal = pal, values = (n),   title = "Legenda")



dados_raw %>% 
  filter(classe == "Comercial" | classe == "Industrial") %>% 
  group_by(ano, classe) %>% 
  summarise(pot = sum(potencia)) %>% 
  ggplot(aes(x = ano, y = pot, col = classe)) +  geom_line(size = 1.3) + 
  ylab("poted")

