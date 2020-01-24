

dados_raw <- read_excel("C:/Users/aguia/Downloads/Solar_Brazil_Database.xlsx")

dados <- dados_raw %>% 
  mutate(address = paste(municipios, uf, cep))

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

dados$long <- NA
dados$lat <- NA

for(m in 1:length(dados$distribuidora)){
  
  dados$long[m] <- geocodeAdddress(dados$address[m])[1]
  dados$lat[m]  <- geocodeAdddress(dados$address[m])[2]
}