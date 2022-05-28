library(devtools)

if(!require(boliga)){
  install_github("emilmahler/boliga",force = T)
}

library(boliga)
library(dplyr)
library(ggplot2)

houses <- boliga_webscrape_sold(min_sale_date = "2007-01-01", 
                                max_sale_date = Sys.Date(), 
                                type = "Alle", 
                                zipcodeFrom = 1000,
                                zipcodeTo = 2499)

boliger <- boliga_webscrape_sold(min_sale_date = "2017-04-01", 
                                 max_sale_date = "2017-06-30", 
                                 type = "Fritidshus", 
                                 postal_code = 4500)
glimpse(boliger)

houses %>% 
  filter(pris_kvm < 250000) %>% 
  ggplot(aes(x=salgsdato,y=pris_kvm)) +
  geom_point()
