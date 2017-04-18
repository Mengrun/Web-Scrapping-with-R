library("stringr")
library("XML")
library("xml2")

locs = read.csv("dennys_coords.csv",header = FALSE)
N = nrow(locs)
#Get the number of xmls

make_df <- function(n){
  df <- data.frame(matrix(rep(NA,n),nrow=1,ncol=n))
  colnames(df) <-  c("address","city","state","country","phone","postalcode","latitude",
                     "longtitude")
  return(df)
}
#This function is used to create new data frame, whose columns are "address",
#"city","state","country","phone","postalcode","latitude", "longtitude"
Dennys <- setNames( 
  lapply(rep(8,N), function(n) make_df(n)) , 
  paste0("Dennys_", 1:N))
#Create a sequence of data frames to store the data in xmls

dennys <- data.frame()

for (i in 1:N){
  data = read_xml(paste("data/dennys/",i,".xml",sep=""))
  count = str_extract(data, 'count=\"[0-9]*\"') %>% str_extract('[0-9]{1,}')
  count = as.numeric(count)
  #count is the number of xmls
  data1 <- data.frame(list(str_split(data,"<name>")))
  #data1 contains the listed raw data, whose first row is the parameters forgeting data
  data2 <- data1[2:(count+1),1]
  #data2 contains the real listed raw data
  countryS <- str_match(data2,"<country>.*</country>")
  #countryS is the country for selecting dennys
  Dennys_US <- data2[countryS == "<country>US</country>"]
  #Dennys_US is the strings of Dennys in US
  
  address <- str_extract_all(Dennys_US,'<address1>.*</address1>') %>% 
    str_replace_all("<address1>","") %>% 
    str_replace_all("</address1>","")
  #According to the style, we move the begining and the ending of the address string
  
  city <- str_extract_all(Dennys_US,'<city>.*</city>') %>% 
    str_replace_all("<city>","") %>% 
    str_replace_all("</city>","")
  #According to the style, we move the begining and the ending of the city string
  
  state <- str_extract_all(Dennys_US,'<state>.*</state>') %>% 
    str_extract_all('([A-Z]{2})')
  #According to the style, we extract the two capital letters of the state
  
  country <- str_extract_all(Dennys_US,'<country>.*</country>') %>% 
    str_extract_all('([A-Z]{2})')
  #According to the style, we extract the two capital letters of the country
  
  phone <- str_extract_all(Dennys_US,'<phone>.*</phone>') %>% 
    str_extract_all('[\\(]([2-9][0-9]{2})[\\)][- .]([0-9]{3})[- .]([0-9]{4})')
  #According to the style, we extract the numbers of phone
  
  postalcode <- str_extract_all(Dennys_US,'<postalcode>.*</postalcode>') %>% 
    str_extract_all('([0-9]{5})')
  #According to the style, we extract the numbers of postalcode
  
  lat <- str_extract_all(Dennys_US,'<latitude>.*</latitude>') %>% 
    str_extract_all('([0-9]{1,})([.]{0,})([0-9]{1,})')
  #According to the style, we extract the numbers of latitude
  
  long <- str_extract_all(Dennys_US,'<longitude>.*</longitude>') %>% 
    str_extract_all('([-][0-9]{1,})([.]{0,})([0-9]{1,})')
  #According to the style, we extract the numbers of longtitude
  
  Dennys[[i]] <- cbind(address,city,state,country,phone,postalcode,lat,long)
  #The ith Dennys contains the data from ith xml
  dennys <- rbind(dennys,Dennys[[i]])
  #Combine the Dennys and get the whole data frame dennys, which has repeats.
}
dennys <- unique(dennys)
#Get the dennys without repeat
save(dennys, file = "data/dennys.RData")
#Save data as RData