library(rvest)
library(xml2)
library(stringr)


dir = paste0(getwd(), "/data/lq")
# Read all the files 
files = list.files(dir, full.names =TRUE)

# This function is to be used in function extract_data to provide info related to
# amenities and services (e.g. internet availability)
amenity_info = function(html, detail) {
  amenity = html_nodes(html, ".hotelDetailsFeaturesAmenity")[[1]] %>% 
    html_nodes("li") %>%
    html_text() 
  has.detail = amenity %>% 
    str_detect(detail) %>% 
    any()
  return(has.detail)
}

# This function is to be used in function extract_data to provide info related to
# hotel details (i.e. # of floors, # of suites, # of rooms)
hotel_details = function(html, detail) {
  hotel.detail = html_nodes(html, ".hotelFeatureList") %>% 
    html_nodes("li") %>% 
    html_text
  exist.detail = hotel.detail %>% str_detect(detail)
  if(any(exist.detail)) {
    loc = exist.detail %>% which()
    extract_detail = hotel.detail[loc] %>% 
      str_replace(detail, "") %>% 
      as.numeric()
    return(extract_detail)
    
  }
  return("NA")
}

# This function is to extract the information of interests contained in one
# file. A data frame with 1 row and 12 columns is created and returned. 
extract_data = function(file) {
  d = read_html(file)
  
  # extract basic info (names, address, phone numbers, latitude, longtitude etc.)
  titles = html_nodes(d,".hotelDetailsBasicInfoTitle")
  x = titles[[1]]
  df = data.frame(
    title = html_nodes(x,"h1")[1] %>% html_text()
  )
  addr_pho_fax = html_nodes(x,"p") %>%
    html_text() %>%
    str_trim() %>%
    str_split(pattern = "\n*\n*\n")
  address = paste0(unlist(addr_pho_fax)[1],unlist(addr_pho_fax)[2]) %>%
            str_replace("[ ]{2,}", " ")
  df$address = unlist(str_split(address,",*,"))[1]
  df$city = unlist(str_split(address,",*,"))[2]
  df$state = unlist(str_split(address,",*,"))[3] %>%
             str_match(pattern = "[A-Z]{2}")
  df$postalcode = str_match(df$address,"[0-9]{5}")
  df$phone = unlist(addr_pho_fax)[5] %>%
    str_match(pattern = "[0-9][-][0-9]{3}[-][0-9]{3}[- .][0-9]{4}")
  df$fax = unlist(addr_pho_fax)[8] %>%
    str_match(pattern ="[0-9][-][0-9]{3}[-][0-9]{3}[- .][0-9]{4}")
  lat_long = html_nodes(d, ".minimap") %>% 
    html_attr("src") %>%
    str_match(pattern = "[0-9]{2}[.][0-9]{2, },-[0-9]{2,3}[.][0-9]{2, }") %>%
    str_split(pattern =",")
  df$lat = unlist(lat_long)[1] %>%
    as.numeric()
  df$long = unlist(lat_long)[2] %>%
    as.numeric()
  
  # extract information of amenities and services
  df$internet = ifelse(amenity_info(d, "Free .*?Internet"), "yes (free)", "no")
  df$swimmingpool = ifelse(amenity_info(d, "Swimming Pool"), "yes", "no")
  df$breakfast = ifelse(amenity_info(d, "Free.*? Breakfast"), "yes (free)", "no")
  
  # extract information of hotel details 
  df$floor = hotel_details(d, " Floors: ")
  df$room = hotel_details(d, "Rooms: ")
  df$suit = hotel_details(d, "Suites: ")
  
  # output data frame (containing info of one hotel)
  df
}


l = lapply(files, extract_data)  # apply extract_data to each file in /data/lq
lq.info = do.call(rbind, l)  # row combine to merge into a single data frame
save(lq.info, file = "data/lq.RData")  # Save the data frame into RData file 
