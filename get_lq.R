library(rvest)
library(stringr)

base_url = "http://www.lq.com"
listing_page = "/en/findandbook/hotel-listings.html"

# read the csv files of states names
data = read.csv("lq_states.csv", sep =" ")
states = as.character(data[[1]])

listings = html(paste0(base_url, listing_page))

# Function to download webpages of each hotel 
get_state_hotels = function(html, states, base_url, out_dir = "data/lq/")
{
  for(state in states)
  {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    hotels = html_nodes(html, "#hotelListing a")
    
    # Find the start of a state's hotels by finding its name in the node text
    # +2 to skip State and Back to Top anchor tags
    start = 2 + which(html_text(hotels) %>% str_trim() %in% paste("Hotels in", state))

     stopifnot(length(start) == 1)
    
    # State label anchor have no href result in NA
    urls = html_attr(hotels, "href")
    label_index = which(is.na(urls))
    end = label_index[label_index > start] %>% min() - 1
    
    for(url in urls[start:end])
    {
      download.file(paste0(base_url,url),
                    destfile = paste0(out_dir,basename(url)),
                    quiet = TRUE)
     Sys.sleep(5)
    }
  }
}

# Call function to download all the webpages of la quinta in the USA
get_state_hotels(listings, states, base_url)  


