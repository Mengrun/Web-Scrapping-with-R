
key = "6B962D40-03BA-11E5-BC31-9A51842CA48B"
#Key is important for extracting the data of Dennys

get_dennys_locs = function(dest, key, lat, long, radius, limit)
{
    url = paste0(
            "https://hosted.where2getit.com/dennys/responsive/ajax?&xml_request=",
            "<request>",
            "<appkey>",key ,'</appkey><formdata id="locatorsearch">',
            "<dataview>store_default</dataview>",
            "<limit>",limit,"</limit>",
            "<order>rank,_distance</order>",
            "<geolocs><geoloc><addressline></addressline>",
            "<longitude>",long,"</longitude>",
            "<latitude>",lat,"</latitude>",
            "<country>US</country></geoloc></geolocs><stateonly>1</stateonly>",
            "<searchradius>",radius,"</searchradius></formdata></request>")

    download.file(url,destfile=dest,method = "wget")
}
#This function is used to download the data of dennys, given the center of key, latitude, 
#longtitude, limit, radius.


locs = read.csv("dennys_coords.csv",header = FALSE)
#Change the working directory and read the coordinates of the centers for searching dennys
#At here, to cover the US, we made 8 coordinates, which are located at FL, NJ, CA, WA, 
#MN, TX, AK and HI. And also, we take the radius as 300 kilometers.

dir.create("data/dennys/",recursive = TRUE,showWarnings = FALSE)
limit = 1000
#Because the upper limit is 1000, we take limit equal to 1000.

for(i in 1:nrow(locs))
{
  long = locs[i,1]
  lat = locs[i,2]
  radius = locs[i,3]
  dest = paste0("data/dennys/",i,".xml")
  
  get_dennys_locs(dest, key, lat, long, radius, limit)
}
#Use the function to download the strings of dennys we need.


