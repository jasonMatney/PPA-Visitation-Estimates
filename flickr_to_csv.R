# Purpose: Create a spreadsheet of geotagged photo data from Flickr.
# Results based on search dates and bounding box (minimum latitude, minimum longitude, 
#                                                 maximum latitude, maximum longitude)
# Functions adapted from Fredheim, R. (2015). Digital data collection course. 
# Retrieved June 1 2015 from http://www.r-bloggers.com/digital-data-collection-course/. 

# Install and require necessary R packages.
# install.packages("RCurl")
# install.packages("rjson")
# install.packages("RJSONIO")
rm(list=ls())
require(RCurl) 
require (rjson)
require(RJSONIO)

# Set working directory with path name in quoation marks and / instead of \
setwd("")


# Flicker API access keys 
# Key must have single quotations around it
# Example: 'A8MNR7997LLOP123'
api_key = ''
api_secret = ''
  
# Create function to return URL using flickr.photos.search 
# More information at https://www.flickr.com/services/api/flickr.photos.search.html
getURL_flickr = function(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum){
    root = 'https://api.flickr.com/services/rest/?method=flickr.photos.search&'
    u = paste0(root,"api_key=",api_key, "&min_taken_date=",  minDate,"&max_taken_date=",
               maxDate,"&bbox=", minLon,"%2C+", minLat, "%2C+", maxLon, "%2C+", maxLat,
               "&has_geo=1&extras=geo%2C+date_taken%2C+date_upload%2C+views%2C+tags%2c+url_o&per_page=250&page=",
               pageNum, "&format=json&nojsoncallback=1" )
    return(URLencode(u))
  }

#Create function to return URL to get Exif data from image metadata
getURL2 = function(api_key, id, secret){
  root2 = 'https://api.flickr.com/services/rest/?method=flickr.photos.getExif&'
  u2 = paste0(root2,"api_key=",api_key,"&photo_id=",id,"&secret=",secret,"&format=json&nojsoncallback=1" )
  return(URLencode(u2))
}

# Set location search parameters. 
# Coordinates should be in decimal degrees, surrounded by single quotes
# Example: minLon = '-155.798371' 
minLon = -78.819049
minLat = 35.715808
maxLon = -78.471063 
maxLat = 35.971728
  
# Set date search parameters.
# Use date format 'YYYY-MM-DD' with single quotations. 
minDate = '2017-10-15'
maxDate = '2017-11-15'
  
#First call for specified date and bbox
  ##This will return the URL that contains the information based on search variables above
  ##The remainder of the code will then read through the information and write it to a data frame
  ## getURL(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum=1)

#Read data returned from first call 
target = getURL_flickr(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum=1)
data = fromJSON(target)

# Get the total number of photo records returned using the current search parameters.
total = as.numeric(data$photos$total) 

# Number of pages of records returned using search parameters.
numPages = data$photos$pages 

# Create empty dataframe to populate with data.
df = NULL 
# For each page of results, from the first to maximum page number extract photo information.
for (j in 1:numPages){  
  pageNum = j 
  target.loop = getURL_flickr(api_key, minDate, maxDate, minLon, minLat, maxLon, maxLat, pageNum)
  data1 = fromJSON(target.loop)
  numPhotos = length(data1$photos$photo)
  
  # Read photo information for each photo on the current page.
  for (i in 1:numPhotos){ 
    print(paste("we are processing photo number", i, "out of", numPhotos, "on page", pageNum))
    id = data1$photos$photo[[i]]$id
    title = data1$photos$photo[[i]]$title
    lat = data1$photos$photo[[i]]$latitude
    lon = data1$photos$photo[[i]]$longitude
    owner = data1$photos$photo[[i]]$owner
    taken = data1$photos$photo[[i]]$datetaken
    dateupload = data1$photos$photo[[i]]$dateupload
    
    # Convert UNIX epoch to date-time.
    upload = as.character.Date(as.POSIXct(as.numeric(dateupload), origin="1970-01-01"))
    views = data1$photos$photo[[i]]$views
    tags = data1$photos$photo[[i]]$tags
    secret = data1$photos$photo[[i]]$secret
    server = data1$photos$photo[[i]]$server 
    farm = data1$photos$photo[[i]]$farm
    imageURL = paste("https://farm", farm, ".staticflickr.com/", server, "/", id, "_", secret, ".jpg", sep="")
    getURL2(api_key, id, secret)
    target2 = getURL2(api_key, id, secret)
    exifData = fromJSON(target2)
    
    if (exifData$stat!="fail") {
      device = exifData$photo$camera
    } else{
      # If a value is not provided, then skip.
      device='NA'
    }
    row = cbind(lon, lat, id, owner, taken, upload, views, tags, title, imageURL, device)
    rbind(df, row)-> df
    
    if (j + 1 < numPages) {
      pageNum = j + 1
    }     
  }
}

# Write data in dataframe to a csv file.
write.csv(x=df, file="flickrPhotos.csv", row.names = TRUE)
