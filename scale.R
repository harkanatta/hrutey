Packages <- c("magrittr", "magick", "here", "exifr", "sf", "tidyverse", "glue", "httr", "stringr", "leaflet", "leaflet.extras", "leafem", "leafpop", "htmlwidgets")
pacman::p_load(Packages, character.only = TRUE)

#Lesa inn myndir, búa til möppu "minnimyndir" og setja þær inn í hana
herepath <- here()
pathnew <- paste(herepath,"minnimyndir",sep = "/")
#dir.create(pathnew)
defines <- c("png:compression-filter" = "1", "png:compression-level" = "0")

myndir <- list.files(herepath,pattern = "JPEG|JPG", recursive = T,full.names = T)
myndirB <- myndir[!gsub('.*/ ?(\\w+)', '\\1', myndir) %in% list.files(pathnew)] #ef þarf að bæta við eftir á 


for (i in myndirB) {
  mynd <- image_read(i) %>%
    image_resize("800x800")
  image_write(mynd,path = paste(pathnew, paste(gsub(".*[/]([^.]+)[.].*", "\\1", i), "JPEG", sep = "."), sep = "/"))
  image_destroy(mynd)
  print(paste(gsub(".*[/]([^.]+)[.].*", "\\1", i)))
  gc(reset = T)
}


### Git: commit og push

###Ná í slóðirnar að myndunum eftir að þær eru komnar í möppuna minnimyndir
###muna heiti á repository
Heitirepo <- strsplit(dir()[grepl("Rproj", dir())],"[.]")[[1]][1]
urlid <- glue::glue("https://api.github.com/repos/harkanatta/",Heitirepo,"/git/trees/main?recursive=1")
req <- GET(urlid)
stop_for_status(req)
url2 <- glue::glue("https://raw.githubusercontent.com/harkanatta/",Heitirepo,"/main/")

filelist <- tibble(path=unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>% 
                     stringr::str_subset("minnimyndir") %>% 
                     stringr::str_subset("JPEG|JPG|PNG")) %>%
  mutate(URL=url2,
         mURL=glue("{URL}{path}")) %>% 
  select(mURL)

for (i in filelist) {
  a=glue('<!-- .slide: data-background="{i}"data-background-size="contain" -->\n<span>\n\n<h2>\nminntexti\n</h2>\n<!-- .element: class="fragment" data-fragment-index="1" --></span>\n\n---\n\n')
}
clipr::write_clip(a) #slæðurnar komnar í clipboard

### Opna slæðuskjalið og líma inn í

### Kort

image_files <- list.files(pathnew, full.names = TRUE,recursive = T) %>% 
  read_exif(tags = "GPSPosition") %>% 
  separate(GPSPosition, into = c("lat", "lon"), sep = "\\s") %>% 
  mutate(lat=as.numeric(lat), lon=as.numeric(lon),
         myndir=filelist$mURL) %>% 
  drop_na(lat) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 'WGS84')

img <- "https://github.com/harkanatta/ssnv_trident/blob/master/graphs/tvologo.jpg?raw=true"
ganga <- rgdal::readOGR("kort/ganga.shp")
breidur <- rgdal::readOGR("kort/breidur.shp")
m <- leaflet() %>%
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addScaleBar() %>%  
  addPolygons(data = breidur,color = "#33FFE9", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5) %>% 
  addCircleMarkers(data = image_files,
                   popup = leafpop::popupImage(image_files$myndir),
                   color = "#FFFF00") %>%
  addPolylines(data = ganga, color = "#000000") %>% 
  leafem::addLogo(img, width = '20%', height = '25%',offset.y = 20,offset.x = 80,alpha = 0.7) %>% 
  leaflet.extras::addFullscreenControl(pseudoFullscreen = T)



#saveWidget(m, file="index.html")
