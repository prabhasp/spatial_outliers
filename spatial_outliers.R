####### LIBRARIES ########
library('shapefiles')
library('stringr')
library('ggplot2')
library('plyr')
library('sp')
library('maptools')
gpclibPermit()

##### silly libraries... no titlecase...? ####
CapLeading <- function (string){
   fn <- function(x){
     v <- unlist(strsplit(x, split = " "))
     u <- sapply(v, function(x){
            x <- tolower(x)
            substring(x, 1, 1) <- toupper(substring(x, 1, 1))
            x})
     paste(u, collapse = " ")
     }
   sapply(string, fn)
   }


###### READ IN ALL DATA; COMBINE SURVEYS #############
print("Loading variables..")
if("edu" %in% ls()) {	print("skipping edu load process") } else {
	edu <- read.csv("~/Dropbox/Nigeria/NMIS - Nigeria/NMIS Data/final_cleaned_data/csv/facility_csvs/Educ_Baseline_PhaseII_all_merged_cleaned_2011Nov21.csv") }
if("health" %in% ls()) {	print("skipping health load process") } else {
	health <- read.csv("~/Dropbox/Nigeria/NMIS - Nigeria/NMIS Data/final_cleaned_data/csv/facility_csvs/Health_PhII_RoundI&II&III_Clean_2011.10.21.csv") }
if("water" %in% ls()) {	print("skipping water load process") } else {
	water <- read.csv("~/Dropbox/Nigeria/NMIS - Nigeria/NMIS Data/final_cleaned_data/csv/facility_csvs/Water_Baseline_PhaseII_all_merged_cleaned_09_19_2011.csv") }

process <- function(sector, df) {
	gpscol <- if(sector=="education") df$gps else df$geocodeoffacility
	foo <- data.frame(do.call('rbind', strsplit(as.character(gpscol),' ',fixed=TRUE)))
	foo <- summarize(foo, y=as.numeric(as.character(X1)), x=as.numeric(as.character(X2)), prec=as.numeric(as.character(X4))) # note, y-axis comes first
	if (sector=="education") {
		gpses <-  cbind(foo, subset(df, select=c("lga", "zone", "state", "geo_id", "school_name")))
	} else if(sector=="health") {
		gpses <-  cbind(foo, subset(df, select=c("lga", "zone", "state", "geoid", "facility_name")))
	} else if(sector=="water") {
		gpses <-  cbind(foo, subset(df, select=c("lga", "zone", "state", "geoid", "water_source_type")))
	}	
	names(gpses) <- c("lat", "long", "gps_precision", "lga", "zone", "state", "id", "name")
	gpses$lga <- factor(CapLeading(str_replace_all(as.character(gpses$lga), "_", " ")))
	gpses
}
if (file.exists("gpses.csv")) {
	gpses <- read.csv("gpses.csv") 
} else { 
	gpses <- rbind(process("education", edu), process("health", health), process("water", water)) 
}

######### Reading the shapefile #############
## The following is taken straight from the reading shape files vignette -- cran.r-project.org/web/packages/spatstat/vignettes/shapefiles.pdf
if (("ggplot_lga_polygons" %in% ls()) &&  ("windows" %in% ls())) {
	print("shapefiles found... skipping loading.")
} else {
	print("Loading Shapefiles..")
	xx <- readShapeSpatial("/Users/prabhaspokharel/Dropbox/Nigeria/NMIS - Nigeria/LGA Shape Files/LGA.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
	levels(xx@data$Name) <- str_replace_all(levels(xx@data$Name), "-", " ")
	regions <- slot(xx, "polygons")
	names(regions) <- xx@data$Name
	regions <- lapply(regions, function(x) SpatialPolygons(list(x)))
	windows <- lapply(regions, as.owin)
	
	## For mapping in ggplot, we need to fortify things (and in fact, generate the ggplot polygon map object)
	regions4ggplot <- fortify.SpatialPolygonsDataFrame(model=xx, region='Name')
	ggplot_lga_polygons <- dlply(regions4ggplot, .(id), function(dfdf) {
			geom_polygon(aes(x=long, y=lat, group=piece), fill=alpha("grey20", 0.2), data=dfdf)
		})
}

######### Dealing with naming issues #########
# The issue is that LGA names in shapefile and data are different. This section
# removes _ and - to make them spaces, matches based on that, and throws away all
# data that can't be matched this way.
print("Dealing with naming issues..")
lga_names_in_data <- ldply(levels(gpses$lga))
lga_names_in_shp <- names(windows)
lga_names_in_data$MATCH <- lga_names_in_data$V1 %in% lga_names_in_shp

print(paste("Throwing away", nrow(subset(lga_names_in_data, subset=!MATCH)), "LGAs with name mismatch between data and Shapefile"))
lga_names_in_data <- subset(lga_names_in_data, subset=MATCH)

gpses_matched <- na.omit(subset(gpses, subset=(gpses$lga %in% lga_names_in_data$V1)))

######## Figuring out whether things are inside polygons ##########
print("Looking at inside/outside datapoints")
inlga <- function(df) {
   thisLGA <- as.character(df[1,"lga"])
   w <- windows[[thisLGA]]
   df$inside <- inside.owin(df$long, df$lat, w)
   df$dists <- distfun(w)(df$long, df$lat) # TODO: this is euclidean-distance in lat/long; revise to something that makes sense in real world.
   df
}
res <- ddply(gpses_matched, .(lga), inlga)

###### PRINT into pdf ############
print("Writing data")
write.csv(res, "~/Code/R/spatial_outliers/spatial_outlier_info.csv")
draw <- function(fname="lga_inside_outside_with_prec.pdf", testlga=NULL) {
	if(!is.null(testlga)) {
		myres = subset(res, lga==testlga)
	} else {
		myres = res
	}
	print(paste("Writing pictures to", fname))
	pdf(fname)
	d_ply(myres, .(lga), .print=TRUE, function(df) {
		thisLGA <- as.character(df[1,"lga"])
		ggplot() + layer(data=df, mapping=aes(x=long, y=lat, colour=inside, size=gps_precision), geom='point') + 	
		scale_color_manual(name="Inside", values=c("TRUE"="darkgreen", "FALSE"="darkred")) + opts(title=thisLGA) + ggplot_lga_polygons[[thisLGA]] +
		scale_size(name="GPS Precision (meters)", trans="log2", limits=c(2,256)) 
	})
}

######### For when we need real spatial objects; not used now ##########
#read_spatial_objects <- function () {
#	xx <- readShapeSpatial("/Users/prabhaspokharel/Dropbox/Nigeria/NMIS - Nigeria/LGA Shape Files/LGA.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
#	xx@data <- xx@data[,c("Name", "POP_90")]	
#}

