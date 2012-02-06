####### LIBRARIES ########
library('shapefiles')
library('stringr')
library('ggplot2')
library('plyr')
library('sp')

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


###### SOME NEEDED VARIABLES #############
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
	names(gpses) <- c("long", "lat", "gps_precision", "lga", "zone", "state", "id", "name")
	gpses$lga <- factor(CapLeading(str_replace_all(as.character(gpses$lga), "_", " ")))
	gpses
}
gpses <- rbind(process("education", edu), process("health", health), process("water", water))

####### FIRST PASS -- JUST GRAPH THE DATA in X,Y -- NOT IN USE ANYMORE ############
just_xy_graphs <- function() {
	# the plotting function
	plot <- function(df) {
	     df$lga = factor(as.character(df$lga))
	     df$state = factor(as.character(df$state))
	     with(df, coplot(-y~-x | lga))
	     #qplot(x, y, data=df) + facet_grid(.~state)
	}
	# print to file called lga_x_y_by_state; the .(state) makes the prints state-by-state
	pdf("lga_y_x_by_state.pdf")
	d_ply(gpses, .(state), plot, .print=TRUE)
	dev.off()
}

######### Reading the shapefile #############
#lga_shapes <- read.shapefile("/Users/prabhaspokharel/Dropbox/Nigeria/NMIS - Nigeria/LGA Shape Files/LGA")
#lga_data <- lga_shapes$dbf$dbf
#lga_shapes <- convert.to.simple(lga_shapes$shp)
#LGApolys <- transform(lga_shapes, Name=lga_data[Id, "Name"])
#polylist <-  dlply(LGApolys, .(Name), function (df) cbind(df$X, df$Y))
#names(polylist) <- str_replace_all(names(polylist), "-", " ")
xx <- readShapeSpatial("/Users/prabhaspokharel/Dropbox/Nigeria/NMIS - Nigeria/LGA Shape Files/LGA.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
levels(xx@data$Name) <- str_replace_all(levels(xx@data$Name), "-", " ")
regions <- slot(xx, "polygons")
names(regions) <- xx@data$Name
regions <- lapply(regions, function(x) SpatialPolygons(list(x)))
windows <- lapply(regions, as.owin)
regions4ggplot <- fortify.SpatialPolygonsDataFrame(model=xx, region='Name')
maps <- dlply(regions4ggplot, .(id), function(dfdf) {
		geom_polygon(aes(x=long, y=lat, group=piece), fill=alpha("grey20", 0.2), data=dfdf)
	})


######### Dealing with naming issues #########
print("Dealing with naming issues..")
# The issue is that LGA names in shapefile and data are different. This section
# removes _ and - to make them spaces, matches based on that, and throws away all
# data that can't be matched this way.
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
   print(thisLGA)
   w <- windows[[thisLGA]]
   df$inside <- inside.owin(df$lat, df$long, w)
   df$dists <- distfun(w)(df$lat, df$long)
#   polygon <- as.data.frame(polylist[thisLGA])
#   if (length(polygon)==2) names(polygon) <- c("x", "y")
#   df$inside <- factor(point.in.polygon(as.vector(df$lat), as.vector(df$long), as.vector(polygon$x), as.vector(polygon$y)))
	df
}
res <- ddply(gpses_matched, .(lga), inlga)

###### PRINT into pdf ############
print("Writing data")
write.csv(res, "~/Code/R/spatial_outliers/spatial_outlier_info.csv")
draw <- function(fname="lga_inside_outside_with_prec.pdf") {
	print(paste("Writing pictures to", fname))
	pdf(fname)
	d_ply(res, .(lga), .print=TRUE, function(df) {
		thisLGA <- as.character(df[1,"lga"])
		ggplot() + layer(data=df, mapping=aes(x=x, y=y, colour=inside, size=ceiling(log(gps_precision))), geom='point') + scale_size(limits = c(1, 6)) + opts(title=thisLGA) + maps[[thisLGA]]
	})
	dev.off()
}


######### For when we need real spatial objects; not used now ##########
#read_spatial_objects <- function () {
#	xx <- readShapeSpatial("/Users/prabhaspokharel/Dropbox/Nigeria/NMIS - Nigeria/LGA Shape Files/LGA.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
#	xx@data <- xx@data[,c("Name", "POP_90")]	
#}


