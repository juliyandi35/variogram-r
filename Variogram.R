library(gstat)
library(sp)
library(readxl)

TheData=read_excel("Variogram.xlsx",sheet = "Timur-Barat")
plot(TheData$x,TheData$y)

# remove any null data rows
TheData=na.omit(TheData)


# convert simple data frame into a spatial data frame object
coordinates(TheData)= ~ x+y


# create a bubble plot with the random values
bubble(TheData, zcol='m_rand', fill=TRUE, do.sqrt=FALSE, maxsize=3)

TheVariogram=variogram(m_rand~1, data=TheData)
plot(TheVariogram)

TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)

plot(TheVariogram, model=TheVariogramModel)

FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)
plot(TheVariogram, model=FittedModel)

# Gradient
TheVariogram=variogram(m_grad~1, data=TheData)
plot(TheVariogram)
TheVariogramModel <- vgm(psill=50000, model="Gau", nugget=0.001, range=500)
FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)
plot(TheVariogram, model=FittedModel)

# Anisotropy
# Create a "gstat" object
TheGStat <- gstat(id="Sine", formula=m_sin ~ 1, data=TheData)

TheVariogram=variogram(TheGStat, map=TRUE, cutoff=4000, width=200)

## the original data had a large north-south trend, check with a variogram map
plot(TheVariogram, threshold=10)

# Create directional variograms at 0, 45, 90, 135 degrees from north (y-axis)
TheVariogram <- variogram(TheGStat, alpha=c(0,45,90,135))

# Create a new model
TheModel=vgm(model='Lin' , anis=c(0, 0.5))

# Fit a model to the variogram
FittedModel <- fit.variogram(TheVariogram, model=TheModel)

## plot results:
plot(TheVariogram, model=FittedModel, as.table=TRUE)

# Creating Kriged Surfaces
# update the gstat object:
TheGStat <- gstat(TheGStat, id="Sine", model=FittedModel )

# create sequences that represent the center of the columns of pixels
# change "by" to change the resolution of the raster
Columns=seq(from=1, to=1000, by=100)

# And the rows of pixels:
Rows=seq(from=1, to=1000, by=100)

# Create a grid of "Pixels" using x as columns and y as rows
TheGrid <- expand.grid(x=Columns,y=Rows  )

# Convert Thegrid to a SpatialPixel class
coordinates(TheGrid) <- ~ x+y
gridded(TheGrid) <- TRUE

# Plot the grid and points
plot(TheGrid, cex=0.5)
points(TheData, pch=1, col='red', cex=0.7)
title("Interpolation Grid and Sample Points")

# perform ordinary kriging prediction:
TheSurface <- predict(TheGStat, model=FittedModel, newdata=TheGrid)

# Set the margins to 2
par(mar=c(2,2,2,2))

# Add the Kriged surface
image(TheSurface, col=terrain.colors(20))

# Add contours to the surface
contour(TheSurface, add=TRUE, drawlabels=FALSE, col='brown')

# Add the points
points(TheData, pch=4, cex=0.5)

# Set the title
title('Prediction')

ThePoints = list("sp.points", TheData, pch = 4, col = "black", cex=0.5)
spplot(TheSurface, zcol="Sine.pred", col.regions=terrain.colors(20), cuts=19, sp.layout=list(ThePoints), contour=TRUE, labels=FALSE, pretty=TRUE, col='brown', main='OK Prediction')

## plot the kriging variance as well
spplot(TheSurface, zcol='Sine.var', col.regions=heat.colors(100), cuts=99, main='OK Variance',sp.layout=list(ThePoints) )

# Modifying Variograms
boundaries=c(0,1,2,3,4,5,6,7,8,9)
