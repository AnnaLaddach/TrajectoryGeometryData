
library(TrajectoryGeometry)
library(pracma)
library(devtools)

## ###################################################
## ###################################################
## Here be functions concerned with producing data 
## for documentation.  These functions are not part
## of the TrajectoryGeometry package per se.

## ###################################################
## Create data for two paths one of which branches off
## from the other.
##
## This function creates data for two 3-dimensional
## paths.  One branches off from the other.
dataForTwoPaths = function(saveThis=FALSE)
{
    ## ###################################################
    ## Make a path which generally proceeds in the direction
    ## (1,0,0) but also includes noise.
    stepLengths = c(3,2,1,1,3,1,2,1,3,3,2,1,1)
    v = c(1,0,0)
    v = v / Norm(v)
    N = length(stepLengths)
    noise = 3
    half = rep(.5,3)
    path = matrix(0,nrow=N+1,ncol=3)

    for(i in 1:N)
        path[i+1,] = path[i,] + stepLengths[i] * v + noise * (runif(3) - half)

    ## ###################################################
    ## Make a path which starts out in the general direction (1,0,0) 
    ## then changes direction and heads in the general direction (0,1,0).
    ## This path also includes noise.

    ## The new direction:
    v2 = c(0,1,0)
    ## Permute the step lengths:
    stepLengths = stepLengths[sample(N,N)]
    path2 = matrix(0,nrow=N+1,ncol=3)
    ## We'll change direction in mid flight:
    changeAt = 5
    for(i in 1:N)
    {
        if( i < changeAt)
        {
            path2[i+1,] = path2[i,] + stepLengths[i] * v + noise * (runif(3) - half)
        } else {
            path2[i+1,] = path2[i,] + stepLengths[i] * v2 + noise * (runif(3) - half)
        }
    }


    ## ###################################################
    ## ###################################################
    ## We now produce the spherical data for these two paths.
    
    sphericalData = pathToSphericalData(path,from=1,to=nrow(path),d=ncol(path),statistic='mean')
    projection = sphericalData$projections
    center = sphericalData$center
    radius = sphericalData$distance

    sphericalData2 = pathToSphericalData(path2,from=changeAt+1,to=nrow(path),d=ncol(path),statistic='mean')
    projection2 = sphericalData2$projections
    center2 = sphericalData2$center
    radius2 = sphericalData2$distance

    ## ###################################################
    ## Osciallation example.  This is a path which oscilates
    ## around the origin before proceeding in a specific
    ## direction.
    epsilon = 1e-2
    k = 6
    oscillation = matrix(0,nrow=k,ncol=3)
    for(i in 1:k)
        oscillation[i,] = epsilon * generateRandomUnitVector(3)

    oscillation = rbind(oscillation,
                        straightPath)


    ## ###################################################
    ## Save the data:
    straightPath = path
    crookedPath = path2
    straightPathProjection = projection
    crookedPathProjection = projection2
    straightPathCenter = center
    crookedPathCenter = center2
    straightPathRadius = radius
    crookedPathRadius = radius2

    if(saveThis)
    {
        use_data(straightPath,
                 crookedPath,
                 straightPathProjection,
                 crookedPathProjection,
                 straightPathCenter,
                 crookedPathCenter,
                 straightPathRadius,
                 crookedPathRadius,
                 oscillation,
                 overwrite=TRUE)
    } else {
        data = list()
        data[['straightPath']] = straightPath
        data[['crookedPath']] = crookedPath
        data[['straightPathProjection']] = straightPathProjection
        data[['crookedPathProjection']] = crookedPathProjection
        data[['straightPathCenter']] = straightPathCenter
        data[['crookedPathCenter']] = crookedPathCenter
        data[['straightPathRadius']] = straightPathRadius
        data[['crookedPathRadius']] = crookedPathRadius
        data[['oscillation']] = oscillation

        return(data)
    }
}


