
library(ggplot2)

## ####################################################
## ####################################################
set.seed(100)

## Stem points:
stemDirection = rep(1,10)
stemDirection = stemDirection / sqrt(10)
numStem = 2000
stemLength = 4
stemTimes = runif(numStem)
stemPoints = matrix(0,nrow=numStem,ncol=10)
for(i in 1:numStem)
    stemPoints[i,] = stemTimes[i] * stemLength * stemDirection

## ####################################################
## Branch points:
numBranch = 1000
## branchLength = 1.5
branchLength = 3
branchStart = stemLength * stemDirection

## One branch:
branchDirection = rep(0,10)
branchDirection[1] = 1
branchTimes = runif(numBranch)
branchPoints1 = matrix(0,nrow=numBranch,ncol=10)
for(i in 1:numBranch)
    branchPoints1[i,] = branchStart +
        branchTimes[i] * branchLength * branchDirection

## D'other branch:
branchDirection = rep(0,10)
branchDirection[2] = 1
branchTimes = runif(numBranch)
branchPoints2 = matrix(0,nrow=numBranch,ncol=10)
for(i in 1:numBranch)
    branchPoints2[i,] = branchStart +
        branchTimes[i] * branchLength * branchDirection

## Clean points:
points = rbind(stemPoints,branchPoints1,branchPoints2)

sd = .2
sd = .15
noise = rnorm(length(points),sd=sd)
noise = matrix(noise,nrow=nrow(points))
noisyPoints = points + noise
noisyPoints = abs(noisyPoints)

colnames(points) = paste0('x',1:10)
colnames(noisyPoints) = paste0('x',1:10)

g = ggplot() +
    geom_point(data=as.data.frame(points),aes(x=x1,y=x2),color='red',size=.2) +
    geom_point(data=as.data.frame(noisyPoints),aes(x=x1,y=x2),color='blue',size=.2) +
    ggtitle('Synthetic branching 02')

print(g)

ggsave(plot=g,
       filename='syntheticBranching02.jpg')
saveRDS(noisyPoints,
        'syntheticBranching02.rds')


