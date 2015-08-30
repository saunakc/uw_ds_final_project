# KMeansHelper.R

# Function plots labeled observations and centers
ClusterPlot <- function(observations=sampleObservations, centers=centersGuess, labels=labelsRandom)
{
  # Create the frame of a plot without contents
  x_Min <- min(observations[,1], centers[,1], na.rm=T)
  x_Max <- max(observations[,1], centers[,1], na.rm=T)
  y_Min <- min(observations[,2], centers[,2], na.rm=T)
  y_Max <- max(observations[,2], centers[,2], na.rm=T)
  yLocation <- xLocation <- c()
  plot(c(), xlab='X', ylab='Y', ylim=c(y_Min, y_Max), xlim=c(x_Min, x_Max))
  
  # Add observations
  for (label in 1:nrow(centers))
  {    
    affiliatedObservations <- matrix(data=observations[labels == label,], ncol=ncol(observations))
    if(nrow(affiliatedObservations) > 0)
    {
      points(affiliatedObservations, pch=21, col=label+1, cex=4, lwd=2)
      text(x=affiliatedObservations[,1], y=affiliatedObservations[,2], label, cex=1, col=label+1)
    }
  } # for
  
  # Add centers
  iter<-1:nrow(centers)
  points(centers, pch=24, cex=5, col=iter+1, bg='lightgrey')
  text(x=centers[iter,1], y=centers[iter,2], iter, cex=2, col=iter+1)
  
  Sys.sleep(1)
} # ClusterPlot

# Use sampleObservations for testing
sampleObservations <- matrix(nrow=83, ncol=2, byrow= T, data=-c(
  1.91,1.43,
  0.9,0.79,
  1.26,0.52,
  0.61,1.55,
  1.25,0.66,
  1.04,0.62,
  0.53,1.33,
  0.99,1.27,
  1.11,1.04,
  0.1,2.41,
  -0.15,1.83,
  0.83,1.02,
  0.72,1.17,
  0.69,0.97,
  0.74,0.91,
  0.72,0.14,
  1.09,0.53,
  0.68,1.15,
  0.67,0.96,
  0.82,0.87,
  0.74,0.27,
  0.94,-0.15,
  0.64,0.82,
  1.44,0.72,
  0.76,0.84,
  1.06,1.52,
  0.79,0.93,
  0.88,0.91,
  0.76,0.87,
  0.85,0.93,
  0.88,0.97,
  0.75,1,
  0.83,0.86,
  0.85,0.88,
  0.35,0.55,
  0.63,-1.99,
  -0.14,-0.78,
  -0.04,-0.32,
  0.3,0.67,
  -0.52,-1.75,
  -0.27,-0.7,
  -0.32,-0.51,
  -0.08,-0.37,
  -0.39,-0.55,
  -0.06,-0.42,
  0.09,-0.48,
  -0.51,0.64,
  -0.22,-0.49,
  -0.03,-0.51,
  -0.12,-0.32,
  0.01,-0.48,
  -0.21,-0.57,
  -0.21,-0.32,
  0.37,-0.28,
  1.18,-1.51,
  0,-0.41,
  0,-0.44,
  -0.66,-2.27,
  -0.1,-0.67,
  1.01,-0.32,
  1.19,0.43,
  -0.3,-1.26,
  -2.2,-1.85,
  -1.82,-0.16,
  -1.33,-0.89,
  -0.84,0.05,
  -2.17,-0.38,
  -1.67,-0.53,
  -1.38,-1.75,
  -1.39,-0.98,
  -1.32,-0.33,
  -1.49,-1.41,
  -2.16,-1.33,
  -1.64,-0.9,
  -1.44,-0.72,
  -1.58,-0.77,
  -1.53,-0.66,
  -1.53,-0.81,
  -0.27,-0.87,
  -1.32,-0.94,
  -0.89,-1.73,
  -0.33,0.55,
  -1.29,-0.7))

centersGuess <- matrix(-c(1, -2, 0, -2, 1, 0), nrow=3)

labelsRandom <- rep_len(1:nrow(centersGuess), nrow(sampleObservations))
