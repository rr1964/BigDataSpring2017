#install.packages("microbenchmark")

require(RColorBrewer)
##library(slam)###Sparse matrices. For efficiency and fast access
library(RColorBrewer)

library(roxygen2)

########This is the first main function. This is where it all begins.

###If you give the number of total cars (numCars) desired AND the number of red & blue cars,
####  the number of red and blue cars will override whatever the total number of cars
####  given as a paramter.

####The different ways of calling this function make for a rather messy string of if
####  statements to preface the main body of the function.
####  But we do get rather good versatility. Naturally, one needs to give the function
####  enough information to fully define the total number of cars. The function
####  will just use the default values to calculate numCars if we otherwise input nonsense.
####  For example, if you desire more red cars than total cars, we ignore the request and
####  just use the default rho to calculate numCars.


#' Make a \code{car.grid} object
#'
#' This implements a convenient way to represent the locations of blue and red cars on a grid.
#' The returned value is a \code{car.grid} object.
#' @param r is number of rows desired.
#' @param c is number of columns desired.
#' @param rho is the rho desired.
#' @param p is the probability of a car being red
#' @param numCars can be defined in lieu of \code{rho}
#' @param numRedCars can be used to specifically define the desired number of red cars.
#' @param numBlueCars can be used to specifically define the desired number of blue cars.
#' @return Returns a populated \code{car.grid} object.
#'
beginDriving <- function(r=5, c=5, rho=0.2, p=0.5, numCars, numRedCars=NULL, numBlueCars=NULL)
{
  if(missing(numCars))
  {numCars = round(r*c*rho)}

  if(!(missing(numRedCars)|missing(numBlueCars)))
  {
    numCars = numRedCars+numBlueCars

  }

  ###Given total cars and blue cars find redcars. Note the short circuiting of the if.
  if(!(missing(numCars)|missing(numBlueCars)) && (numCars >= numBlueCars))
  {
    numRedCars = numCars-numBlueCars
  }

  ###Given total cars and red cars, find bluecars. Note the short circuiting of the if.
  if(!(missing(numCars)|missing(numRedCars)) && (numCars >= numRedCars))
  {
    numBlueCars = numCars-numRedCars
  }

  if(numCars > r*c)
  {warning("More cars than spaces on grid. Simulation terminating"); return(list())}

  if(numCars==0)
  {warning("Total cars to be placed is ZERO (0). Simulation ending.");return(list())}else
  {
    numberedIndices = sample(1:((r*c)), numCars, replace = FALSE)
    ##

    ##print(numberedIndices)

    if(!(missing(numRedCars)|missing(numBlueCars)))
    {
      carTypes = sample(c(rep(1,numRedCars),rep(0,numBlueCars)))

    }else
    {carTypes = rbinom(numCars,1, p)}###Generate a vector of car types (1=RED, 0 = BLUE)


    vectRed = vector(mode = "integer", length = r*c)
    vectRed[numberedIndices[as.logical(carTypes)]] = 1

    vectBlue = vector(mode = "integer", length = r*c)
    vectBlue[numberedIndices[!as.logical(carTypes)]] = 1

    assign("numCars", numCars, envir = .GlobalEnv)
    assign("rows", r, envir = .GlobalEnv)
    assign("cols", c, envir = .GlobalEnv)

    if(!noOverlaps(vectRed, vectBlue))
    {warning("We had overlaps! Check the uniquenes of the assigning algorithm")}

    #####Create the reference concordances for the indices.
    gridSlots = r*c

    assign("redForwardConcordance", sapply(1:gridSlots,makeRedForwardIndex,c=c), envir = .GlobalEnv)
    assign("blueForwardConcordance",
           sapply(1:gridSlots,makeBlueForwardIndex,c=c,gridSlots = gridSlots), envir = .GlobalEnv)

    assign("redStepbackConcordance",sapply(1:gridSlots,makeRedBackwardIndex,c=c), envir = .GlobalEnv)
    assign("blueStepbackConcordance",
           sapply(1:gridSlots,makeBlueBackwardIndex,c=c,gridSlots = gridSlots), envir = .GlobalEnv)

    fullGrid = list(red = vectRed, blue = vectBlue, nrow = r, ncol = c)

    class(fullGrid) = "car.grid"

    return(fullGrid)
  }
}


#'
####The following functions take as input a single index and output the new index of the value
#### at the original index after performing the given move
####    (red car forward/back, blue car forward/back).
#### Think of each spot in a matrix being permuted to a new spot, based on the rules of the car color
makeRedForwardIndex <- function(vectIndex, c)
{
  modIndex = vectIndex-1
  return((modIndex%/% c)*c + (modIndex-1)%%c + 1)
}

#'
makeBlueForwardIndex <- function(vectIndex, c, gridSlots)
{##This can also be done using the head and tail functions. But those are slow.
  modIndex = vectIndex-1
  return((modIndex+c)%%gridSlots + 1)
}

#'
makeRedBackwardIndex <- function(vectIndex, c)
{
  modIndex = vectIndex-1
  return((modIndex%/% c)*c + (modIndex+1)%%c + 1)
}

#'
makeBlueBackwardIndex <- function(vectIndex, c, gridSlots)
{
  modIndex = vectIndex-1
  return((modIndex-c)%%gridSlots + 1)
}

###Given a matrix for the red cars and a matrix for the blue cars
### check if there are any overlaps between them and also within groups.
### Note that this function not only checks for overlaps, it also checks for loss of cars.
### That is, it confirms that we have the same number of cars that we started with.

#'
noOverlaps <- function(redVect, blueVect)
{
  if(sum((redVect + blueVect)%%2) == numCars)###Remember that numCars is global.
  {
    #print("There were no overlaps. Placement worked!")
    return(TRUE)##As in TRUE, there are NO overlaps.

  } else
  {
    #warning("We had overlaps! Check the uniquenes of the assigning algorithm")
    return(FALSE)##As is FALSE, there ARE overlaps.
  }
  ## Here is how this works:  ##
  # The 2 vectors (1 for each color) should contain en totale 'numCars' nonzero entries.
  # So, when we add the two vectors together and then mod by 2, if there was an overlap
  # between them, this overlap would turn into a 0 (mod 2) and we would not have the
  # necessary number of cars when summing. Furthermore, if there was an overlap by two cars
  # of the SAME color, we would also not get enough total cars. This is because
  # if we overlapped at any point with the red (blue) cars, there would only be a value of 1 there.
  # So if two or more red (or blue) cars were placed at the same index, that index would
  # still only contain a value of 1. This would result in there being too few cars counted.


}


#'
###Moves every red car whether it overlaps or not. This is then checked for valid moves.
blindMoveRedCar <- function(oldRedMat)
{
  newRedMat = oldRedMat[redForwardConcordance]

  return(newRedMat)
}

#'
#Moves every blue car whether it overlaps or not.
blindMoveBlueCar <- function(oldBlueMat)
{


  newBlueMat = oldBlueMat[blueForwardConcordance]

  return(newBlueMat)
}

####Confirm that the moves of the red cars were valid (not blocked).
#### If a car cannot move, move it back to where it was.
#### This function returns the final result (taking into account blocked cars)
#### of one step of red car moves.

#'
checkREDMoves <- function(newRed, oldRed, oldBlue)
{

  stepVect = (newRed + oldRed + oldBlue)

  blocked = (stepVect == 2)

  stuckCars = as.numeric(blocked[redStepbackConcordance])


  diffVect = newRed - oldRed - oldBlue
  clear = (diffVect == 1)


  # as.matrix(


  finalRedResult = stuckCars + as.numeric(clear)

  if(!noOverlaps(finalRedResult, oldBlue))
  {warning("There is an issue with overlaps after moving the red cars.")}


  return(finalRedResult)

}

#'
####Confirm that the moves of the blue cars were valid (not blocked).
#### If a car cannot move, move it back to where it was.
#### This function returns the final result (taking into account blocked cars)
#### of one step of blue car moves.
checkBLUEMoves <- function(newBlue, oldRed, oldBlue)
{

  stepVect = newBlue + oldRed + oldBlue
  blocked = (stepVect == 2)
  stuckCars = blocked[blueStepbackConcordance]


  diffVect = newBlue - oldRed - oldBlue
  clear = (diffVect == 1)


  finalBlueResult = stuckCars + as.numeric(clear)

  if(!noOverlaps(oldRed, finalBlueResult))
  {warning("There is an issue with overlaps after moving the blue cars.")}

  return(finalBlueResult)
}

######plot.cars() gives a visual representation of a single state of moves.
###### Give the function the sparse matrix describing where the red cars are.
###### Give the function the sparse matrix describing where the blue cars are.
##### Also tell the function the state (t) to output (for printing purposes only).

par(mfrow = c(1,1))###Change this for the desired plotting layout

#'
plot.cars <- function(redCars, blueCars, state= NULL)
{
  library(RColorBrewer)
  colorsPal = colorRampPalette(brewer.pal(9,"RdBu"))(255)
  #Colors can be tweeked as desired.

  entries = -redCars +blueCars

  m = matrix(entries, nrow=rows, ncol = cols, byrow  = TRUE)
  if(missing(state))
  {
    image(t(m[nrow(m):1,] ), axes=FALSE, zlim=c(-1,1),
          col = c(colorsPal[20], "gray90", colorsPal[230]))
  }else
  {
    image(t(m[nrow(m):1,] ), axes=FALSE, zlim=c(-1,1),
          col = c(colorsPal[20], "gray90", colorsPal[230]),
          main = c("Time", state))
  }



}

############
### Adding to a pre-existing list is nortoriously slow from what I have seen.
### It is a bit quicker to add to an environment, so that is what I do.
### SEE: http://stackoverflow.com/questions/17046336/
{###This code is run to initialize the adding to an environment of time states.

  #Counter <- -1
  #listOfStates <- new.env()
  #AddItemEnvir <- function(item)
  #{
  #  .GlobalEnv$Counter <- .GlobalEnv$Counter + 1
  #
  #   .GlobalEnv$listOfStates[[as.character(.GlobalEnv$Counter)]] <- item
  #}
}####End of code to initialize adding for states of the process.


#####This is where the actual simulation occurs.
### time tells R how many states to simulate (what we were calling "t")
### speed is a parameter for the speed of animation.
###        speed=1 should place about ~1 second between frames.
### animate indicates whether or not to display the animations of the states.

#'
#'Runs the simulation for the defined grid. The simulation as well as the initialization of the \code{car.grid} can be done through this function.
#' @param time indicates how many iterations to perform in the simulation.
#' @param speed is a parameter for the speed of animation.\code{speed=1} should place about ~1 second between frames.
#' @param animate indicates whether or not to display the animations of the states.
#'
#' The remaining variables define the grid, as documented in \code{beginDriving()}.
#'
#' @return Returns a list of the states of the iteration. Index j corresponds to time t = j-1.
#'
#' @examples listOfStates = runTheSimulation(r= 5, c = 8, rho = 0.3, p = 0.4, speed = 0.2)
runTheSimulation <- function(time=10, speed=1.5, animate = TRUE, r, c, rho, p)
{
  #.GlobalEnv$Counter <- -1 ###This makes it so the initial start state is also stored.
  #.GlobalEnv$listOfStates <- new.env()


  bothCars = beginDriving(r=r,c=c, rho=rho ,p=p)
  #print(class(bothCars))
  startRedMat = bothCars[[1]]
  startBlueMat = bothCars[[2]]


  listOfStates = vector(mode = "list", length = time+1)

  origGrid = list(red = startRedMat, blue = startBlueMat, nrow = r, ncol = c)
  class(origGrid) = "car.grid"

  #AddItemEnvir(origGrid)
  listOfStates[[1]] = origGrid

  redStates = startRedMat
  blueStates = startBlueMat

  if(animate)
  {
    plot.cars(startRedMat,startBlueMat,0)
    Sys.sleep(speed)
  }

  for(t in 1:time)
  {
    if(t %%2 == 0)
    {
      redStates = checkREDMoves(blindMoveRedCar(redStates), redStates, blueStates)
      #print(c("Red moves for time ", t))
      #print(as.matrix(redStates-blueStates))
      #print("-------------------------------------")
      #print("-------------------------------------")
      newGrid = list(red = redStates, blue = blueStates, nrow =r , ncol = c)

      class(newGrid) = "car.grid"


      listOfStates[[t+1]] = newGrid
      #AddItemEnvir(newGrid)

      if(animate)
      {plot.cars(redStates, blueStates, t);Sys.sleep(speed)}


    }
    if(t %%2 == 1)
    {
      blueStates = checkBLUEMoves(blindMoveBlueCar(blueStates), redStates, blueStates)

      newGrid = list(red = redStates, blue = blueStates, nrow =r , ncol = c)

      class(newGrid) = "car.grid"

      listOfStates[[t+1]] = newGrid

      #AddItemEnvir(newGrid)

      if(animate)
      {plot.cars(redStates, blueStates, t);Sys.sleep(speed)}

    }
  }
  return(listOfStates)
}

pig = runTheSimulation(time = 1000, r= 20, c = 20, rho = 0.6, p = 0.15, speed = 0.1)

#'
#' Plot a \code{car.grid} object. This allows for full color plots of \code{car.grid} objects.
#' @param carsGrid is a \code{car.grid} object that is to be plotted to the plotting window
#'
plot.car.grid <- function(carsGrid)
{
  if(class(carsGrid) != "car.grid")
  {warning("Object input to plot.car.grid() is not of type 'car.grid'")}

  plot.cars(carsGrid$red, carsGrid$blue)
}


#plot(listOfStates[["3"]])


#'
#'Print a \code{car.grid} object. Given as a +/- 1 matrix.
#'@param carsGrid is a \code{car.grid} object that is to be printed to the console in its current state.
print.car.grid <- function(carsGrid)
{
  if(class(carsGrid) != "car.grid")
  {warning("Object input to print.car.grid() is not of type 'car.grid'")}

  print("Current State of Grid")
  print("Blue is +1, Red is -1")

  gridCurrent = matrix(data = carsGrid$blue - carsGrid$red,
                       nrow = carsGrid$nrow, ncol = carsGrid$ncol, byrow = TRUE)
  print(gridCurrent)
}

