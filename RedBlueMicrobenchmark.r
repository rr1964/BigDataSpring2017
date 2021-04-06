
library(microbenchmark)

########This is the first main function. This is where it all begins.

###If you give the number of total cars (numCars) desired AND the number of 
####  red & blue cars, the number of red and blue cars will override whatever 
####  the total number of cars as a paramter.

####The different ways of calling this function make for a rather messy string 
####  of if statements to preface the main body of the function.
####  But we do get rather good versatility. Naturally, one needs to give the 
####  function enough information to fully define the total number of cars. 
####  The function will just use the default values to calculate numCars if we 
####  otherwise input nonsense.
####  For example, if you desire more red cars than total cars, we ignore the 
####  request and just use the default rho to calculate numCars.

beginDriving <- function(r=5, c=5, rho=0.2, p=0.5, numCars, numRedCars=NULL, 
                         numBlueCars=NULL)
{
  if(missing(numCars))
  {
    numCars = round(r*c*rho)
  }
  
  if(!(missing(numRedCars)|missing(numBlueCars)))
  {
    numCars = numRedCars+numBlueCars
  }
  
  ###Given total cars and blue cars find redcars. 
  ###Note the short circuiting of the if.
  if(!(missing(numCars)|missing(numBlueCars)) && (numCars >= numBlueCars))
  {
    numRedCars = numCars-numBlueCars
  }
  
  ###Given total cars and red cars, find bluecars.
  ###Note the short circuiting of the if.
  if(!(missing(numCars)|missing(numRedCars)) && (numCars >= numRedCars))
  {
    numBlueCars = numCars-numRedCars
  }
  
  if(numCars > r*c)
  {
    warning("More cars than spaces on grid. Simulation terminating")
    return(list())
  }
  
  if(numCars==0)
  {
    warning("Total cars to be placed is ZERO (0). Simulation ending.")
    return(list())
  }
  numberedIndices = sample(1:(r*c), numCars, replace = FALSE)
  
  if(!(missing(numRedCars)|missing(numBlueCars)))
  {
    carTypes = sample(c(rep(1, numRedCars), rep(0, numBlueCars)))
  }else
  { ###Generate a vector of car types (1 = RED, 0 = BLUE)
    carTypes = rbinom(numCars,1, p)
  }
  
  vectRed = vector(mode = "integer", length = r*c)
  vectRed[numberedIndices[as.logical(carTypes)]] = 1
  
  vectBlue = vector(mode = "integer", length = r*c)
  vectBlue[numberedIndices[!as.logical(carTypes)]] = 1
  
  assign("numCars", numCars, envir = .GlobalEnv)
  assign("rows", r, envir = .GlobalEnv)
  assign("cols", c, envir = .GlobalEnv)
  
  if(!noOverlaps(vectRed, vectBlue))
  {
    warning("We had overlaps! Check the uniquenes of the assigning algorithm")
  }
  
  #####Create the reference concordances for the indices. 
  gridSlots = r*c
  
  assign("redForwardConcordance", 
         sapply(1:gridSlots, makeRedForwardIndex, c=c), 
         envir = .GlobalEnv)
  assign("blueForwardConcordance",
         sapply(1:gridSlots, makeBlueForwardIndex, c=c, gridSlots=gridSlots), 
         envir = .GlobalEnv)
  assign("redStepbackConcordance",
         sapply(1:gridSlots, makeRedBackwardIndex, c=c), 
         envir = .GlobalEnv)
  assign("blueStepbackConcordance",
         sapply(1:gridSlots, makeBlueBackwardIndex, c=c, gridSlots=gridSlots), 
         envir = .GlobalEnv)  
  
  return(list(vectRed, vectBlue))
}

####The following functions take as input a single index and output the new 
#### index of the value at the original index after performing the given move 
####    (red car forward/back, blue car forward/back).
#### Think of each spot in a matrix being permuted to a new spot, based on the 
#### rules of the car color
makeRedForwardIndex <- function(vectIndex, c)
{
  modIndex = vectIndex - 1
  return((modIndex %/% c) * c + (modIndex - 1) %% c + 1)
}

makeBlueForwardIndex <- function(vectIndex, c, gridSlots)
{##This can also be done using the head and tail functions. But those are slow. 
  modIndex = vectIndex - 1
  return((modIndex + c) %% gridSlots + 1)
}

makeRedBackwardIndex <- function(vectIndex, c)
{
  modIndex = vectIndex - 1
  return((modIndex %/% c) * c + (modIndex + 1)%%c + 1)
}

makeBlueBackwardIndex <- function(vectIndex, c, gridSlots)
{
  modIndex = vectIndex - 1
  return((modIndex - c) %% gridSlots + 1)
}

###Given a matrix for the red cars and a matrix for the blue cars
### check if there are any overlaps between them and also within groups.
### Note that this function not only checks for overlaps, it also checks for 
### loss of cars. That is, it confirms that we have the same number of cars 
### that we started with.
noOverlaps <- function(redVect, blueVect)
{
  if(sum((redVect + blueVect) %% 2) == numCars)### numCars is global. 
  {
    #print("There were no overlaps. Placement worked!")
    return(TRUE)##As in TRUE, there are NO overlaps.
  }
  #warning("We had overlaps! Check the uniquenes of the assigning algorithm")
  return(FALSE)##As is FALSE, there ARE overlaps.
  ## Here is how this works:  ##
  # The 2 vectors (1 for each color) should contain en totale 'numCars' nonzero 
  # entries.
  # So, when we add the two vectors together and then mod by 2, if there was an 
  # overlap between them, this overlap would turn into a 0 (mod 2) and we would 
  # not have the necessary number of cars when summing. Furthermore, if there 
  # was an overlap by two cars of the SAME color, we would also not get enough 
  # total cars. This is because if we overlapped at any point with the
  # red (blue) cars, there would only be a value of 1 there.So if two or more 
  # red (or blue) cars were placed at the same index, that index would still 
  # only contain a value of 1. 
  # This would result in there being too few cars counted.
}

###Moves every red car whether it overlaps or not. 
###This is then checked for valid moves. 
blindMoveRedCar <- function(oldRedMat)
{
  newRedMat = oldRedMat[redForwardConcordance]
  return(newRedMat)
}

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
checkREDMoves <- function(newRed, oldRed, oldBlue)
{
  stepVect = (newRed + oldRed + oldBlue)
  blocked = (stepVect == 2)
  stuckCars = as.numeric(blocked[redStepbackConcordance])
  diffVect = newRed - oldRed - oldBlue
  clear = (diffVect == 1)
  finalRedResult = stuckCars + as.numeric(clear)
  return(finalRedResult)
}

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
  return(finalBlueResult)
}

Run <- function(time = 10, startRedMat, startBlueMat)
{
  redStates = startRedMat
  blueStates = startBlueMat
  for(t in 1:time)
  {
    if(t %% 2 == 0)
    {
      redStates = checkREDMoves(blindMoveRedCar(redStates), 
                                redStates, blueStates)
    }
    if(t %% 2 == 1)
    {
      blueStates = checkBLUEMoves(blindMoveBlueCar(blueStates), 
                                  redStates, blueStates)
    }
  }
  list(redStates, blueStates)
}

byteCompiledBeginDrive = compiler::cmpfun(beginDriving)
byteCompiledRun = compiler::cmpfun(Run)

letsDriveFAST <- function(r, c, rho, p, time)
{
  bothCars = byteCompiledBeginDrive(r=r, c=c, rho=rho ,p=p)
  redCars = bothCars[[1]]
  blueCars = bothCars[[2]]
  byteCompiledRun(time = time, startRedMat = redCars, startBlueMat = blueCars)
}

byteCompiledDrive = compiler::cmpfun(letsDriveFAST)
byteCompiledDrive(r=5, c=4, rho=0.5, p=0.5, time = 10)

microbenchmark(byteCompiledDrive(r=30, c=20, rho=0.5, p=0.5, time = 1000), 
               times = 100, unit="ms")



