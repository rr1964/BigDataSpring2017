# Modelling work in Adele Cutler's classes.

Stat 6910 Big Data with Adele Cutler. Modelling house price based on a variety of metrics. Done for the Kaggle contest https://www.kaggle.com/c/house-prices-advanced-regression-techniques

Also included here is code for a traffic simulation (Red Blue Cars). I had the fastest algorithm in the class. This means that my simulation was fastest at finding the final position of the cars after a given number of steps in the simulation. 
The premise of the question being modelled is "How long does it take for a traffic jam when we have cars going north (blue) and cars going east (red)?"

** It obviously would be cheating to use this code as your own to complete this project in Adele Cutler's computing class. Do not use this code as your own for her computing class.**

My model allows us to examine density of traffic (rho), distribution of traffic (red versus blue, given by p = P[car is red]), and the size of the simulation grid. 
When one wants to model several thousand cars on the grid, especially over several thousand iterations, having a fast running simulation can be beneficial. If your simulation is too slow, these large scale simulations will lag and finding the final position of the cars (jammed or not) will be difficult. 

To run the Red Blue cars simulations, initiate all functions in RedBlueCars.R up through line 390.

Then run the function runTheSimulation(time = 100, r= 30, c = 30, rho = 0.5, p = 0.5, speed = 0.2)
This yields a good "trial" simulation to allow users to see how the simulation basically works. 
The arguments of the "runTheSimulation()" function can of course be changed to fit the desires of the user.

The file RedBlueMicrobenchmark.R allows users to examine the speed of the traffic simulation using the microbenchmark package in R. If you do not already have the microbenchmark package installed, you will need to install it. 





