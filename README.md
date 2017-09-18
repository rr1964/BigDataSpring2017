# Modelling work in Adele Cutler's classes.

Stat 6910 Big Data with Adele Cutler. Modelling house price based on a variety of metrics. Done for the Kaggle contest https://www.kaggle.com/c/house-prices-advanced-regression-techniques

Also included here is code for a traffic simulation (Red Blue Cars). I had the fastest algorithm in the class. This means that my simulation was fastest at finding the final position of the cars after a given number of steps in the simulation. 
The premise of the question being modelled is "How long does it take for a traffic jam when we have cars going north (blue) and cars going east (red)?"
My model allows us to examine density of traffic (rho), distribution of traffic (red versus blue, given by p = P[car is red]), and the size of the simulation grid. 
When one wants to model several thousand cars on the grid, especially over several thousand iterations, having a fast running simulation can be beneficial. If your simulation is too slow, these large scale simulations will lag and finding the final position of the cars (jammed or not) will be difficult. 






