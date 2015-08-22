# IV_function
Function to compute Importance Values (adapted from [Willig et al. 2005](http://www.esajournals.org/doi/abs/10.1890/04-0394)), with bootstrap procedure

# arguments
inputs:

matrix.M= a matrix with communities in rows and values of metrics in columns; 

IV.bootstrap= logical argument, if TRUE the function will compute bootstrap values of IV for matrix.M, if FALSE the function will compute only observed values;

n.sample= number of bootstrap samples to be carried out with matrix.M;

scale= logical argument, if TRUE the matrix.M will be standardized to zero mean and unit variance, if FALSE no standardization is performed;

output:

list with length two containing observed and bootstrap IVs of components of diversity present in matrix.M
