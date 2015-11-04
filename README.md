# IV_function
Function to compute Importance Values (adapted from [Willig et al. 2005](http://www.esajournals.org/doi/abs/10.1890/04-0394)), with bootstrap procedure

# arguments
inputs:

matrix.M= a matrix with communities in rows and values of metrics in columns; 

IV.bootstrap= logical argument, if TRUE the function will compute bootstrap values of IV for matrix.M, if FALSE the function will compute only observed values;

n.sample= number of bootstrap samples to be carried out with matrix.M;

scale= logical argument, if TRUE the matrix.M will be standardized to zero mean and unit variance, if FALSE no standardization is performed;

method= If scale= TRUE, method correspond to the type of standardization imposed to matrix.M before calculation of IVs. The arguments to be used are the same to be passed  to the argument method in function decostand() in vegan. Default argument is "max", where the metrics are standardized by their respective maximum value observed between communities.

output:

list with length two containing observed and bootstrap IVs of components of diversity present in matrix.M
