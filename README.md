# IV_function
Function to compute Importance Values (IV) (adapted from [Willig et al. 2005](http://www.esajournals.org/doi/abs/10.1890/04-0394)).

# arguments
inputs:

matrix.M= a matrix with communities in rows and values of metrics in columns; 

scale= logical argument, if TRUE the matrix.M will be standardized to zero mean and unit variance, if FALSE no standardization is performed;

method= If scale= TRUE, method correspond to the type of standardization imposed to matrix.M before calculation of IVs. The arguments to be used are the same to be passed  to the argument method in function decostand() in vegan. Default argument is "max", where the metrics are standardized by their respective maximum value observed between communities.

stopRule= logical, if TRUE the Importance Values of dimensions will be calculated using only significant axes according to Kaiser-Guttman criterion. Default values is TRUE.  

output:

list with length three, containing observed IVs in IV.obs_stopRule element, the proportion of variation accounted by each significant PCs in Var.by.axis element and the squared correlation among diversity metrics in matrix **M** in Metrics_correlation element.
