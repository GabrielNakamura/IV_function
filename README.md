# IV_function - under construction, new implementation from original version
Function to compute Importance Values (adapted from [Willig et al. 2005](http://www.esajournals.org/doi/abs/10.1890/04-0394)), with bootstrap procedure 

# arguments
inputs:

matrix.M= a matrix with communities in rows and values of metrics in columns; 

scale= logical argument, if TRUE the matrix.M will be standardized if FALSE no standardization is performed; default TRUE;

method= If scale= TRUE, method correspond to the type of standardization applied in matrix.M before calculation of IVs. The arguments to be used are the same to be passed  to the argument method in function decostand() in vegan. Default argument is "standardize", where the metrics are set to zero mean and unit variance. Method "max" is also recommended;

stopRule= logical, if TRUE the Importance Values of dimensions will be calculated using only significant axes according to Kaiser-Guttman criterion. Default values is TRUE.  

output:

List with length three containing:
IVs.resul= matrix, IV result for metrics used in matrix M. If stoprule=FALSE this will be a matrix containing only the significant PCs in the rows;
prop.var= proportion of contribution for each metric in each axis;
metric.sqrt.corr= correlation among metrics.
