IV<- function(matrix.M, IV.bootstrap= FALSE, n.sample= 999, scale= TRUE){
    library(vegan)
    if(is.matrix(matrix.M) == FALSE){
        matrix.M<- as.matrix(matrix.M)
        if(ncol(matrix.M)<3){
            stop("\n matrix M must be at least 3 components of diversity\n")
        }
        if(nrow(matrix.M)<3){
            stop("\n Matrix M must be at least 3 communities\n")
        }
    } 
        
    matrix.M.stand<-decostand(x = matrix.M, method = "standardize", MARGIN = 2)[1:nrow(matrix.M),]
    
    if(scale == TRUE){
        metric.sqrt.corr<- (prcomp(x = matrix.M.stand, scale.= FALSE)$rotation ^ 2)
        prop.var<- summary(prcomp(x = matrix.M.stand, scale.= FALSE))$importance[2,]
        
        names.matrix.IV<- list("IV.resu", colnames(matrix.M))
        IVs.result<- matrix(nrow= 1, ncol= ncol(matrix.M), dimnames= names.matrix.IV)
        for(i in 1:nrow(metric.sqrt.corr)){
            IVs.result[,i]<- metric.sqrt.corr[i,] %*% as.matrix(prop.var)
        }
        
        if(IV.bootstrap == FALSE){
            return(IVs.result)
        }
        
        if(IV.bootstrap == TRUE) {
            matrix.M.boot<- vector("list", n.sample)
            for(i in 1:n.sample) {
                matrix.M.boot[[i]]<- matrix.M.stand[sample(1:nrow(matrix.M.stand), replace= TRUE),]
            }
            
            names.IV.result.boot<- list(c(1:n.sample), colnames(matrix.M))
            IV.result.boot<- matrix(nrow= n.sample, ncol= ncol(matrix.M), dimnames= names.IV.result.boot)
            metric.sqrt.corr.boot<- vector("list", n.sample)
            prop.var.boot<- vector("list", n.sample)
            for(i in 1:length(matrix.M.boot)){ 
                metric.sqrt.corr.boot[[i]]<- (prcomp(x = matrix.M.boot[[i]], scale.= FALSE)$rotation ^ 2)
                prop.var.boot[[i]]<- summary(prcomp(x = matrix.M.boot[[i]], scale.= FALSE))$importance[2,]
            }
            
            
            IVs.result.boot<- matrix(nrow = n.sample, ncol= ncol(matrix.M), byrow = TRUE)
            for(i in 1:length(metric.sqrt.corr.boot)){
                metric.sqrt.corr.boot[[i]] 
                for(j in 1:ncol(matrix.M)){
                   IV.result.boot[i,j]<- metric.sqrt.corr.boot[[i]][j,] %*% as.matrix(prop.var.boot[[i]])
                }
            }        
            
            IV.bootstrap.result<- setNames(list(IVs.result, IV.result.boot), c("IV.obs", "IV.boot"))
            return(IV.bootstrap.result)
        }
    }
    
    if(scale == FALSE){
        metric.sqrt.corr<- (prcomp(x = matrix.M, scale.= FALSE)$rotation ^ 2)
        prop.var<- summary(prcomp(x = matrix.M, scale.= FALSE))$importance[2,]
        
        names.matrix.IV<- list("IV.resu", colnames(matrix.M))
        IVs.result<- matrix(nrow= 1, ncol= ncol(matrix.M), dimnames= names.matrix.IV)
        for(i in 1:nrow(metric.sqrt.corr)){
            IVs.result[,i]<- metric.sqrt.corr[i,] %*% as.matrix(prop.var)
        }
        
        if(IV.bootstrap == FALSE){
            return(IVs.result)
        }
        
        if(IV.bootstrap == TRUE) {
            matrix.M.boot<- vector("list", n.sample)
            for(i in 1:n.sample) {
                matrix.M.boot[[i]]<- matrix.M[sample(1:nrow(matrix.M.stand), replace= TRUE),]
            }
            
            names.IV.result.boot<- list(c(1:n.sample), colnames(matrix.M))
            IV.result.boot<- matrix(nrow= n.sample, ncol= ncol(matrix.M), dimnames= names.IV.result.boot)
            metric.sqrt.corr.boot<- vector("list", n.sample)
            prop.var.boot<- vector("list", n.sample)
            for(i in 1:length(matrix.M.boot)){ 
                metric.sqrt.corr.boot[[i]]<- (prcomp(x = matrix.M.boot[[i]], scale.= FALSE)$rotation ^ 2)
                prop.var.boot[[i]]<- summary(prcomp(x = matrix.M.boot[[i]], scale.= FALSE))$importance[2,]
            }
            
            
            IVs.result.boot<- matrix(nrow = n.sample, ncol= ncol(matrix.M), byrow = TRUE)
            for(i in 1:length(metric.sqrt.corr.boot)){
                metric.sqrt.corr.boot[[i]] 
                for(j in 1:ncol(matrix.M)){
                    IV.result.boot[i,j]<- metric.sqrt.corr.boot[[i]][j,] %*% as.matrix(prop.var.boot[[i]])
                }
            }        
            
            IV.bootstrap.result<- setNames(list(IVs.result, IV.result.boot), c("IV.obs", "IV.boot"))
            return(IV.bootstrap.result)
        }
    }
}
