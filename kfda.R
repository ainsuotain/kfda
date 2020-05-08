# kfda: Kernel Fisher Discriminant Analysis in r
# Published:	2017-09-27
# Author:	Donghwan Kim
# Maintainer:	Donghwan Kim <donhkim9714 at korea.ac.kr, dhkim2@bistel.com>
# This package is a simple and practical package for KFDA based on the paper of Yang, J., Jin, Z., Yang, J. Y., Zhang, D., and Frangi, A. F. (2004) <doi:10.1016/j.patcog.2003.10.015>.


#### pkgs ####
library(kernlab)
library(MASS)

#### main functions ####
kfda = function(trainData = data, kernel.name = "rbfdot", kpar.sigma = 0.001, threshold = 1e-8){
  
  kfda.models = list()
  class(kfda.models) = "Kernel Fisher Discriminant Analysis"
  
  # kpca
  kpca.train = kpca(~.,
                     data = trainData[, -dim(trainData)[2]],
                     kernel = kernel.name,
                     kpar = list(sigma = kpar.sigma),
                     th = threshold
  )
  
  kpca.rotation.train = as.data.frame(cbind(kpca.train@rotated, trainData[, dim(trainData)[2]]))
  colnames(kpca.rotation.train)[dim(kpca.rotation.train)[2]] = c("Y")
  kpca.rotation.train$Y = trainData[, dim(trainData)[2]]
  
  # kpca + lda train phase
  lda.rotation.train = lda(kpca.rotation.train$Y~.,
                            data = kpca.rotation.train)
  
  LDs = kpca.train@rotated%*%as.matrix(lda.rotation.train$scaling)
  labels = trainData[, dim(trainData)[2]]
  
  kfda.models$kpca.train = kpca.train
  kfda.models$lda.rotation.train = lda.rotation.train
  kfda.models$LDs = LDs
  kfda.models$label = labels
  
  return(kfda.models)
}



## EOS
