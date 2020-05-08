# kfda: Kernel Fisher Discriminant Analysis in r
# Published:	2017-09-27
# Author:	Donghwan Kim
# Maintainer:	Donghwan Kim <donhkim9714 at korea.ac.kr, dhkim2@bistel.com>
# This package is a simple and practical package for KFDA based on the paper of Yang, J., Jin, Z., Yang, J. Y., Zhang, D., and Frangi, A. F. (2004) <doi:10.1016/j.patcog.2003.10.015>.


#### pkgs ####
library(kernlab)
library(MASS)

#### predict() functions ####
kfda_predict = function(object = obj, testData = data){
  
  # kpca
  predict.kpca = predict(object = object$kpca.train,
                          testData
  )
  
  # kpca + lda = kfda
  predicted.dataPoints = as.matrix(predict.kpca)%*%as.matrix(object$lda.rotation.train$scaling)
  pre = predict(object = object$lda.rotation.train,
                 newdata = as.data.frame(predict.kpca)
  )
  
  return(pre)
  
}


# EOS
