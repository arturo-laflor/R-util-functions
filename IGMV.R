IGMV<-function(dfIGMV){
  
  ################################################################################################
  #regresa un vector con los registros de la variable objetivo utilizando como filtro 
  #el nivel de la variable predictiva a la que se desea calcular su ganancia
  #Recibe el nivel para el que se está calculando el remainder que posteriormente se 
  #utilizará para la ganancia.
  parcialVector<-function(nivel,predictiva,objetivo){
    tablaPV<-cbind.data.frame(pred=predictiva,obj=objetivo)
    names(tablaPV)<-c("pred","obj")
    parcialTabla<-subset(tablaPV,pred==nivel)
    parcialTabla$obj
  }
  ################################################################################################
  
  ################################################################################################
  oneVectorEntropy<-function(variable){
    #Author: Charles Determan Jr.
    #Grade: Data Scientist at Healthgrades
    #linkedin: https://www.linkedin.com/in/cdeterman
    #edited by: Arturo Laflor
    #Descripcion: Calcula la entropía de un vector que en este caso para un 
    #dataframe cada columna será un vector
    freqs <- table(variable)/length(variable)
    -sum(freqs * log2(freqs))  
  }
  ################################################################################################
  
  ################################################################################################
  # recibe un dataframe como este:
  #   # nivel         entropia    peso
  #   # nunca            0.34     0.21
  #   # algunas veces    0.56     0.12
  #   # siempre          0.14     0.30
  # calcula el remainder
  #regresa un valor nuemerico
  
  remainderOneVariable<-function(dfWE){
    sum(apply(dfWE[,2:3],1,prod))
  }
  ################################################################################################
  
  ################################################################################################
  # calcula el nivel de entropia y el peso por nivel para una variable
  # utiliza parcialVector
  # recibe un vector que es una variable categorica y la variable objetivo
  # regresa una dataframe como el siguiente
  
  # nivel         entropia    peso
  # nunca            0.34     0.21
  # algunas veces    0.56     0.12
  # siempre          0.14     0.30
  
  entropyWeightOneVariable<-function(predictive,target){
    
    #test code
    # datos<-read.csv(file="C:/Users/artur/Google Drive/DAIH/Paper komputer sapiens/ejemplo-enf.csv",header = T,sep = ",")
    # predictive<-datos$Estornudos
    # target<-datos$Enfermedad
    #test code
    
    predictiveLeves<-table(predictive)
    matrixRes<-matrix(NA,nrow = length(predictiveLeves),ncol = 3)
    
    for(i in 1:length(predictiveLeves)){
      #names(predictiveLeves)[i]
      entropyLevel<- round(oneVectorEntropy(c(parcialVector(names(predictiveLeves)[i],predictive,target))),digits = 3)
      weightLevel<-round(predictiveLeves[[i]]/length(target),digits = 3)
      matrixRes[i,]<-c(names(predictiveLeves)[i],entropyLevel,weightLevel)
    }
    #fue necesario hacer columna por columna para poner el tipo de dato correcto
    dfRes<-cbind.data.frame(Nivel=as.factor(matrixRes[,1]),Entropia=as.numeric(matrixRes[,2]),Peso=as.numeric(matrixRes[,3]))
    
  }
  ################################################################################################
  
  ################################################################################################
  #Recibe una variable predictiva y la variable objetivo
  #Regresa la ganancia de informacion de una variable predictiva
  informationGainOneVariable<-function(ColumnIG,targetIG){
    #testcode
    # ColumnIG<-datos$Pamarilla
    # targetIG<-datos$Enfermedad
    #testcode
    
    dfWEIG<-entropyWeightOneVariable(ColumnIG,targetIG)
    remainderIG<-remainderOneVariable(dfWEIG)
    targetEntropy<-oneVectorEntropy(targetIG)
    IG<-round(targetEntropy-remainderIG,digits = 3)
  }
  ################################################################################################
  
  ################################################################################################
  ####################################Information Gain Multi Variable#############################
  #se asume que la variable objetivo está en la última columna del dataset
  #testercode
  #dfIGMV<-datos
  #testercode
  dfInformationGain<-apply(dfIGMV[,1:(ncol(dfIGMV)-1)],2,FUN=informationGainOneVariable,dfIGMV[,ncol(dfIGMV)])
  prdf<-data.frame(dfInformationGain)  
  prdf<-cbind.data.frame(Variable=row.names(prdf),Ganancia=prdf[,1])
}
################################################################################################