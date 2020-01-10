#######################################################
# ENTROPTY AND MUTUAL INFROMATION ESTIMATION FROM DATA
#######################################################

library(gplots) #Load R-Library for plotting and histograms

#####################################
## LOAD DATA
#####################################

## You can either input the data manually, like this, here:
# x=c(1,1,1,0,1,1,0,1,0,0,1,1,0,0,1,1,1,0,1,1,0,1)
# y=c(1,0,1,0,1,0,1,0,1,1,0,1,1,0,1,1,0,1,1,0,0,1)

## Or upload empirical data from a csv file:
# Set working directory
setwd("C:/Users/mhilbert/OneDrive/Analytics Software/R_simple channel")

# import datafile: the input and output columns have to have the same length (number of rows). The file is set up to have a header in the first row; an ID in the 1st column, values for X in the 2nd column (e.g. sender), values for Y in the 3rd column (e.g. receiver).
data <- read.csv("TestCase.csv",stringsAsFactors=FALSE)  # exchange TestCase.csv with your own .csv dataset
xRaw=data[,2] # assigns the values of the 2nd colum to X
yRaw=data[,3] # assigns the values of the 3rd colum to Y


## CREATE CLASSES AND BINS
nDatax=length(xRaw) # count the size of the data set
# Get the unique values of the vectors
classesX=sort(unique(xRaw))
classesY=sort(unique(yRaw))
# Number of bin clases in the histogram
nclass=matrix(nrow = 2, ncol = 1)
nclass[1]=length(classesX)
nclass[2]=length(classesY)

# This will print out the size and number of classes (you can comment it off)
print(paste0("There are ", nDatax, " data points, with ", nclass[1], " different classes in X, namely: ")) 
print(classesX)

print(paste0("There are ", nDatax, " data points, with ", nclass[2], " different classes in Y, namely: ")) 
print(classesY)


# assign numeric identifiers to categorical characters 
idClassesX=seq(0, nclass[1]-1,1) # idClasses=1:nclass
idClassesY=seq(0, nclass[2]-1,1)
x=matrix(nrow = nDatax, ncol = 1) # replace the characters with numeric identificators, create vectors
y=matrix(nrow = nDatax, ncol = 1)
# Fill the vectors with the numeric id for each class
for(i in 1:nclass[1]) {
  x[xRaw==classesX[i]]=idClassesX[i]
}
for(i in 1:nclass[2]) {
  y[yRaw==classesY[i]]=idClassesY[i]
}


#####################################
## PROBABILITIES 
#####################################

## MARGINAL PROBABILITIES
  # Define the function that is a marginal probability
prob <- function(x,nclass){
  if (nclass==0){#If number of classes is equal to 0, then specify manualy the bins for the histogram
    bins = seq(min(x)-0.5, max(x)+0.5)
    px=hist(x,plot = FALSE,breaks=bins)
  }
  if (nclass>0){#If number of clases is > to 0, then specify the bins using number of classes
    px=hist(x,plot = FALSE,nclass=nclass)
  }
  #Convert frequency count into probability [0,1]
  px=px$counts/sum(px$counts)
  return(px)
}

  # Calculate marginal probabilities P(X) and P(Y)
px=prob(x,0) # using the option nclass=0 for automaticaly selection of the number bins for the histogram, otherwise specify the number of classes you want by replacing 0 with another number 
py=prob(y,0)
pxdf = data.frame(px,row.names=classesX) # Create a dataframe for the marginal probabilities
pydf = data.frame(py,row.names=classesY) 

print(pxdf) # print the marginal probability of X
print(pydf) # print the marginal probability of Y


## JOINT PROBABILITY
  # Define the function that is a joint probability
probxy <- function(x,y,nclass){
  xy <- data.frame(x,y)
  pxy <- hist2d(xy,nbins=nclass,show=FALSE)
  #Convert frequency count into probability [0,1]
  pxy=pxy$counts/pxy$nobs
  
  return(pxy)
}

  # Calculate joint probability P(X,Y)
pxy=probxy(x,y,nclass)
pxydf=data.frame(pxy,row.names=classesX)  # Create a dataframe for the joint probabilities
names(pxydf)=classesY

print(pxydf) # print the 2D matrix of the joint distribution among classes


## CONDITIONAL (NOISE) PROBABILITY
  #Define conditional (noise) probability (based on X)
conditionalProb <- function(x,y,nclass){
  px = prob(x,0)
  py = prob(y,0)
  pxy = probxy(x,y,nclass)
    
    pY_X =matrix(nrow=dim(pxy)[1],ncol=dim(pxy)[2]) # Create a vector to store the conditional probabilities
    
    for (i in 1:dim(pxy)[1]) {
      for (j in 1:dim(pxy)[2]) {
        pY_X[i,j]=pxy[i,j]/px[i]  # Calculate the conditional probability
      }
    }
   return(pY_X) 
}

  # Calculate conditional (noise) probability
pY_X=conditionalProb(x,y,nclass)
px_ydf=data.frame(pY_X,row.names=classesX) 
names(px_ydf)=classesY
print("P(Y|X)=P(Y,X)/P(X)")
print(px_ydf)



#####################################
## INFORMATION MEASURES  
#####################################

## ENTROPIES
  #Define entropy
entropy <- function(x,nclass){
  px=prob(x,0)
  H=-sum(px*log2(px))
  return(H)
}

  # Calculate H(x)
Hx=entropy(x,nclass[1])
print(paste0("H(x)=",Hx))

  # Calculate H(y)
Hy=entropy(y,nclass[2])
print(paste0("H(y)=",Hy))


## MUTUAL INFORMATION
  #Define Mutual Information
mutualInformation <- function(x,y,nclass){
  px=prob(x,0) # Calculate marginal probability (X)
  py=prob(y,0) # Calculate marginal probability (Y)
  pxy=probxy(x,y,nclass) # Calculate joint probabilitu (x,Y)
  
  MI = 0
  for (i in 1:nclass[1]) {
    for (j in 1:nclass[2]) {
      if(!pxy[i,j]==0){
        MI=MI+pxy[i,j]*log2(pxy[i,j]/(px[i]*py[j]))
      }
    }
  }
  return(MI)
}

# Calculate I(x,y)
MI=mutualInformation(x,y,nclass)
print(paste0("I(x,y)=",MI))


## JOINT ENTROPY
  # Calculate H(x,y)
Hyx=Hx+Hy-MI
print(paste0("H(x,y)=",Hyx))

## CONDITIONAL ENTROPIES
# H(x|y)
Hx_y=Hx-MI
print(paste0("H(x|y)=",Hx_y))

# H(y|x)
Hy_x=Hy-MI
print(paste0("H(y|x)=",Hy_x))



#####################################
## BOOTSTRAPPING SIGNIFICANCE TESTS  
#####################################

# number Of Samples:
numberSamples=1000
# Get random seeds for every bootstrap
randomSeed=floor(runif(numberSamples, 1,100000))
# randomSeed=1:numberSamples
set.seed(666)


## BOOTSTRAP MARGINAL VARIABLE
  # Define bootstrap of a marginal variable
bootstrap1 <- function(x){
  nDatax=length(x)  # Get the lenght of the vectors
  
  posBS=floor(runif(nDatax, 1,nDatax))   #Select randomly with repetition the same number of observations
  bx=x[posBS]  # asign them to new vectors
  
  return(bx)
}


  # Bootstrap Y
byMI=matrix(NA,nrow = numberSamples, ncol = 1) # Create a matrix for all the statistics
diffbyMI=matrix(nrow = numberSamples, ncol = 1) # Create a matrix for all the differences

for(k in 1:numberSamples){
  set.seed(randomSeed[k])
  
  by=bootstrap1(y)   #Select randomly with repetition the same number of observations
  
  byMI[k]=mutualInformation(x,by,nclass)   # I(x,y) Mutual information
  diffbyMI[k]=byMI[k]-MI   # Calculate the difference with the original
}

  #Calculate differences for bootstrapped Y
ShareMIby=sum(as.numeric(diffbyMI>0),na.rm = T)/sum(as.numeric(!is.nan(diffbyMI))) # Calculate % of estimations greater than MI

hist(byMI,main = paste("I(X,Y) =" , MI,"% > =",ShareMIby))
lines(rbind(c(MI,sum(as.numeric(!is.nan(diffbyMI)))),c(MI,0)),lwd=3,col="red")

print(paste0("Share of MI(X,bootstrapped Y) > MI(X,Y)=",ShareMIby))



# Bootstrap X
bxMI=matrix(NA,nrow = numberSamples, ncol = 1) # Create a matrix for all the statistics
diffbxMI=matrix(nrow = numberSamples, ncol = 1) # Create a matrix for all the differences

for(k in 1:numberSamples){
  set.seed(randomSeed[k])
  
  bx=bootstrap1(x)   #Select randomly with repetition the same number of observations
  
  bxMI[k]=mutualInformation(bx,y,nclass)   # I(x,y) Mutual information
  diffbxMI[k]=bxMI[k]-MI   # Calculate the difference with the original
}

#Calculate differences for bootstrapped X
ShareMIbx=sum(as.numeric(diffbxMI>0),na.rm = T)/sum(as.numeric(!is.nan(diffbxMI))) # Calculate % of estimations greater than MI

hist(bxMI,main = paste("I(X,Y) =" , MI,"% > =",ShareMIbx))
lines(rbind(c(MI,sum(as.numeric(!is.nan(diffbxMI)))),c(MI,0)),lwd=3,col="red")

print(paste0("Share of MI(bootstrapped X , Y) > MI(X,Y)=",ShareMIbx))




## BOOTSTRAP JOINT VARIABLE
  # Define bootstrap of a joint variable (x,y) pairs
bootstrap2 <- function(x,y){
  nDatax=length(x)  # Get the lenght of the vectors
  nDatay=length(y)
  if(nDatax==nDatay){#If they are the same length, select randomly with repetition the same number of observations 
    posBS=floor(runif(nDatax*2, 1,nDatax)) 
    bx=x[posBS] # asign them to new vectors
    by=y[posBS]
    bxy=matrix(nrow = nDatax*2, ncol = 2)   # merge the two vector into one to use as retuning value of the funtion 
    bxy[,1]=bx
    bxy[,2]=by
  }else{
    print("SIZE OF X AND Y MUST BE THE SAME")
    bxy = NaN
  }
  return(bxy)
}


  #Bootstrap (x,y) pairs
bxy=bootstrap2(x,y)
bx=bxy[,1]
by=bxy[,2]

bMI=matrix(NA,nrow = numberSamples, ncol = 1) # Create a matrix for all the statistics
diffbMI=matrix(nrow = numberSamples, ncol = 1) # Create a matrix for all the differences

for(k in 1:numberSamples){
  set.seed(randomSeed[k])
  bxy=bootstrap2(x,y)
  bx=bxy[,1]
  by=bxy[,2]
  
  bMI[k]=mutualInformation(bx,by,nclass) # I(x,y) Mutual information

    diffbMI[k]=bMI[k]-MI  # Calculate the difference with the original
}

# Estimate the % of estimations greater than MI and  P(Y|X)
ShareMIb=sum(as.numeric(diffbMI>0),na.rm = T)/sum(as.numeric(!is.nan(diffbMI)))


hist(bMI,main = paste("I(X,Y) =" , MI,"% > =",ShareMIb))
lines(rbind(c(MI,sum(as.numeric(!is.nan(diffbMI)))),c(MI,0)),lwd=3,col="red")

print(paste0("MI(bootstrapped pairs  x,y) > MI(x,y)=",ShareMIb))



# Calculate and print joint probability of bootstrapped pairs (x,y)
pbxy=probxy(bx,by,nclass)
pbxydf=data.frame(pbxy,row.names=classesX)  # Create a dataframe for the joint probabilities
names(pbxydf)=classesY

print(pbxydf) # print the 2D matrix of the joint distribution among classes

