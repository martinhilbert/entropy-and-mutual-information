###############################################################
# ENTROPTY AND MUTUAL INFROMATION ESTIMATION FROM DISTRIBUTIONS
###############################################################

#####################################
## LOAD DISTRIBUTIONS P(X), P(Y) and P(Y|X)
#####################################

# Set working directory
setwd("C:/Users/mhilbert/OneDrive/Analytics Software/R_simple channel")

# import the marginal distributions: the spread sheets are set up to have the varaibles in the 1st column (top-down) and the probabilities in the second. No header in the row.
pxRaw <- read.csv("px1.csv",stringsAsFactors=F,header = F)  #exchange px1.csv with your own .csv data
pyRaw <- read.csv("py1.csv",stringsAsFactors=F,header = F)  #exchange py1.csv with your own .csv data
# import the conditional (noise) distributions: the spread sheet is set up to have the X varaible in the 1st column (top-down) and Y variable in the 1st row (left-right). This gives both a natural header. Probabilities starting from 2nd row, 2nd columnn. 
py_xRaw <- read.csv("pY_x1.csv",stringsAsFactors=F,header = F)   #exchange pY_x1.csv with your own .csv data


## CREATE CLASSES AND BINS
classesX=pxRaw[,1] # Get the different clasess of X
classesY=pyRaw[,1] # Get the different clasess of X

# Number of clases in the histogram
nclass=matrix(nrow = 2, ncol = 1)
nclass[1]=length(classesX)
nclass[2]=length(classesY)

# identify variable
px=pxRaw[,2]
py=pyRaw[,2]
pY_X=py_xRaw[1:nclass[1]+1,1:nclass[2]+1]

#print the nubmer of classes and their values
print(paste0("There are ", nclass[1], " different classes in X : "))
print(classesX)

print(paste0("There are ", nclass[2], " different classes in Y : "))
print(classesY)

# Create a dataframe for the marginal probabilities and print
pxdf = data.frame(px,row.names=classesX) 
print(pxdf)

pydf = data.frame(py,row.names=classesY) 
print(pydf)

# Create a dataframe for the conditional (noise) probability and print
px_ydf=data.frame(pY_X,row.names=classesX) 
names(px_ydf)=classesY
print("P(Y|X)=P(Y,X)/P(X)")
print(px_ydf)


#####################################
## CALCULATE JOINT PROBABILITIES FROM P(X) and P(Y|X)
#####################################

##JOINT PROBABILITY  P(X)*P(Y|X)=P(Y,X)
  #Define joint probability
probxy <- function(px,pY_X){
    pxy=px*pY_X
  return(pxy)
}

  #Calculate joint probability
pxy=probxy(px,pY_X)

  # Create a dataframe for the joint probability and print
pxydf=data.frame(pxy,row.names=classesX) 
names(pxydf)=classesY
print("P(X,Y)")
print(pxydf)


#####################################
## INFORMATION MEASURES  
#####################################


## ENTROPIES
  #Define entropy
entropy <- function(px,nclass){
  
  H=-sum(px*log2(px))
  
  return(H)
}

  # Calculate H(x)
Hx=entropy(px)
print(paste0("H(x)=",Hx))

  # Calculate H(y)
Hy=entropy(py)
print(paste0("H(y)=",Hy))


## MUTUAL INFORMATION
  #Define Mutual Information
mutualInformation <- function(px,py,pxy){
  MI = 0
  for (i in 1:dim(pxy)[1]) {
    for (j in 1:dim(pxy)[2]) {
      if(!pxy[i,j]==0){
        MI=MI+pxy[i,j]*log2(pxy[i,j]/(px[i]*py[j]))
      }
    }
  }
  return(MI)
}


  #Calculate Mutual information I(x,y) 
MI=mutualInformation(px,py,pxy)
print(paste0("I(x,y)=",MI))


## JOINT ENTROPY
  # Calculate Joint entropy H(x,y) 
Hyx=Hx+Hy-MI
print(paste0("H(x,y)=",Hyx))


## CONDITIONAL ENTROPIES
  #Calculate Noise H(y|x)
Hy_x=Hy-MI
print(paste0("H(y|x)=",Hy_x))

  #Calculate equivocation H(x|y)
Hx_y=Hx-MI
print(paste0("H(x|y)=",Hx_y))

