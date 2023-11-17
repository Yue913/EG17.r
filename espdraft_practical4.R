# Members in group 17: Linfan Shi s2508485 Xinyue Huang s2504007 Yue Yu s2496178
# Github repo address: https://github.com/Yue913/ESP17.r.git
# Contribution:
#
#

# This project sets up a basic neural network for classification. The network
# contains layers of nodes, each fully interconnected with every consecutive layer.
# These Layers include an input layer, several hidden layers and an output layer.
# Every node in this network holds a value. After assigning values to the input 
# layer nodes, the network combines and transforms these values across each layer,
# terminating at the output layer. The process involves ReLU transformation and 
# parameter tuning. Stochastic gradient descent refines the parameters by reducing
# the loss. The back-propagation algorithm is used for efficient gradient computation.  

# The project comprises six parts, starting with the initialization of node values
# and parameters using the netup() function. Node values are then processed by 
# the forward() function, followed by the application of back-propagation through  
# the backward() function to compute loss gradients. In part 4, the train() 
# function is used to iteratively refine the network's parameters via minibatch 
# processing, integrating both forward and backward passes. Once all functions 
# are established, a 4-8-7-3 network is trained on the 'iris' dataset for species 
# classification. Finally, the model's performance is evaluated using the rate  
# of misclassification on test data.

# Part 1

# In this part, function netup() initializes node values and parameters in the 
# neural network according to the given structure. These will be used in the ReLU 
# transformation, and the parameters will be tuned subsequently. 

# Input 
# d: A vector giving the number of nodes in each layer of a network.

# Output
# nn: A network list containing the following elements:
# h, A list which will contain the node values for each layer.
# W, A list of weight matrices linking consecutive layers.
# b, A list of offset vectors linking consecutive layers.

netup <- function(d){
  n <- length(d);h <- W <- b <- list()
  for (i in 1:(n-1)){
    h[[i]] <- rep(NA,d[i])
    
    # W[[i]] is a d[i+1] by d[i] matrix linking layer i to layer i+1, with its 
    # elements initially distributed uniformly between 0 and 0.2.
    W[[i]] <- matrix(runif(d[i+1]*d[i],0,0.2),d[i+1],d[i])
    
    # Offset vector b[[i]] with with its components initially distributed 
    # uniformly between 0 and 0.2.
    b[[i]] <- runif(d[i+1],0,0.2)
  }
  h[[n]]<-rep(NA,d[n])
  nn <- list(h = h,W = W,b = b)
  return(nn)
}

# Part 2

# In this part, the function forward() computes the remaining node values using 
# the ReLU transformation and returns a new 'nn' containing both parameters and 
# updated node values.

# Input
# nn: A network list returned by function netup()
# inp: A vector of input values for the first layer. 

# Output
# nn: The updated network list.

forward <- function(nn,inp){
  h <- nn$h
  W <- nn$W
  b <- nn$b
  h[[1]] <- inp
  for (i in 1:(length(h)-1)){
    
    # The node values at the next layer are calculated using ReLU(g) = max(0,g), 
    # where g is calculated as the product of weights matrix and the node values  
    # of the current layer, plus the bias.
    g <- W[[i]]%*%h[[i]]+b[[i]]
    
    # The function pmax() is used to compare each element of the vector g with 0 
    # and return the larger of the two. 
    h[[i+1]]  <- pmax(0,g)
  }
  nn <- list(h=h,W=W,b=b)
  return(nn) 
}

# Part 3

# In this part, the function backward() implements the back-propagation algorithm
# by computing the gradients of the loss function with respect to node values and
# parameters, which will be used for tuning the parameters during the following
# training process. The function computes the gradient one data point at a time.

# Input
# nn: The updated network list returned from function forward()
# k: output class 

# Output
# A list containing the following elements:
# dh: Derivatives of the loss function w.r.t. the nodes
# dW: Derivatives of the loss function w.r.t. the weights 
# db: Derivatives of the loss function w.r.t. the offsets

backward <- function(nn,k){
  
  #initialize list of derivatives with corresponding dimension
  dW <- nn$W 
  db <- nn$b
  h <- dh <- nn$h
  
  # number of layers
  L=length(dh) 
  
  minus <- rep(0,length(dh[[L]]));minus[k] <- 1
  
  # Compute the derivatives of loss function w.r.t. node values of the output layer
  dh[[L]] <- exp(h[[L]])/sum(exp(h[[L]]))-minus
  
  # Compute the derivatives of loss function w.r.t. all the other node values by
  # back-propagation using chain rule in differentiation.
  for (l in 1:(L-1)){
    d <- dh[[L-l+1]]*(h[[L-l+1]]>0)
    dh[[L-l]] <- t(dW[[L-l]])%*%d
    
    # Calculate the gradient of weights.
    dW[[L-l]] <- d%*%t(h[[L-l]])
    
    # The derivative of the loss function w.r.t. the offset values is just d of 
    # the next layer
    db[[L-l]] <- d
    
  }
  return(list(dh=dh,dW=dW,db=db))
}

# Part 4

# In this part, the function train() iteratively updates the network's parameters
# by mainly using function forward() to obtain the prediction of classification 
# and function backward() to obtain the gradient. The function processes data in
# minibatches, computes the gradients, and updates the parameters to minimize the 
# loss function.  

# Input
# nn: The network list returned from function netup()
# inp: A matrix with the input data arranged in the rows
# k: A vector with corresponding labels
# eta: the step length used for parameter updating
# mb: the number of data randomly sampled to compute the gradient
# nstep: the number of optimization steps

# Output
# A list containing the following elements:
# h: A list of node values for each layer
# b: A list of tuned offset vectors
# W: A list of tuned weight matrices

train <- function(nn,inp,k,eta=.01,mb=10,nstep=10000){
  nnb <- nn$b;lb <- length(unlist(nnb))
  nnW <- nn$W;lW <- length(unlist(nnW))
  
  # Iterative training loop over the specified number of steps.
  for (i in 1:nstep){
    
    # Randomly select indices to form a minibatch.
    minibatch <- sample(nrow(inp),mb)
    derb <- rep(0,lb)
    derW <- rep(0,lW)
    
    # A 'for' loop is used to process each sample in the minibatch.
    for (j in 1:mb){
      prediction <- forward(nn,inp[minibatch,][j,])
      derivative <- backward(prediction,k[minibatch][j])
      
      # Aggregate gradients of biases and weights.
      derb <- derb+unlist(derivative$db)
      derW <- derW+unlist(derivative$dW)
    }
    
    # Update the biases and weights based on aggregated gradients.
    nnb <- relist(unlist(nnb)-eta*derb/mb,skeleton=nnb)
    nnW <- relist(unlist(nnW)-eta*derW/mb,skeleton=nnW)
    nn$b <- nnb
    nn$W <- nnW
  }
  return(list(h=nn$h,b=nnb,W=nnW))
} 

# Part 5

# In this part, we use the functions defined above to train a 4-8-7-3 network for 
# classifying irises species based on four characteristics from the 'iris' dataset.
# The dataset is divided into training and test data; the test data consist of
# every 5th row starting from the 5th row. The remainder is used for training.

# Define the structure of the neural network
d <- c(4,8,7,3)
nn <- netup(d)
data(iris)

# Create indices for test data: every 5th row starting from the 5th row.
indices <- seq(from = 5, to = nrow(iris), by = 5)

# Convert species names into numeric factors for training and testing.
k <- as.numeric(as.factor(iris$Species))

k_train <- k[-indices]
k_test <- k[indices]
inp_train <- as.matrix(iris[-indices, 1:4])
inp_test <- as.matrix(iris[indices, 1:4])

# Train the neural network using the training data.
predict <- train(nn,inp_train,k_train,eta=.01,mb=10,nstep=10000)

# Part 6

# In this part, the test data are used to make predictions using the tuned 
# parameters obtained from Part 5. The misclassification rate is then calculated 
# to evaluate the neural network's performance.

index <- rep(0,nrow(inp_test))

# A 'for' loop is used to iterate over each test sample.
for (i in 1:nrow(inp_test)){
  output <- forward(predict,inp_test[i,])$h[[4]]
  
  # The label corresponding to the largest output values is the predicted outcome
  index[i] <- which(output == max(output))
}

# Calculate the misclassification rate by comparing the predicted class with the
# actual test labels.
difference_rate <- sum(index != k_test)/nrow(inp_test)

