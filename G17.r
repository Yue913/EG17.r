# Members in group 17: Linfan Shi s2508485 Xinyue Huang s2504007 Yue Yu s2496178
# Github repo address: https://github.com/Yue913/ESP17.r.git
# Contribution:
#
#
#



# Overview: This project sets up a basic neural network for classification. The network
# contains layers of nodes, each fully interconnected with every consecutive layer. 
# The main mechanism of neural network consist of two parts: forward prediction 
# and backward training.
#
# In forward prediction, after assigning values to the input layer nodes, the network
# combines (via linear transformation and non-linear activation) present layer and 
# transforms across each layer, with the last (output) layer representing some forms
# of prediction probability for each classes. 
# In backward training, The training process is based on minimizing loss function by 
# gradient descent. The main technique used is called back-propagation, which is an
# efficient way of computing gradients combining of chainrule and memorization.
#
# The three main choice of structure in project are:
# (1) use ReLU activation function.
# (2) use exponential values of output layer (divided by sum) as prediction probability.
# (3) use cross entropy error as loss function.
# In addition, stochastic gradient descent is used with efficient purpose.
#
# The project comprises six parts, In part 1-3, node and parameters first initialized
# by netup() function. Node values are then processed by the forward() function, 
# followed by the computation of gradients through backward() function. In part 4, 
# the train() function is used to iteratively refine the network's parameters. Once all 
# functions are established, in part 5 a 4-8-7-3 network is trained on the 'iris' dataset
# for species classification. Finally in part 6, the model's performance is evaluated using
# the rate of misclassification on test data and decreased amount of loss.



# Part 1
# In this part, function netup() initializes node values and parameters in the 
# neural network according to the given structure.
# Input 
# d: A vector giving the number of nodes in each layer of a network.
# Output
# nn: A network list containing the following elements:
# h, A list of vectors storing the node values for each layer.
# W, A list of weight matrices linking consecutive layers.
# b, A list of offset vectors linking consecutive layers.

netup <- function(d){
  
  n <- length(d);h <- W <- b <- list()
  for (i in 1:(n-1)){
    h[[i]] <- rep(NA,d[i])
    # W[[i]] is a d[i+1] by d[i] matrix linking layer i to layer i+1, with its 
    # elements initially sample from uniform distribution between 0 and 0.2.
    W[[i]] <- matrix(runif(d[i+1]*d[i],0,0.2),d[i+1],d[i])
    # Offset vector b[[i]] with with its components initially sampled from 
    # uniform distribution between 0 and 0.2.
    b[[i]] <- runif(d[i+1],0,0.2)
  }
  h[[n]]<-rep(NA,d[n])
  nn <- list(h = h,W = W,b = b)
  return(nn)
  
}



# Part 2
# In this part, the function forward() computes the remaining node values and 
# returns a new 'nn' containing both parameters and updated node values. The 
# function updates for one data point at a time.
# Input
# nn: A network list of specified structure
# inp: A vector of input values to be classified. 
# Output
# nn: The updated network list.

forward <- function(nn,inp){

  # load parameters and structures
  h <- nn$h
  W <- nn$W
  b <- nn$b
  
  h[[1]] <- inp
  for (i in 1:(length(h)-1)){
    # The node values at the next layer are calculated using ReLU(g) = max(0,g), 
    # where g is calculated as the product of weights matrix and the node values  
    # of the current layer, plus the bias.
    g <- as.vector(W[[i]]%*%h[[i]])+b[[i]]
    h[[i+1]]  <- g*(g>0)
  }
  nn <- list(h=h,W=W,b=b)
  return(nn) 
}



# Part 3
# In this part, the function backward() implements the back-propagation algorithm
# by computing the gradients of the loss function with respect to node values and
# parameters. The function computes the gradient of one data point at a time.
# Input
# nn: The updated network list (with input value)
# k: output class 
# Output
# A list containing the following elements:
# n,W,b: character vectors and matrices of the input neural network used for prediction
# dh: Derivatives of the loss function w.r.t. the nodes
# dW: Derivatives of the loss function w.r.t. the weights 
# db: Derivatives of the loss function w.r.t. the offsets

backward <- function(nn,k){
  
  #initialize list of derivatives with corresponding dimension
  dW <- nn$W 
  db <- nn$b
  h <- dh <- nn$h
  L=length(dh) # number of layers

  # Compute the derivatives of loss function w.r.t. node values of the output layer
  minus <- rep(0,length(dh[[L]])); minus[k] <- 1
  dh[[L]] <- exp(h[[L]])/sum(exp(h[[L]]))-minus
  
  # Iteratively compute the derivatives of loss function for other layers
  for (l in 1:(L-1)){
    d <- dh[[L-l+1]]*(h[[L-l+1]]>0)
    dh[[L-l]] <- t(dW[[L-l]])%*%d # gradient of node values
    dW[[L-l]] <- d%*%t(h[[L-l]]) # gradient of weights matrix
    db[[L-l]] <- d # gradient of offset vector
  }
  return(list(h=nn$h,W=nn$W,b=nn$b,dh=dh,dW=dW,db=db))
}



# Part 4
# In this part, the function train() iteratively updates the network's parameters
# by mainly using function forward() to obtain the prediction of classification 
# and function backward() to obtain the gradient. The function processes data in
# minibatches, computes the gradients, and updates the parameters to minimize the 
# loss function.  
# Input
# nn: The network list returned from function netup()
# inp: A matrix with each observation arranged in the rows
# k: A vector with corresponding labels
# eta: the step length used for parameter updating
# mb: the number of data randomly sampled for each minibatch to compute the gradient
# nstep: the number of optimization steps
# Output
# A list containing the following elements:
# h: A list that has the same layer size as that of input neural network
# b: A list of tuned offset vectors
# W: A list of tuned weight matrices

train <- function(nn,inp,k,eta=.01,mb=10,nstep=10000){
  # load structures of neural network
  nnb <- nn$b;lb <- length(unlist(nnb))
  nnW <- nn$W;lW <- length(unlist(nnW))
  
  # Iterative training loop over the specified number of steps.
  for (i in 1:nstep){
    minibatch <- sample(nrow(inp),mb) # Randomly select indices to form a minibatch.
    derb <- rep(0,lb) # initialize gradient vectors 
    derW <- rep(0,lW)
    
    # Iterate for each sample in the minibatch.
    for (j in 1:mb){
      prediction <- forward(nn,inp[minibatch,][j,]) # forward prediction
      derivative <- backward(prediction,k[minibatch][j]) # backward training (updating)
      # Aggregate gradients of biases and weights.
      derb <- derb+unlist(derivative$db) 
      derW <- derW+unlist(derivative$dW)
    }
    
    # Update biases and weights based on aggregated gradients.
    nn$b=relist(unlist(nn$b)-eta*derb/mb,skeleton=nnb)
    nn$W=relist(unlist(nn$W)-eta*derW/mb,skeleton=nnW)
  }
  return(list(h=nn$h,b=nn$b,W=nn$W))
} 



# Part 5
# In this part, we use the functions defined above to train a 4-8-7-3 network for 
# classifying irises species based on four characteristics from the 'iris' dataset.
# The dataset is divided into training and test data; the test data consist of
# every 5th row starting from the 5th row. The remainder is used for training.

d <- c(4,8,7,3) # Define the structure of the neural network
nn <- netup(d) # Initialize neural network
data(iris) # load data
indices <- seq(from = 5, to = nrow(iris), by = 5) # indices for test data
k <- as.numeric(as.factor(iris$Species)) # Convert species names into numeric factors.

# Divide variables and labels into training set and test set
k_train <- k[-indices]
k_test <- k[indices]
inp_train <- as.matrix(iris[-indices, 1:4])
inp_test <- as.matrix(iris[indices, 1:4])

# Train the neural network using the training data.
predict <- train(nn,inp_train,k_train,eta=.01,mb=10,nstep=10000)



# Part 6
# In this part, the test data are used to make predictions using the model obtained from
# Part 5. The misclassification rate and decrease of loss is then calculated to evaluate
# the neural network's performance. 

# which class is classified as
index <- apply(inp_test,1,function(x) which.max(forward(nn=predict,x)$h[[4]]))
difference_rate <- sum(index != k_test)/nrow(inp_test) # misclassification rate


# loss decreased in test set
pa_test <- apply(inp_test,1,function(x) exp(forward(nn=predict,x)$h[[4]])/sum(exp(forward(nn=predict,x)$h[[4]])))
# loss in test set after training
la_test <- -sum(pa_test[cbind(k_test,1:length(k_test))])/length(k_test) 
pb_test <- apply(inp_test,1,function(x) exp(forward(nn=nn,x)$h[[4]])/sum(exp(forward(nn=nn,x)$h[[4]])))
# loss in test set before training
lb_test <- -sum(pb_test[cbind(k_test,1:length(k_test))])/length(k_test) 
diff_test <- la_test - lb_test # loss decreasing in test set after training

# loss decreased in train set        
pa_train <- apply(inp_train,1,function(x) exp(forward(nn=predict,x)$h[[4]])/sum(exp(forward(nn=predict,x)$h[[4]])))
# loss in train set after training
la_train <- -sum(pa_train[cbind(k_train,1:length(k_train))])/length(k_train)
pb_train <- apply(inp_train,1,function(x) exp(forward(nn=nn,x)$h[[4]])/sum(exp(forward(nn=nn,x)$h[[4]])))
# loss in test set before training                  
lb_train <- -sum(pb_train[cbind(k_train,1:length(k_train))])/length(k_train)
diff_train <- la_train - lb_train # loss decreasing in train set after training

# From the result, it can be seen that classification precision in test set is high,
# indicating high generalization performance of the model constructed. Also, the dramatic
# decrease in loss function also shows satisfied performance of model.
