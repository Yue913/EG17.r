## Members in group 17: Linfan Shi；Xinyue Huang；Yue Yu.
## Description of contribution: 

setwd("C:/Users/hp/Downloads") ## comment out of submitted
a <- scan("gutenberg.org_files_4300_4300-01.txt",what="character",skip=73,nlines=32858-73,encoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("



# step 4

#choose marks to be separated
custom_pattern <- "[.,;!:?]"

#Define "split_punct" function
split_punct <- function(x){
  
  # remove "_" , "-" , "(" , ")" , "*" from each word
  x <- gsub("_","",x,fixed=TRUE)
  x <- gsub("-","",x,fixed=TRUE)
  x <- gsub("(","",x,fixed=TRUE)
  x <- gsub(")","",x,fixed=TRUE)
  x <- gsub("*","",x,fixed=TRUE)
  
  # in each iteration, split the last punctuation mark (if exists) each words
  # find the number of iterations needed to completely split punctuation marks
  max_iteration <- 0 #initialize variable max_iteration, determine how many iterations needed to perform in next step
  t_x <- x # set auxiliary variable, in order to make input x unaffected by the procedures
  repeat{
    punc <- grep(custom_pattern,substr(t_x,nchar(t_x),nchar(t_x))) #which word have a punctuation mark at the last position
    total <- length(punc) #update variable total, which determines whether maximum iteration has been attained
    t_x[punc] <- substr(t_x[punc],1,nchar(t_x[punc])-1) #remove the last punctuation mark (if exists) from every word
    max_iteration <- max_iteration+1 #update variable max_iteration
    if (total==0){break()}
  }
  max_iteration <- max_iteration-2 #total iterations needed to completely split punctuation marks
  
  
  split_location <- grep(custom_pattern,substr(x,nchar(x),nchar(x))) #at which position (of the present vector) should a split being made
  transform_x <- x #set auxiliary variable, in order to make input x unaffected by the procedures
  for (j in 1:max_iteration){
    string <- rep(0,length(transform_x)+length(split_location)) #create a vector that is needed to be filled later to obtain vector after one iteration of split
    split_combine_location <- split_location+(1:length(split_location)) #the position punctuation marks should occupy
    string[split_combine_location] <- substr(transform_x[split_location],nchar(transform_x[split_location]),nchar(transform_x[split_location])) #fill punctuation marks
    transform_x[split_location] <- substr(transform_x[split_location],1,nchar(transform_x[split_location])-1) 
    #update previous vector by: at the position that a split being made, delete the last punctuation mark
    
    
    string[-split_combine_location] <- transform_x #fill the corresponding position of new vectors by the updated old word vector
    string_char <- which(nchar(string)>1) #in the new vector, the position that contains only one character or one mark should not be further split
    split_location <- grep(custom_pattern,substr(string,nchar(string),nchar(string))) 
    #in the new vector, the position that do not have a punctuation mark in the end should not be further split
    split_location <- intersect(split_location,string_char) #only the position satisfying the above two condition should be split
    transform_x <- string #update the vector that should be further split
  }
  return(string)
}


# step 8

#T and P are the results from step 7, b is the result from step 6
index <- sample(1:nrow(matrixT),1)
##Y pick a random row from the matrixT
fra_T <- data.frame(matrixT)
fra_P <- data.frame(matrixP)
##Y transfer the matrixT amd matrixP to data.frame
colnames(matrixT)=NULL
colnames(matrixP)=NULL
##Y delete the column names
i <- matrixT[index,1] 
##Y i is the element in the first column of the random row 
j <- matrixT[index,2] 
##Y j is the element in the second column of the random row
freq_b <- tabulate(index_vector)[sorted_index[1:mword]]
##Y calculate how many times that each common word appears in the "a_processed"
rep_b <- rep(1:mword,freq_b)
##Y express each word as a number and repeat the number of times it appears, 
##Y so that when randomly selected in "rep-b"
##Y the probability of each word being extracted is the frequency of the word
kab <- c(i,j) 
##Y set the final 50-words simulation named "kab", which contains updated i and j.
for(iteration in 1:48){
  ##Y at start we have two words, so repeat 48 times
  if (length(which(fra_T$t1==i & fra_T$t2==j)>0)){
    ##Y Firstly, pick next word from matrixT,
    ##Y if length>0, it means that the sub-matrix has the rows which contain i and j
    rows <- sample(which(fra_T$t1==i & fra_T$t2==j),1)
    ##Y Extract the rows number which contains the i in column 1 and j in column 2
    ##Y pick one row randomly by using "sample"
    i <- j
    ##Y update i <- j
    j <- matrixT[rows,3] 
    ##Y update j,which is the element in column 3 of picked row in matrixT. 
    kab <- c(kab,j) 
    ##Y update the “kab”
  }
  else{if (length(which(fra_P$p1==j))>0){ 
    ##Y If we cannot find the corresponding rows in matrixT
    ##Y we simulate the next word using matrixP. 
    rows <- sample(which(fra_P$p1==j),1) 
    ##Y extract the rows number which contains the j in column 1 
    ##Y pick one row randomly by using "sample"
    i <- j 
    ##Y update i <- j
    j <- matrixP[rows,2] 
    ##Y update j,which is the element in column 2 of picked row in matrixP. 
    kab <- c(kab,j) 
    ##Y update the kab
  }
    else{ 
      ##Y If we cannot find the corresponding rows in matrixP
      ##Y we simulate the next word using the common word frequencies
      i <- j
      ##Y update i <- j
      j <- sample(rep_b,1) 
      ##Y update j,which is the element in b,selected by the common word frequencies.
      kab <- c(kab,j) 
      ##Y update the kab.
    }}
}

cat <- b[kab] 
##Y "kab" is the word index corresponding to common words, b[kab] is the final 50-words section

#step10
Capital_letter <- "A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z" #for the purpose of identifying capital letter in words
word_capital <- grep(Capital_letter,substr(a_processed,1,1)) #identify which word has a capital letter at its beginning
id_capital <- rep(0,length(a_processed)) #create auxiliary variable, for further purpose of identifying which word has a capital letter at its beginning
id_capital[word_capital] <- 1 # if a pisition of this vector equals to 1, then there is a capital beginning in this word
frame <- data.frame(table(matching_indices,id_capital)) 
# create a table that summarize the frequency of each word in the most common word list
# and these frequencies are further divided into the case of first letter being capital or not being capital
frame <- cbind(frame[1:mword,],frame[mword+1:2*mword,]) #reshape the table so that frequencies of capital and non-capital are presented in two columns
frame <- frame[c(3,6)] #only save columns of frequency
colnames(frame) <- c("non_cap","cap") #rename columns
cap_prob <- frame$cap/(frame$non_cap+frame$cap) #calculate the probability that each words in most common list have capital letter in the first position
b_cap <- which(cap_prob>=0.5) #determine the first letter of which word should be transformed to capital one
B_cap <- b #create auxiliary variable, in order to make a modified version of b while not affecting b
substr(B_cap[b_cap],1,1) <- toupper(substr(B_cap[b_cap],1,1)) #modify b so that the first letters of selected words are transformed into capital ones
cat_after_cap <- B_cap[kab] #update generating sentence in step 8
