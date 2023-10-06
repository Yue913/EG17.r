## Members in group 17: Linfan Shi:s2508485; Xinyue Huang:s2504007; Yue Yu:s2496178.

## Description of contribution: 

## Everyone participated in the discussion, constructed the logic flow of code together. 

## Linfan Shi worked on code and comments in step6, step7 and step9, and also respondible for removing unnecessary punctuation marks in step4 
## before setting split_punct function.

## Xinyue Huang worked on code and comments in remaining part of step4 and step10.

## Yue Yu worked on code and comments in step8, simulated the 50-words sections.

## Each member contributes roughly equal (33%) to this project.


setwd(" ") ## fill working dictionary between the quotient marks
a <- scan("gutenberg.org_files_4300_4300-01.txt",what="character",skip=73,nlines=32858-73,encoding="UTF-8")
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("



## step 4

## choose marks to be separated
custom_pattern <- "[.,;!:?]"

## define "split_punct" function
split_punct <- function(x){
  
  x <- gsub("_","",x,fixed=TRUE)
  x <- gsub("-","",x,fixed=TRUE)
  x <- gsub("(","",x,fixed=TRUE)
  x <- gsub(")","",x,fixed=TRUE)
  x <- gsub("*","",x,fixed=TRUE)
  ## "gsub" function removes all the unnecessary punctuation marks, every
  ## punctuation mark in the first argument will be replaced by "" in the second
  ## argument, which means to remove it. "fixed=TRUE" will treat the marks as fixed strings

  
  ## in each iteration, split the last punctuation mark (if exists) each words
  ## find the number of iterations needed to completely split punctuation marks
  
  max_iteration <- 0 #initialize variable max_iteration, determine how many iterations needed to perform in next step
  t_x <- x ## set auxiliary variable, in order to make input x unaffected by the procedures
  repeat{
    punc <- grep(custom_pattern,substr(t_x,nchar(t_x),nchar(t_x))) ## which word have a punctuation mark at the last position
    total <- length(punc) ## update variable total, which determines whether maximum iteration has been attained
    t_x[punc] <- substr(t_x[punc],1,nchar(t_x[punc])-1) ## remove the last punctuation mark (if exists) from every word
    max_iteration <- max_iteration+1 ## update variable max_iteration
    if (total == 0){break()}
  }
  max_iteration <- max_iteration-2 ## total iterations needed to completely split punctuation marks
  
  
  split_location <- grep(custom_pattern,substr(x,nchar(x),nchar(x))) ## at which position (of the present vector) should a split being made
  transform_x <- x ## set auxiliary variable, in order to make input x unaffected by the procedures
  for (j in 1:max_iteration){
    string <- rep(0,length(transform_x)+length(split_location)) ## create a vector that is needed to be filled later to obtain vector after one iteration of split
    split_combine_location <- split_location+(1:length(split_location)) ## the position punctuation marks should occupy
    string[split_combine_location] <- substr(transform_x[split_location],nchar(transform_x[split_location]),nchar(transform_x[split_location])) ## fill in punctuation marks
    transform_x[split_location] <- substr(transform_x[split_location],1,nchar(transform_x[split_location])-1) 
    ## update previous vector by: at the position that a split being made, delete the last punctuation mark
    
    
    string[-split_combine_location] <- transform_x ## fill the corresponding position of new vectors by the updated old word vector
    string_char <- which(nchar(string)>1) ## in the new vector, the position that contains only one character or one mark should not be further split
    split_location <- grep(custom_pattern,substr(string,nchar(string),nchar(string))) 
    ## in the new vector, the position that do not have a punctuation mark in the end should not be further split
    split_location <- intersect(split_location,string_char) ## only the position satisfying the above two condition should be split
    transform_x <- string ## update the vector to the version after splitting so that ot can be further split
  }
  return(string)
}


## step 5

## split string vector "a" as required
a_processed <- split_punct(a)


## step 6

b1 <- unique(tolower(a_processed)) ## "tolower" function converts characters in a_processed to lowercase form, from which "unique" function extracts every unique word
index_vector <- match(tolower(a_processed),b1) ## match function is used to find the position of every element in "tolower(a_processed)" within "b1" 
sorted_index <- order(tabulate(index_vector),decreasing = TRUE)
## "tabulate" function will count up the frequency of occurrence of every index
## "order" function can sort the outcome in descending order by setting "decreasing = TRUE"
sorted_tabulate <- tabulate(index_vector)[sorted_index]
thresholdfunc <- function(x,y){
  ## x represents the tabulate outcome we want to handle, y represents the target number of common words
  threshold <- 0
  indices <- integer(0)
  for (i in 1:length(x)) {
    indices <- c(indices,i)
    if (length(indices) == y) {
      threshold <- x[i]
      break
    }
  }
  ## for each iteration we add one index into a vector until its length equals to the target number.
  ## meanwhile it will give the index of breaking point, which is the threshold 
  return(threshold)
}
threshold1 <- thresholdfunc(sorted_tabulate,1000)
mword <- match(threshold1-1, sorted_tabulate)
mword <- mword-1
## these 2 steps above is to find the place of the last occurrence of threshold number 
b <- b1[sorted_index[1:mword]] ## b gives the 1015 most common words, which is nearly 1000


# step 7

matching_indices <- match(tolower(a_processed),b)
t1 <- matching_indices[1:(length(matching_indices)-2)]
t2 <- matching_indices[2:(length(matching_indices)-1)]
t3 <- matching_indices[3:(length(matching_indices))]
## Given that the 2nd and 3rd columns are created by shifting the matching indices by one place and two places, the length of three columns are length(matching_indices),
## length(matching_indices)-1, length(matching_indices)-2, respectively. To form the matrix1 we need to make the number of rows equal to length(matching_indices)-2
matrix1 <- cbind(t1,t2,t3)
T <- matrix1[!rowSums(is.na(matrix1)), ]
## function "is.na" is used to detected NA in matrix1 by giving TRUE or FALSE, 
## "rowSums" function counts up the number of NA in each row, "!" is logical negation, which can remove rows containing NA finally. " " in the last keeps columns unchanged 
p1 <- matching_indices[1:(length(matching_indices)-1)]
p2 <- matching_indices[2:(length(matching_indices))]
matrix2 <- cbind(p1,p2)
P <- matrix2[!rowSums(is.na(matrix2)), ] ## the same method applied into forming P


## step 8

index <- sample(1:nrow(T),1) ## pick a random row from the matrix T
fra_T <- data.frame(T)
fra_P <- data.frame(P) ## transfer the matrix T amd matrix P to data.frame
colnames(T)=NULL
colnames(P)=NULL ## delete the column names
i <- T[index,1] ## i is the element in the first column of the random row 
j <- T[index,2] ## j is the element in the second column of the random row
freq_b <- tabulate(index_vector)[sorted_index[1:mword]] ## calculate how many times that each common word appears in the "a_processed"
rep_b <- rep(1:mword,freq_b) 
## express each word as a number and repeat the number of times it appears, 
## so that when randomly selected in "rep-b"
## the probability of each word being extracted is the frequency of the word
kab <- c(i,j)  ## set the index of final 50-words simulation named "kab", which contains updated i and j.
for(iteration in 1:48){
  ## at start we have two words, so repeat 48 times
  if (length(which(fra_T$t1==i & fra_T$t2==j)>0)){
    ## Firstly, pick next word from matrix T,
    ## if length>0, it means that the sub-matrix has the rows which contain i and j
    rows <- sample(which(fra_T$t1==i & fra_T$t2==j),1)
    ## Extract the rows number which contains the i in column 1 and j in column 2
    ## pick one row randomly by using "sample"
    i <- j ## update i <- j
    j <- T[rows,3] ## update j,which is the element in column 3 of picked row in matrix T. 
    kab <- c(kab,j) ## update the index vector "kab"
  }
  else{if (length(which(fra_P$p1==j))>0){ 
    ## If we cannot find the corresponding rows in matrix T, we simulate the next word using matrix P. 
    rows <- sample(which(fra_P$p1==j),1) 
    ## extract the rows number which contains the j in column 1, pick one row randomly by using "sample"
    i <- j ## update i <- j
    j <- P[rows,2] ## update j,which is the element in column 2 of picked row in matrix P. 
    kab <- c(kab,j) ## update the index vector "kab"
  }
    else{ 
      ## If we cannot find the corresponding rows in matrix P, we simulate the next word using the common word frequencies
      i <- j ## update i <- j
      j <- sample(rep_b,1) ## update j,which is the element in b,selected by the common word frequencies.
      kab <- c(kab,j) ## update the index vector "kab"
    }}
}

cat <- b[kab] ## "kab" is the word index corresponding to common words, b[kab] gives the final 50-words section.
print(cat) ## shows the final 50-words section.


## step 9

freq_b <- sorted_tabulate[1:mword]
prob_b <- freq_b/sum(tabulate(index_vector))
text_comparison <- sample(b,50,prob=prob_b) ## "sample" function independently selects 50 characters from b with probability vector specified by frequencies
print(text_comparison) ## shows the final 50-words section for comparison.


## step10

Capital_letter <- "A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z" ## for the purpose of identifying capital letter in words
word_capital <- grep(Capital_letter,substr(a_processed,1,1)) ## identify which word has a capital letter at its beginning
id_capital <- rep(0,length(a_processed)) ## create auxiliary variable, for further purpose of identifying which word has a capital letter at its beginning
id_capital[word_capital] <- 1 ## if a pisition of this vector equals to 1, then there is a capital beginning in this word
frame <- data.frame(table(matching_indices,id_capital)) 
## create a table that summarize the frequency of each word in the most common word list,
## and these frequencies are further divided into the case of first letter being capital or not being capital
frame <- cbind(frame[1:mword,],frame[(mword+1):(2*mword),]) ## reshape the table so that frequencies of capital and non-capital are presented in two columns
frame <- frame[c(3,6)] #only save columns of frequency
colnames(frame) <- c("non_cap","cap") ## rename columns
cap_prob <- frame$cap/(frame$non_cap+frame$cap) ## calculate the probability that each words in most common list have capital letter in the first position
b_cap <- which(cap_prob>=0.5) ## determine the first letter of which word should be transformed to capital one
B_cap <- b ## create auxiliary variable, in order to make a modified version of b while not affecting b
substr(B_cap[b_cap],1,1) <- toupper(substr(B_cap[b_cap],1,1)) ## modify b so that the first letters of selected words are transformed into capital ones
cat_after_cap <- B_cap[kab] ## update generating sentence in step 8
print(cat_after_cap) ## shows the final 50 words section after updating.
