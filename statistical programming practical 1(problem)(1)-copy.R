
#separating marks

mark <- c(",", ".", ";", "!", ":", "?") 
##S create a vector which contains all the punctuation marks
pattern <- "[.,;!:?]" 
##S this pattern will match the specific punctuation marks whenever they occur in the text

# step 4

split_punct=function(x){
  x=gsub("_","",x,fixed=TRUE)
  x=gsub("-","",x,fixed=TRUE)
  x=gsub("(","",x,fixed=TRUE)
  x=gsub(")","",x,fixed=TRUE)
  x=gsub("*","",x,fixed=TRUE) 
  ##S "gsub" function removes all the unnecessary punctuation marks, every
  ##S punctuation mark in the first argument will be replaced by "" in the second
  ##S argument, which means to remove it. "fixed=TRUE" will treat the marks as fixed strings

# step 6

b1 <- unique(tolower(a_processed)) 
##S "tolower" function converts characters in a_processed to lowercase form, from which "unique" function extracts every unique word
index_vector <- match(tolower(a_processed),b1)
##S match function is used to find the position of every element in "tolower(a_processed)" within "b1" 
sorted_index <- order(tabulate(index_vector),decreasing = TRUE)
##S "tabulate" function will count up the frequency of occurrence of every index
##S "order" function can sort the outcome in descending order by setting "decreasing = TRUE"
sorted_tabulate <- tabulate(index_vector)[sorted_index]
thresholdfunc <- function(x,y){
##S x represents the tabulate outcome we want to handle, y represents the target number of common words
  threshold <- 0
  indices <- integer(0)
    for (i in 1:length(x)) {
    indices <- c(indices,i)
    if (length(indices) == y) {
      threshold <- x[i]
      break
    }
  }
##S for each iteration we add one index into a vector until its length equals to the target number.
##S meanwhile it will give the index of breaking point, which is the threshold 
  return(threshold)
}
threshold1<-thresholdfunc(sorted_tabulate,1000)
mword<-match(threshold1-1, sorted_tabulate)
mword <- mword-1
##S these 2 steps above is to find the place of the last occurrence of threshold number 
b<-b1[sorted_index[1:mword]]
##S b gives the 1015 most common words, which is nearly 1000

# step 7

matching_indices <- match(tolower(a_processed),b)
t1 <- matching_indices[1:(length(matching_indices)-2)]
t2 <- matching_indices[2:(length(matching_indices)-1)]
t3 <- matching_indices[3:(length(matching_indices))]
##S Given that the 2nd and 3rd columns are created by shifting the matching indices
##S by one place and two places, the length of three columns are length(matching_indices),
##S length(matching_indices)-1, length(matching_indices)-2, respectively. To form the matrix1
##S we need to make the number of rows equal to length(matching_indices)-2
matrix1 <- cbind(t1,t2,t3)
matrixT <- matrix1[!rowSums(is.na(matrix1)), ]
##S function "is.na" is used to detected NA in matrix1 by giving TRUE or FALSE, 
##S "rowSums" function counts up the number of NA in each row, "!" is logical negation 
##S which can remove rows containing NA finally. " " in the last keeps columns unchanged 
p1 <- matching_indices[1:(length(matching_indices)-1)]
p2 <- matching_indices[2:(length(matching_indices))]
matrix2 <- cbind(p1,p2)
matrixP <- matrix2[!rowSums(is.na(matrix2)), ]
##S the same method applied into forming matrixP

#step 9

freq_b <- sorted_tabulate[1:mword]
prob_b <- freq_b/sum(tabulate(index_vector))
text_comparison <- sample(b,50,prob=prob_b)
##S "sample" function independently selects 50 characters from b 
##S with probability vector specified by frequencies

