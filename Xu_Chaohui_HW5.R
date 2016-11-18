#Overall Comment: Good exercise except for some details. And please do pay attentions to the setting of any dataframe to avoid some errors.


library('ggplot2') #install ggplot 2 package

#Question 1 Print to the console all methods and attributes associates with a dataframe.	
#Write code to determine the number of columns in a dataframe
methods(class=data.frame) #print to the console all methods associated with a dataframe

attributes(diamonds) #print to the console all attributes associated with a dataframe

ncol(diamonds) #number of columns in the data frame

#Question 2 Write code to determine how	many rows are in a dataframe
nrow(diamonds) #number of rows in the data frame

#Question 3 Write code to extract the column names from	a dataframe	and print the	
#names of the columns (one per line) to	the	console.
cat(colnames(diamonds), sep = '\n') #colnames() extracts the column names and cat() prints the
#name of the columns (one per line)

#Question 4 Write code to determine	the	type of	each column (numeric, factor, logical,	
# etc.). Print the type of each	column to the console
type = sapply(diamonds, class) #returns the type of each column

#Question 5 Write code that will loop through any dataframe and calculate the mean of	
#every numeric column. Label the output with the name of the column.
sapply(diamonds,mean)[which(type=="numeric")] #loop	through	the	dataframe and calculate	the	mean of	
#every numeric column

#Comment:It doesn't work with me and I have following output
#> sapply(diamonds,mean)[which(type=="numeric")]
#Error in which(type == "numeric") : object 'type' not found
#In addition: Warning messages:
#1: In mean.default(X[[i]], ...) :
#  argument is not numeric or logical: returning NA
#2: In mean.default(X[[i]], ...) :
#  argument is not numeric or logical: returning NA
#3: In mean.default(X[[i]], ...) :
#  argument is not numeric or logical: returning NA
#I suggest to select the numeric first or you can write one loop to solve the numeric column selection.

#Question 6 Write code that will loop through any dataframe	and create a frequency table	
#for every factor column. Label the	output with	the	name of the	column.
#Solution 1:
Factor = diamonds[,seq(2,4,by = 1)] #extract the factor columns
summary(Factor) #create a frequency table for every factor column
#Solution 2:
summary(Filter(is.factor,diamonds)) #loop through the dataframe and create a frequency table
                                    #for the factor column

#Comment: Actually the fist solution doesn't work since it must can be extend to any dataframe. Solution one is only suitable for diamonds data.

#Question 7 
#a) Write code that will loop through any dataframe	and	determine the number	
#of rows containing NA (missing value) in each column.
lapply(as.data.frame(lapply(diamonds,is.na)),sum) #loop	through	any dataframe and determine the	number	
#of rows containing NA (missing value) in each column
#b) Determine the percentage of rows containing	an NA in any of the columns.
nrow(diamonds[rowSums(is.na(diamonds))>0,])/nrow(diamonds) #determine the persentage of rows containing an NA

#Question 8
Pearson_col <- function(mydata) {
  #This function accepts a data frame as a parameter and returns a dataframe	
  #that contains each pair of column names in the first	column
  #in a single string separated	by a "-", and their corresponding	
  #Pearson correlation coefficient in the second column.
  # @parameter: a data frame
  # @return the paired names and their correlation coefficient
  type <- sapply(mydata,class) #get the type of all columns
  mydata <- mydata[which(type == "numeric")] #extract the numeric columns
  col_name <- colnames(mydata) #extract the column names
  pair_names <- c() #create a new empty vector for later storage of paired column names
  pair_cor <- c() #create a new empty vector for correlation coefficient column storage
  for (i in 1:(length(col_name) - 1)) { #looking into every column, starting with 1st column
    for (j in (i+1):length(col_name)) {  #each column after ith column to make sure no repetition
      corr <- cor(mydata[i],mydata[j],method = "pearson") #use cor() to compute the correlation
      pair_names <- c(pair_names,paste(col_name[i],col_name[j], sep = "-")) 
      pair_cor <- c(pair_cor, corr)
    }
  }

return(data.frame(pair_names, pair_cor)) #make the new vector a new data frame
}

#Example:
Pearson_col(diamonds)
  
  
      
      
      

  







