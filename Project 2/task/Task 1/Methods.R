### The Methods for the list.

# List Convenience Methods.
Lst <- list(name="Fred", wife="Mary", no.children=3,child.ages=c(4,7,9,1))

# Listing the contents of the column.
Lst[[1]]

# Length
length(Lst)

# Coefficients of the components.
# There is a difference between the double brackets and single set of brackets in the list.

# Listing the components.
Lst[[1]]

# Subscript of the list.
Lst[1]

# Typeof
typeof(Lst[[4]]) # Shows the overall type of the combined values.


# Iteration over the values.
for(i in Lst[[4]]){
  print(i)
}

# Creating a vector with length =0.
x <- vector(mode="character",length=0)
x <- c(x,"Abhi")

# Creating a dummy bar plot

barplot(c(5455,2332),ylim=8000)


x <- c(1,3,4)
length(x)

