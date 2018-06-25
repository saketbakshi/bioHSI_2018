data(mtcars)
typeof(mtcars)
# 1. "mtcars" is a list. I used "typeof()" to test
data(trees)
data(precip)
precip
typeof(precip)
# 2. "precip" is a 1-dimensional array. Again, I found out using the "typeof()" function after seeing "precip", which is a list. Since the list was defined as "double," it's a 1-dimensional array.
trees.data=as(trees, "matrix")
# 3. use trees.data=as(trees, "matrix") to convert "trees" into a matrix
trees.data
precip[14]
# 4. Atlanta
# 5. I would use "data.frame()"
# 6. Yes. I typed "precip" and hit enter to see the data. Since precip is an array, it has numerical values.
# 7. "mtcars[2,7]", "mtcars["Mazda RX4 Wag", 7]", mtcars["Mazda RX4 Wag", "qsec"], and "mtcars[2, "qsec"]"
# 8. I would use "precip["Juneau"]<-23", "precip["Phoenix"]<-46", and "precip["Sacramento"]<-12".
trees["Girth"]>trees["Volume"]
# 9. There are no trees with more girth than volume. I used the code above to see if any values came up TRUE on whether a tree's girth was greater than its width.
A<-sum(trees["Height"])
B<-sum(mtcars["Valiant",])
C<-sum(precip[1:8])
(C/B)+A
# 10. 2356.628