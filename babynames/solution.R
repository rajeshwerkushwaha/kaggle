###########################################################################

# TASK 1

###########################################################################



#QUESTION 1

#==========

#read.csv function is used to read the names file of year 1950

data = read.csv('yob1950.txt', header = FALSE)



#QUESTION 2.a

#2(a). According to these data, how many children were born in 1950?

#============

#colnames function is used to add the column names

colnames(data) <- c("name", "sex", "number")

#sum function is used to add all elements of the given object

total_child_born_in_1950 = sum(data$number)

print ("Total child born in 1950 :")

print (total_child_born_in_1950)

print ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")



#QUESTION 2.b

#2(b). Which were the 10 most popular names for each sex?

#============

#subset(data, sex=='F') will give the subset of female data

female_data = subset(data, sex=='F')

#female_data$name[1:10] will return the first 10 most popular names from the female_data

top10_popular_female = female_data$name[1:10]

print ("10 Most popular female names in 1950: ")

print (top10_popular_female)

#the same calculation we can do in a single line for male

top10_popular_male = subset(data, sex=='M')$name[1:10]

print ("10 Most popular male names in 1950: ")

print (top10_popular_male)

print ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")



#QUESTION 2.c

#2(c). Are there any names in the data set that were only given once?

#============

#subset(data, number=='1')$name will return all the names who appear only once

onetime_appear_names = subset(data, number=='1')$name

print ("One time appear names are : ")

print (onetime_appear_names)

print ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")



#QUESTION 3

#==========

#copy the 'data' to another variable 'd1950'

d1950 = data

#update the column lables

colnames(d1950) = c('name', 'sex', 'number_1950')

#reading file 'yob2010.txt' and giving lables to the columns

d2010 = read.csv('yob2010.txt', header = FALSE, col.names=c('name', 'sex', 'number_2010'))

#merge two list into single list 'mdata'

mdata = merge(d1950,d2010)

#adding new 'diff' column which is the difference of numbers in 2010 to 1950

mdata$diff = mdata$number_2010 - mdata$number_1950

#arrange 'mdata' list in order according to 'diff' column value.

mdata = mdata[order(mdata$diff),]



#in the below line firstly we are getting subset of all the male childs 

#and then taking min value from the 'diff' column. After that get the 

#row value from the 'mdata' whose diff column value is same as min value.

#this will give the 'Male Name' who had the biggest fall from 1950 to 2010.

biggest_fall_mName = subset(mdata, mdata$diff == min(subset(mdata, mdata$sex=='M')['diff']))[1,]

print ("Biggest fall male name from 1950 to 2010: ")

print (biggest_fall_mName)



#in the same way the next line will give the 'Male Name' who had the 

#biggest rise from 1950 to 2010.

biggest_rise_mName = subset(mdata, mdata$diff == max(subset(mdata, mdata$sex=='M')['diff']))[1,]

print ("Biggest rise male name from 1950 to 2010: ")

print (biggest_rise_mName)



#similarly we have done for female the next line will give the 'Female Name' who had the 

#biggest fall from 1950 to 2010.

biggest_fall_fName = subset(mdata, mdata$diff == min(subset(mdata, mdata$sex=='F')['diff']))[1,]

print ("Biggest fall female name from 1950 to 2010: ")

print (biggest_fall_fName)



#the next line will give the 'Female Name' who had the biggest rise  

#from 1950 to 2010.

biggest_rise_fName = subset(mdata, mdata$diff == max(subset(mdata, mdata$sex=='F')['diff']))[1,]

print ("Biggest rise female name from 1950 to 2010: ")

print (biggest_rise_fName)

print ("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
