---
title: "Analysis on baby names over 130 years data"
author: "Rajesh Kumar"
date: "Saturday, May 07, 2016"
output: html_document
---

```
#read the baby names from year 1950
data = read.csv('yob1950.txt', header = FALSE)
```


```
#see few lines of dataset sorted on number
head(data)
```


<table>
<thead><tr><th></th><th scope=col>V1</th><th scope=col>V2</th><th scope=col>V3</th></tr></thead>
<tbody>
	<tr><th scope=row>1</th><td>Linda</td><td>F</td><td>80412</td></tr>
	<tr><th scope=row>2</th><td>Mary</td><td>F</td><td>65443</td></tr>
	<tr><th scope=row>3</th><td>Patricia</td><td>F</td><td>47920</td></tr>
	<tr><th scope=row>4</th><td>Barbara</td><td>F</td><td>41560</td></tr>
	<tr><th scope=row>5</th><td>Susan</td><td>F</td><td>38019</td></tr>
	<tr><th scope=row>6</th><td>Nancy</td><td>F</td><td>29618</td></tr>
</tbody>
</table>




```
#convert column names to meaningful names
colnames(data) <- c("name", "sex", "number")
```


```
#find how many children were born in 1950
total_child_born_in_1950 = sum(data$number)
total_child_born_in_1950
```


3503298



```
#get only females from data
female_data = subset(data, sex=='F')
```


```
#get top10 most popular female names
top10_popular_female = female_data$name[1:10]
top10_popular_female
```


<ol class=list-inline>
	<li>Linda</li>
	<li>Mary</li>
	<li>Patricia</li>
	<li>Barbara</li>
	<li>Susan</li>
	<li>Nancy</li>
	<li>Deborah</li>
	<li>Sandra</li>
	<li>Carol</li>
	<li>Kathleen</li>
</ol>




```
#get top10 most popular male names
top10_popular_male = subset(data, sex=='M')$name[1:10]
top10_popular_male
```


<ol class=list-inline>
	<li>James</li>
	<li>Robert</li>
	<li>John</li>
	<li>Michael</li>
	<li>David</li>
	<li>William</li>
	<li>Richard</li>
	<li>Thomas</li>
	<li>Charles</li>
	<li>Gary</li>
</ol>




```
#find if there are any name which appears only ones
onetime_appear_names = subset(data, number=='1')$name
onetime_appear_names
```


<ol class=list-inline>
</ol>




```
#find the name of biggest rise and fall male from 1950 to 2010
#copy the 'data' to another variable 'd1950'
d1950 = data
```


```
#update the column lables
colnames(d1950) = c('name', 'sex', 'number_1950')
```


```
#reading file 'yob2010.txt' and giving lables to the columns
d2010 = read.csv('yob2010.txt', header = FALSE, col.names=c('name', 'sex', 'number_2010'))
```


```
#merge two list into single list 'mdata'
mdata = merge(d1950,d2010)
```


```
#adding new 'diff' column which is the difference of numbers in 2010 to 1950
mdata$diff = mdata$number_2010 - mdata$number_1950
```


```
#arrange 'mdata' list in order according to 'diff' column value.
mdata = mdata[order(mdata$diff),]
```


```
#in the below line firstly we are getting subset of all the male childs 
#and then taking min value from the 'diff' column. After that get the 
#row value from the 'mdata' whose diff column value is same as min value.
#this will give the 'Male Name' who had the biggest fall from 1950 to 2010.
biggest_fall_mName = subset(mdata, mdata$diff == min(subset(mdata, mdata$sex=='M')['diff']))[1,]
biggest_fall_mName
```


<table>
<thead><tr><th></th><th scope=col>name</th><th scope=col>sex</th><th scope=col>number_1950</th><th scope=col>number_2010</th><th scope=col>diff</th></tr></thead>
<tbody>
	<tr><th scope=row>4228</th><td>Robert</td><td>M</td><td>83534</td><td>7510</td><td>-76024</td></tr>
</tbody>
</table>




```
#in the same way the next line will give the 'Male Name' who had the biggest rise from 1950 to 2010.
biggest_rise_mName = subset(mdata, mdata$diff == max(subset(mdata, mdata$sex=='M')['diff']))[1,]
biggest_rise_mName
```


<table>
<thead><tr><th></th><th scope=col>name</th><th scope=col>sex</th><th scope=col>number_1950</th><th scope=col>number_2010</th><th scope=col>diff</th></tr></thead>
<tbody>
	<tr><th scope=row>2278</th><td>Jacob</td><td>M</td><td>465</td><td>22011</td><td>21546</td></tr>
</tbody>
</table>




```
#similarly we have done for female the next line will give the 'Female Name' who had the 
#biggest fall from 1950 to 2010.
biggest_fall_fName = subset(mdata, mdata$diff == min(subset(mdata, mdata$sex=='F')['diff']))[1,]
biggest_fall_fName
```


<table>
<thead><tr><th></th><th scope=col>name</th><th scope=col>sex</th><th scope=col>number_1950</th><th scope=col>number_2010</th><th scope=col>diff</th></tr></thead>
<tbody>
	<tr><th scope=row>3067</th><td>Linda</td><td>F</td><td>80412</td><td>471</td><td>-79941</td></tr>
</tbody>
</table>




```
#the next line will give the 'Female Name' who had the biggest rise  
#from 1950 to 2010.
biggest_rise_fName = subset(mdata, mdata$diff == max(subset(mdata, mdata$sex=='F')['diff']))[1,]
biggest_rise_fName
```


<table>
<thead><tr><th></th><th scope=col>name</th><th scope=col>sex</th><th scope=col>number_1950</th><th scope=col>number_2010</th><th scope=col>diff</th></tr></thead>
<tbody>
	<tr><th scope=row>2216</th><td>Isabella</td><td>F</td><td>55</td><td>22822</td><td>22767</td></tr>
</tbody>
</table>


