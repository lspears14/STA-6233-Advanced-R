#Project to analysis if there was a relationship between the locations of Dollar General store #and the income for those cities in the state of Texas
#The acs5_2.csv will need to be place in the working directory

#install libraries needed for project
#install.packages("XML")
library(rvest)
library(plyr)
library(dplyr)
library(stringr)
library(purrr)
#library(XML)
#library(xml)
#install.packages("xmls")
library(tidyverse)


#Creating empty dataset to be filled with loop
all_cities<-data.frame(City_Name=character(), Zipcode=character())
TXcity<-data.frame(City_Name=character(), Avg_Income=integer())
incomes<-data.frame(City_Name=character(), income=integer())
freqt<-data.frame(City_Name=character(), Freq=integer())


for (c in 1:2){
  theurl<-paste0("https://www.mystore411.com/store/list_state/903/Texas/Dollar-General-store-locations?page=",c,"&")
  
  #Download/Read the html from each city 
  html1<-xml2::read_html(theurl)
  
  #I use the CSS selector to figure out what table to read from the city table
  get_citystore<-html_nodes(html1, "td")
  
  #Remove parts of the string
  trim_city<-str_replace(get_citystore, "<td><a href=\"/store/list_city/903/Texas/", "")
  #spliting the list into matrix
  trim_city1<-str_split_fixed(trim_city, "\">", n = Inf)
  
  
  #Create new url to get city stores
  for(j in 1:nrow(trim_city1)){
    theurl1<-paste0("https://www.mystore411.com/store/list_city/903/Texas/",trim_city1[j,1])
    #Download/Read the html from each city 
    html1<-read_html(theurl1)
    
    
    #I use the CSS selector to figure out what table to read from the city table
    get_citystore<-html_nodes(html1, ".dotrow a")
    
    #Remove parts of the store string
    trim_city2<-str_replace(get_citystore, "<a href=\"", "")
    trim_city3<-str_split_fixed(trim_city2, "\">", n = Inf)
    
    #reset b = 1 for store location
    #b<-1
    
    #Create new url to get store zip
    for(b in 1:nrow(trim_city3)){
      
      #Need to account for locations that do not have a city
      trim_city5 <-"NA"
      
      #beginning the loop
      theurl2<-paste0("https://www.mystore411.com",trim_city3[b,1])
      
      #read url for city
      #Download/Read the html from each city 
      html2<-read_html(theurl2)
      
      #I use the CSS selector to figure out what table to read from the                              specific city data table
      get_citydata<-html_nodes(html2, ".adr span")
      
      #getzipcode
      get_zip<-get_citydata[4]
      trim_city4<-str_replace(get_zip, "<span itemprop=\"postalCode\">", "")
      trim_city5<-str_replace(trim_city4, "</span>", "")
      
      #getcity
      get_zip<-get_citydata[2]
      trim_city6<-str_replace(get_zip, "<span itemprop=\"addressLocality\">", "")
      trim_city7<-str_replace(trim_city6, "</span>", "")
      
      
      #appending to the dataframe
      newRow <-data.frame(City_name = trim_city7, Zipecode = trim_city5)
      all_cities <-rbind(all_cities, newRow)
      
    }
    
  }
}

#cleaning up envirement
rm(get_citydata, get_citystore, get_zip, trim_city1, trim_city3, trim_city, trim_city2, trim_city4, trim_city5, trim_city6, trim_city7, newRow, html1, html2, acscity)

#Bring in census data
acs5_2 <- read_csv("acs5_2.csv")

#break the location into two colums
for (a in 2:nrow(acs5_2)){
  acscity<-(str_split_fixed(acs5_2[a,2], "city", n=Inf))
  city <-(acscity[1,1])
  income<-(acs5_2[a,3])
  newRow1<-data.frame(City_Name = city, Avg_Income = income)
  TXcity<-rbind(TXcity, newRow1)
  
}

#get frequence of stores per city
freqt<-rbind(City_Name=freqt, as.data.frame(table(unlist(all_cities$City_name))))
#Reanem freqt-Var1 to CityName
colnames(freqt)[colnames(freqt)=="Var1"]<-"City_Name"

#Matching cities to income brackets
for (b in 1:nrow(freqt)){
  for (g in 1:nrow(TxCity4)){
    
    city1<-TxCity4[g,1]
    city2<-as.character(freqt[b,1])
    #icome<-as.integer(TxCity4[g,4])
    if(city2==city1){
      newRow2<-data.frame(City_Name=city1, income=TxCity4[g,4])
      incomes<-rbind(incomes,newRow2)
    }
    
  }
  
}

#Seperate TX city to city and income
listTXcity<-list(TXcity)

#seperating the state from adresses
TXCity2<-TXcity
TxCity3<-separate(TXCity2, City_Name,sep=",",into=c("City_Name", "State"))
TxCity4<-separate(TxCity3, City_Name, sep="CDP", into=c("City_Name", "CDP"))

#trim leading white space
TxCity4$City_Name<-trimws(TxCity4$City_Name)

#Clean up envirnment
rm(TxCity3, TXCity2, TXcity)

#Matching cities to income brackets
for (b in 1:nrow(freqt)){
  for (g in 1:nrow(TxCity4)){
    
    city1<-TxCity4[g,1]
    city2<-as.integer(freqt[b,1])
    #icome<-as.integer(TxCity4[g,4])
    if(city2==city1){
      inc<-data.frame(income=TxCity4[g,4])
      freqt<-cbind(freqt,inc)
    }
    
  }
  
}

#cleanup environment 
rm(a, b, c, city, city1, city2, g, j, income, newRow1, newRow2)


#Merging two dataframe income and new table
fictab<-merge(freqt, incomes, by="City_Name")

#Clean up witespace on income
fictab$income<-trimws(fictab$income)


#Assign groupnamesfor data
for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f<15000){
    fictab[i,4]<-"Poverty"
    fictab[i,5]<-15000
    fictab[i,6]<-"$0-$14,999" 
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=15000 & f<25000){
    fictab[i,4]<-"Poverty"
    fictab[i,5]<-15000
    fictab[i,6]<-"$15,000 - $24,999" 
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=25000 & f<35000){
    fictab[i,4]<-"Low Income"
    fictab[i,5]<-25000
    fictab[i,6]<-"$15,000 - $24,999" 
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=35000 & f<50000){
    fictab[i,4]<-"Middle Income"
    fictab[i,5]<-50000
    fictab[i,6]<-"$35,000 - $49,999" 
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=50000 & f<76000){
    fictab[i,4]<-"Middle Income"
    fictab[i,5]<-76600
    fictab[i,6]<-"$50,000 - $75,999"
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=76000 & f<100000){
    fictab[i,4]<-"High Income"
    fictab[i,5]<-100000
    fictab[i,6]<-"$76,000 - $99,999"   
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=100000 & f<150000){
    fictab[i,4]<-"High Income"
    fictab[i,5]<-150000
    fictab[i,6]<-"$100,000 - $149,999" 
    fictab[i,7]<-(f/100)
    
  }
}


for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=150000 & f<200000){
    fictab[i,4]<-"High Income"
    fictab[i,5]<-200000
    fictab[i,6]<-"$150,000 - $199,999" 
    fictab[i,7]<-(f/100)
    
  }
}

for (i in 1:nrow(fictab)){
  f <-as.numeric(fictab$income[i])
  if(f>=200000){
    fictab[i,4]<-"High Income"
    fictab[i,5]<-350000
    fictab[i,6]<-"$200,000 - $350,000" 
    fictab[i,7]<-(f/100)
    
  }
}

#Rename column headers
colnames(fictab)[colnames(fictab)=="V4"]<-"Economic Status"
colnames(fictab)[colnames(fictab)=="V5"]<-"Income Limit"
colnames(fictab)[colnames(fictab)=="V6"]<-"Income Bracket"
colnames(fictab)[colnames(fictab)=="V7"]<-"Divided Income"

#This one works
attach(fictab)
ggplot(fictab, aes(as.integer(`Divided Income`),Freq)) +
  geom_point(aes(x = as.integer(`Divided Income`),y=Freq, color=`Economic Status` ))+
  geom_smooth(method="lm", se=FALSE, color="#fffc01")+
  labs(y="Number of Stores", x="Income Brackets by 1 = $1,000")

ggplot(fictab) +
  geom_bar(mapping = aes(x=`Economic Status`, y = Freq, fill = `Economic Status`), stat = "identity")

ggplot(fictab, aes(x=`Economic Status`, y = Freq, fill = `Economic Status`)) +
  geom_boxplot()
