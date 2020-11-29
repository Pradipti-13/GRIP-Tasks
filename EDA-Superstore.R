#GRIP-TSF-Task-3
#Exploratory Data Analysis- Retail (Level-Beginner)

#Let's load our R Packages that we will use here.
#Loading-Packages
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(skimr)
library(tidyverse)
library(scales)

#Let's load our data now.
#Loading-Data
getwd()
Superstore= read.csv("SampleSuperstore.csv")

#Now let us summarise the data and understand the outputs.
#Summarising-Data
head(Superstore)
tail(Superstore)
skim(Superstore)
summary(Superstore)
str(Superstore)
glimpse(Superstore)
##Observation-None of the values are missing.
##Observation- We have some pre-processed data here.

#Let us do some data visualizations now. 
#Data-Visualizations

##Let's do some Sales Analysis
#Statewise Sales Analysis
Statewise_Sales= Superstore %>%
  group_by(State) %>% 
  summarise(Total_Sales= sum(Sales)) %>%
  arrange(desc(Total_Sales))
View(Statewise_Sales)
#Plotting Statewise Sales Analysis
Sales_A = ggplot(Superstore, aes(x=State,
                                y=Sales,
                                fill= State)) +
  geom_col() + 
  ggtitle("Statewise Sales Analysis") + 
  coord_flip()+
  theme(legend.position = "None",
        axis.text.y = element_text(size= 6))

#Regionwise Sales Analysis
Regionwise_Sales= Superstore %>%
  group_by(Region) %>%
  summarise(TotalS= sum(Sales)) %>%
  arrange(desc(TotalS))
View(Regionwise_Sales)
#Plotting Regionwise Sales Analysis
Sales_C=ggplot(Superstore, aes(x=Region,
                               y=Sales,
                               fill= Region)) +
  geom_col()+
  ggtitle("Regionwise Analysis of Sales")+
  coord_flip()

#Geographic Plots of Statewise Sales Analysis 
GSPlot= Superstore %>%
  group_by(State) %>%
  summarise(Total_Sales= sum(Sales)) %>%
  arrange(desc(Total_Sales))
#Converting into a Geographic Plot
colnames(GSPlot)= c('region', 'value')
GSPlot$region= tolower(GSPlot$region)
Sales_D= state_choropleth(GSPlot,
                          title= "Geographic Analysis of Sales", 
                          legend="Sales in USD")

##Observations- 
#1.State of California recorded the highest Sales, around 450,000 USD.
#2.New York has the second highest Sales, around 300,000 USD.
#3.States of Texas and Washington record the third and fourth highest sales, 
#around 150,000 USD and 140,000 USD respectively.
#4.North Dakota records the least Sales, around 1000 USD. 
#5.We can see from the geographic and regionwise plots that the company's Sales 
#are mostly concentrated on the East and West Coast of America.

##Let's do some Profit Analysis
#Statewise Profit Analysis
Statewise_Profit= Superstore %>%
  group_by(State) %>% 
  summarise(Total_Profit= sum(Profit)) %>%
  arrange(desc(Total_Profit))
View(Statewise_Profit)
#Plotting Statewise Profit Analysis
Profit_A = ggplot(Superstore, aes(x=State,
                                 y=Profit,
                                 fill= State)) +
  geom_col() + 
  ggtitle("Statewise Profit Analysis") + 
  coord_flip()+
  theme(legend.position = "None",
        axis.text.y = element_text(size= 6))

#Regionwise Profit Analysis
Regionwise_Profit= Superstore %>%
  group_by(Region) %>%
  summarise(TotalP= sum(Profit)) %>%
  arrange(desc(TotalP))
View(Regionwise_Profit)
#Plotting Regionwise Profit Analysis
Profit_C= ggplot(Superstore, aes(x=Region,
                                 y=Profit,
                                 fill= Region)) +
  geom_col()+
  ggtitle("Regionwise Analysis of Profit")+
  coord_flip()

#Geographic Plots of Statewise Sales Analysis 
GPPlot= Superstore %>%
  group_by(State) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  arrange(desc(Total_Profit))
#Converting into a Geographic Plot
colnames(GPPlot)= c('region', 'value')
GPPlot$region= tolower(GPPlot$region)
Profit_D= state_choropleth(GPPlot,
                           title= "Geographic Analysis of Profit", 
                           legend="Profit in USD")
##Observations-
#1.From the Statewise Profit Analysis, the States of New York, California and
#Washington recorded the most profits.
#2.The State of Texas was the most unprofitable for the company causing severe 
#losses.


#Doing some Statewise Profit/Sales Ratio Analysis 
BarPlot= Superstore %>% 
  group_by(State) %>%
  summarise(Profit_Sales_Ratio= sum(Profit)/sum(Sales)) %>%
  arrange(desc(Profit_Sales_Ratio))
Barplot3= as.data.frame(BarPlot)
Profit_to_Sales= ggplot(Barplot3, aes(x=Profit_Sales_Ratio,
                                      y= State,
                                      fill= State)) +
  ggtitle("Statewise Profit_Sales_Ratio Analysis")+
  theme(legend.position = "None",
        axis.text.y = element_text(size= 5))+
  geom_col()
##Observations-
#1.Some Profit to Sales Ratio Analysis showed that Ohio had the worst 
#Profit-Sales Ratio
#2.We also understand that the states making the most profits, i.e, New York,
#California, and Washington do not show the maximum Profit-Sales ratio meaning that 
#the company can improve its profits/sales in these states.

#Sales and Profit Analysis Segmentwise 
Boxseg4= Superstore %>%
  group_by(Segment) %>%
  summarise(Ratio= sum(Profit)/sum(Sales)) %>%
  ggplot(aes(x=Segment, y= Ratio, fill=Segment))+
  ggtitle("Profit/Sales Ratio Plots for each segment")+
  geom_col()
##Observations-
#1.For the Home Office Segment the Profit/Sales Ratio is the highest.
#2.There is scope for the company to improve its Profit/Sales ratio in the Consumer 
#and Corporate Segment.

#Now Let us take up five different states and try to understand what
#kind of products are most profitable
#We will choose the states of California, New Jersey, Connecticut, 
#Wisconsin and Colorado for our Analysis.

##Office-Supplies
#Profitable Office Supplies in California
CaliforniaOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "California" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in California State")+
  geom_col()

#Profitable Office Supplies in New Jersey
New_JerseyOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "New Jersey" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in New Jersey State")+
  geom_col()

#Profitable Office Supplies in Connecticut
ConnecticutOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "Connecticut" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in Connecticut State")+
  geom_col()

#Profitable Office Supplies in Connecticut
ConnecticutOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "Connecticut" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in Connecticut State")+
  geom_col()

#Profitable Office Supplies in Wisconsin
WisconsinOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "Wisconsin" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in Wisconsin State")+
  geom_col()

#Profitable Office Supplies in Colorado
ColoradoOS= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Office Supplies" & State== "Colorado" ) %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Office Supplies in Colorado State")+
  geom_col()

#Let's make similar graphs for Furniture and Technology Segment.
##Furnitures
#Profitable Furnitures in California
CaliforniaFur= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Furniture" & State== "California") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Furnitures in California State")+
  geom_col()

#Profitable Furnitures in New Jersey
New_JerseyFur= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Furniture" & State== "New Jersey") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Furnitures in New Jersey State")+
  geom_col()

#Profitable Furnitures in Connecticut
ConnecticutFur= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Furniture" & State== "Connecticut") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Furnitures in Connecticut State")+
  geom_col()

#Profitable Furnitures in Wisconsin
WisconsinFur= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Furniture" & State== "Wisconsin") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Furnitures in Wisconsin State")+
  geom_col()

#Profitable Furnitures in Colorado
ColoradoFur= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Furniture" & State== "Colorado") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Furnitures in Colorado State")+
  geom_col()

##Technology
#Profitable Technology in California
CaliforniaTech= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Technology" & State== "California") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Technology in California State")+
  geom_col()

#Profitable Technology in New Jersey
New_JerseyTech= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Technology" & State== "New Jersey") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Technology in New Jersey State")+
  geom_col()

#Profitable Technology in Connecticut
ConnecticutTech= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Technology" & State== "Connecticut") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Technology in Connecticut State")+
  geom_col()

#Profitable Technology in Wisconsin
WisconsinTech= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Technology" & State== "Wisconsin") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Technology in Wisconsin State")+
  geom_col()

#Profitable Technology in Colorado
ColoradoTech= Superstore %>%
  group_by(Sub.Category) %>% 
  filter(Category== "Technology" & State== "Colorado") %>%
  summarise(Total_Profit= sum(Profit)) %>%
  ggplot(aes(x= Total_Profit, y= Sub.Category, fill= Sub.Category))+ 
  ggtitle("Profitable Technology in Colorado State")+
  geom_col()
#We will see these graphs in the Dashboard section.
##Observations-
#1.In the State of California, in the Office Supplies segment,
#Binders and Paper are some of the most profitable products.In the 
#Furniture Segment,Furnishings are the most profitable,
#whereas tables are a little un-profitable.In the technology segment,
#Accessories and Copiers are the most profitable products.
#2.In New Jersey, in the Office Supplies segment, most profit comes
#from the Binders.In the Furniture Segment, Chairs are the most 
#profitable with Tables accounting for loss.In the Technology
#segment, Machines are the most profitable products.
#3.In the state of Connecticut, Appliances, Paper and Storage are the 
#most profitable.Chairs being most profitable in the Furniture Segment,
#whereas phones are the most profitable in the Technology Segment.
#4.In Winconsin, Appliances and Paper are the most profitable, in the
#Office-Supplies Segment, Chairs are most profitable in Furniture 
#Segment, and Accessories are the most profitable in the Technology 
#Segment.
#5.In the State if Colorado, Binders, Supplies and Storage contribute 
#to loss in the Office Supplies Segment, whereas Bookcases and tables 
#account for losses in the Furniture Segment. Machines incur huge losses
#to the company in the state of Colorado.

#Price per product in different Sub-Categories
Price_per_Product= ggplot(Superstore, aes(x= Sub.Category, 
                                          y= sum(Sales)/sum(Quantity),
                                          fill= Sub.Category))+
  ggtitle("Price per product in different Sub-Categories")+
  theme(axis.text.x = element_text(angle= 60, size= 5))+
  geom_col()

#Profit per product in different Sub-Categories
Profit_per_Product= ggplot(Superstore, aes(x= Sub.Category, 
                                           y= sum(Profit)/sum(Quantity),
                                           fill= Sub.Category))+
  ggtitle("Profit per product in different Sub-Categories")+
  theme(axis.text.x = element_text(angle= 60, size= 5))+
  geom_col()
##Observations-
#1.Price per Product and Profit per product show similar trends with 
#Binders and Paper topping the chart.

#Let us look at the Shipping Models now
ShippingPro= Superstore %>%
  ggplot(aes(x=Ship.Mode,
             y=Profit, 
             fill= Ship.Mode))+
  geom_col()

ShippingCate= Superstore %>%
  ggplot(aes(x=Ship.Mode,
             y= Quantity, 
             fill= Ship.Mode))+
  geom_col()+
  theme(axis.text.x = element_text(angle= 60, size= 5))+
  facet_wrap(~Category)

ShippingSeg= Superstore %>%
  ggplot(aes(x=Ship.Mode,
             y= Quantity,
             fill= Ship.Mode))+
  geom_col()+
  theme(axis.text.x = element_text(angle= 60, size= 5))+
  facet_wrap(~Segment)

##Observations-
#1.Standard Class is the most used shipping method among all consumer segments.
#2.Office Supplies are the most bought products and Consumer segment is the 
#largest Segment among the three.

#Sales and Profits with and without discount
Superstore3= Superstore
Superstore3$DiscountedPrice= Superstore3$Sales- (Superstore3$Sales*Superstore3$Discount)
Superstore3$Sales_Quantity= Superstore3$Sales/Superstore3$Quantity
Superstore3$DP_Quantity= Superstore3$DiscountedPrice/Superstore3$Quantity
View(Superstore3)

No_Discount=Superstore3 %>% 
  filter(Discount==0.00) %>%
  summarise(Total_Quantity= sum(Quantity)) 
No_Discount

Discount=Superstore3 %>% 
  filter(Discount!=0.00) %>%
  summarise(Total_Quantity= sum(Quantity)) 
Discount

Discount- No_Discount
 
##Observations-
#1.Considering sales across different categories, with and without discount,
#there is more sales taking place in the case with Discount, which is 
#obvious.

#Pradipti Thakur.

#Let's put all our charts on the Dashboard now.


