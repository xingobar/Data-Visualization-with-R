# http://docs.ggplot2.org/0.9.3.1/geom_bar.html
# https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
# http://docs.ggplot2.org/current/scale_brewer.html
# http://stackoverflow.com/questions/16074440/r-ggplot2-center-align-a-multi-line-title
# http://docs.ggplot2.org/current/scale_brewer.html
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(gridExtra)




df = read_csv('~/Downloads/vgsales.csv')

### Platform count
platform = df %>% group_by(Platform) %>% summarise(Count = n())
p1 = ggplot(aes(x = Platform , y = Count , fill=Count) , data=platform) +
geom_bar(colour='black',stat='identity') +
theme_bw()+
theme(axis.text.x = element_text(angle=45,hjust=1) , 
      plot.title = element_text(hjust=0.5))+  ## title center
ggtitle('Platform Count')+
scale_fill_distiller(palette = 'RdYlBu') +
ylab('Count')


grid.arrange(p1, ncol = 1)
##########

#### Platform sales

platform_sales = df %>% group_by(Platform) %>% summarise(Global_Sales = sum(Global_Sales),
                                                         NA_Sales = sum(NA_Sales),
                                                         EU_Sales = sum(EU_Sales),
                                                         JP_Sales = sum(JP_Sales))

platform_sales = melt(platform_sales)
names(platform_sales) = c('Platform','SaleType','Sale')
ggplot(data = platform_sales,aes(x = Platform ,y = Sale , fill = SaleType)) + 
geom_bar(colour='black',stat='identity',position='dodge') + 
theme_bw()+
theme(axis.text.x = element_text(angle=45,hjust=1),
      plot.title = element_text(hjust=0.5))+
ggtitle('Platform Sales') +
scale_fill_brewer(palette = 'YlGnBu')



############


#### Genre Sales ######

genre_sales = df %>% group_by(Genre) %>% summarise(GlobalSales = sum(Global_Sales),
                                                   NA_Sales = sum(NA_Sales),
                                                   EU_Sales = sum(EU_Sales),
                                                   JP_Sales = sum(JP_Sales)) 
genre_sales = melt(genre_sales)
names(genre_sales) = c('Genre','SaleType','Sale')

ggplot(data=genre_sales,aes(x = Genre,y = Sale,fill=SaleType)) + 
geom_bar(colour='black' , stat='identity', position='dodge') +  
theme_bw()+
theme(axis.text.x = element_text(hjust=1,angle=45),
      plot.title = element_text(hjust=0.5)) + ## center 
ggtitle('Gener Sale') + 
scale_fill_brewer(palette = 'YlGnBu')+
ylab('Sale')


###########



### Genre Count 

genre_count = df %>% group_by(Genre) %>% summarise(Count = n())

ggplot(data=genre_count , aes(x = Genre,y=Count,fill=Count)) +
geom_bar(colour='black',stat='identity') +
theme_bw()+
ggtitle('Genre Count') + 
theme(axis.text.x = element_text(angle=45,hjust=1),
      plot.title = element_text(hjust=0.5)) +
scale_fill_distiller(palette = 'RdYlBu') + 
ylab('Count') 

#######

## Release per year

year_sales = df %>% filter(!is.na(Year)) %>% group_by(Year) %>% summarise(Count = n()) 

ggplot(data=year_sales,aes(x=Year,y=Count,fill=Count)) + 
geom_bar(colour='black',stat='identity') + 
theme_bw()+
ggtitle('Sales vs Year') +
theme(axis.text.x = element_text(angle=45,hjust=1),
      plot.title = element_text(hjust=0.5)) + 
scale_fill_distiller(palette = 'YlGnBu')


###### 


## top 20 publisher

top_publisher = df %>% group_by(Publisher) %>% 
                summarise(Count = n()) %>% 
                arrange(desc(Count)) %>% top_n(20)

ggplot(data=top_publisher,aes(x=Publisher,y=Count,fill=Count)) +
geom_bar(colour='black',stat='identity') + 
theme_bw() +
ggtitle('Top 20 Publisher') + 
theme(axis.text.x = element_text(angle=45,hjust=1),
      plot.title = element_text(hjust=0.5)) + 
scale_fill_distiller(palette = 'RdYlBu') + 
coord_flip() ## coordinate flip bar == > barh 


#############


### Most popular genre per year

popular_genre_per_year = df %>% group_by(Year,Genre) %>% 
                         summarise(GlobalSales = sum(Global_Sales)) %>%
                         arrange(desc(GlobalSales)) %>%
                         arrange(Year) %>% top_n(1)

ggplot(data = popular_genre_per_year , 
       aes(x = Year, y = GlobalSales,fill=Genre)) +
geom_bar(colour='black',stat='identity') +
ggtitle('Most popular genre per year') +
theme_bw() +
theme(axis.text.x = element_text(angle=45,hjust=1),
      plot.title = element_text(hjust=.5)) +
scale_fill_brewer(palette = 'RdYlBu')


#########














