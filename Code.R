library(waffle)
library(tidyverse)
library(reshape2)
library(tidytuesdayR)
library(lubridate)
library(ggtext)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

race%>%
  left_join(ultra_rankings)%>%
  distinct()%>%
  mutate(Year=as.Date(date)%>%
           year())%>%
  select(Year,participants,gender)%>%
  mutate(gender=recode(gender,"M"="Men","W"="Women"))%>%
  group_by(Year,gender)%>%
  filter(!is.na(gender))%>%
  count()%>%
  spread(gender,n)%>%
  rowwise()%>%
  mutate(Total=sum(Men,Women))%>%
  rowwise()%>%
  mutate(WomenPercentage=round((Women/Total)*100))%>%
  rowwise()%>%
  mutate(menPercentage=round((Men/Total)*100))%>%
  data.frame()->share1

share1[-c(2,3,4)]->share2
share2
melt(share2,id.vars = "Year",
     measure.vars = c("WomenPercentage","menPercentage"),
     value.name = "Share")->share3
share3
ggplot(share3,aes(fill=variable,values=Share))+
  expand_limits(x=c(0,0), y=c(0,0)) +
  geom_waffle(n_rows = 10, size = 1, flip = TRUE,
              colour=alpha("white",1/3),
              make_proportional = TRUE,
              height = 0.8, width = 0.8) +
  scale_fill_manual(name = NULL,
                    values = c("#fe4a49", alpha("black",1/3))) +
  facet_wrap(~Year,ncol=5)+
  coord_equal()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text=element_blank(),
        strip.background = element_rect(fill="#051e3e"),
        strip.text = element_text(colour="#fed766",face="bold",size=12),
        panel.grid = element_blank(),
        plot.background = element_rect(fill="#051e3e"),
        panel.background = element_rect(fill="#051e3e"))+
  labs(title="<b style='color:#fed766'>SHARE OF</b>
       <span style='color:#fe4a49'>FEMALE</span>
       <span style='color:#fed766'>ULTRA-RUNNERS HAS BEEN ON THE RISE IN THE LAST DECADE</span>",
       subtitle = str_wrap("<span style='color:#fed766'>The below data visualization shows the increase in the share of
                           <span style='color:#fe4a49'>female </span><span style='color:#fed766'>ultra-runners in percentage since 2012.<br>The share of women runners was the highest last year (17%) and the lowest this year (13%). Here's an overview </span>",50),
       caption = 'Data:BjnNowak-Github Repo via Tidy Tuesday | Design: @annapurani93')+
       theme(plot.title = element_markdown(face="bold",size=21, hjust=0.5),
        plot.subtitle = element_markdown(size=16,hjust=0.5),
        plot.caption = element_text(colour="#fed766",size=12))->waffleplot

ggsave("waffle.png",waffleplot,width=15,height=7.7)
ggsave("waffle.pdf",waffleplot,width=15,height=7.7)


