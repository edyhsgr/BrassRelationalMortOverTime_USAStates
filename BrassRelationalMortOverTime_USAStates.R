##########
#Brass relational life table parameters by state over time review - Eddie Hunsinger, December 2018
#
#Following code/steps by UToronto professor Monica Alexander as a template: https://github.com/MJAlexander/states-mortality/blob/master/lifespan_variation/lifespan.R 
#(related blog post: https://www.monicaalexander.com/2018/12/21/lifespan-variation-as-a-measure-of-mortality-progress-and-inequality/) 
#
#All data is from the United States Mortality Database (https://usa.mortality.org/) 
#(using the bundled lifetables.zip file)
##########

##########
#Packages needed
library(tidyverse)
library(geofacet)
##########

##########
#List states and read in all the state life tables and create one big dataframe with all states
states <- list.files("C:/.../lifetables/States")

lt_male <- c()
lt_female <- c()
lt_both <- c()

for(i in 1:length(states)){
  folder_path <- paste0("C:/.../lifetables/States/",states[i])
  state_male <- read_csv(paste0(folder_path, "/", states[i], "_mltper_1x1.csv"))
  state_female <- read_csv(paste0(folder_path, "/", states[i], "_fltper_1x1.csv"))
  state_both <- read_csv(paste0(folder_path, "/", states[i], "_bltper_1x1.csv"))
 
  lt_male <- rbind(lt_male, state_male)
  lt_female <- rbind(lt_female, state_female)
  lt_both <- rbind(lt_both, state_both)
  
  rm(state_male)
  rm(state_female)
  rm(state_both)
}

lt <- bind_rows(lt_female, lt_male, lt_both)
##########

##########
#Calculate e0, BA, and BB for states by year
e0 <- lt %>% 
  filter(Age==0) %>% 
#  filter(PopName=="AK") %>%
#  filter(Sex=="f") %>%
  mutate(e0=ex)
e0 <- select(e0,-c(Age,mx,qx,ax,lx,dx,Lx,Tx,ex))

brass_standard <- lt %>% 
  filter(Year==2015) %>% 
#  filter(PopName=="AK") %>%
#  filter(Sex=="f") %>%
  mutate(Yx_standard = .5*log((lx/100000)/(1-(lx/100000)))
)
brass_standard <- select(brass_standard,-c(Year,mx,qx,ax,lx,dx,Lx,Tx,ex))

brass <- lt %>% 
#  filter(PopName=="AK") %>%
#  filter(Sex=="f") %>%
  mutate(Yx = .5*log((lx/100000)/(1-(lx/100000)))) 

brass<-brass %>% 
  left_join(brass_standard, by = c("PopName","Age","Sex")) %>% 
  filter(Age %in% (1:100))

brass<- brass %>% 
  group_by(PopName, Sex, Year) %>% 
  mutate(
	BA = lm(Yx~Yx_standard)$coefficients[1],
	BB = lm(Yx~Yx_standard)$coefficients[2]) %>% 
  group_by(PopName, Sex, Year, BA, BB) %>% 
summarise()

BABBe0<-brass %>% 
  left_join(e0, by = c("PopName","Sex","Year"))

##########

##########
#Plot state graphs, BA and BB
BABBe0 %>% 
  filter(Sex=="b", Year>1959) %>% 
  ggplot(aes(Year, BA, color = "Brass Alpha")) + 
  ggtitle("Brass Relational Life Table (lx) Alpha (level) and Beta (shape), US States, 1960 to 2015 (using state-specific 2015 lx as standard)") +
  facet_geo(~PopName) + 
  geom_line() +
  geom_line(aes(Year, BB-1, color = "Brass Beta")) + 
  scale_y_continuous(sec.axis = sec_axis(~.+1, name = "Brass Beta")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Brass Alpha",
       x = "Year",
       colour = "")  +
  theme_bw() + 
  scale_x_continuous(breaks=seq(1980, 2010, 30))
ggsave("C:/.../BABB.png", height=7,width=12)
##########

##########
#Plot state graphs, BA and e0
e0BAfit<-lm(BABBe0$e0[BABBe0$Sex=="b"]~BABBe0$BA[BABBe0$Sex=="b"])

BABBe0 %>% 
  filter(Sex=="b", Year>1959) %>% 
  ggplot(aes(Year, BA, color = "Brass Alpha")) + 
  ggtitle("Brass Relational Life Table Alpha (state-specific 2015 lx as standard), and Life Expectancy at Birth (e0), US States, 1960 to 2015") +
  facet_geo(~PopName) + 
  geom_line() +
  geom_line(aes(Year, (e0-e0BAfit$coefficients[1])/e0BAfit$coefficients[2], color = "e0")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*e0BAfit$coefficients[2]+e0BAfit$coefficients[1],name = "e0")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "Brass Alpha",
       x = "Year",
       colour = "")  +
  theme_bw() + 
  scale_x_continuous(breaks=seq(1980, 2010, 30))
ggsave("C:/.../BAe0.png", height=7,width=12)
##########

##########
#View table 
#View(BABBe0 %>% 
#  filter(Year=="1959", Sex == "b") %>% 
#  arrange(Year))
##########
 
