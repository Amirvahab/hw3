##################
# Amirvahab Fakhfouri #
# Assignment 3 ###########################################
### Q0
print("Amirvahab Fakhfouri")
print(1505020)
print("afakhfou@ucsc.edu")
##############################
  
### Q1
library(foreign)
df.ex <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
class(df.ex)
#############################

### Q2  
install.packages("dplyr")
require(dplyr)
df.ex.2 <- dplyr::filter(df.ex,year == 2013 & month == 12)
print(nrow(df.ex.2))
df.ex.2 <- dplyr::filter(df.ex,   year == 2013 & (month == 7 | month == 8 | month == 9))
print(nrow(df.ex.2))
##############################

### Q3
df.ex.3a <- df.ex %>% dplyr::arrange(year, month)
###############################

### Q4
#Part (a)
df.ex.4a <- select(df.ex, year,month, minsamp,hrlonglk,age)
#Part (b)
df.ex.4b <- select(df.ex, year,month,starts_with("i", ignore.case=TRUE))
###############################
    
#### Q5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <- function(x){(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}

df.ex.5<- df.ex %>%
       dplyr::mutate(
         rw.stndz = stndz(rw),
         rw.nrmlz = nrmlz(rw)
       )
df.ex.5a <- transmute(df.ex.5, rw.stndz ,rw.nrmlz)
#

df.ex.5 <- df.ex %>%
      dplyr::group_by(year, month) %>%
          dplyr::mutate(
                rw.stndz = stndz(rw),
                rw.nrmlz = nrmlz(rw),
                count    = n()
                )
df.ex.5b <- transmute(df.ex.5, rw.stndz ,rw.nrmlz, count)
###############################

### Q6
            
df.ex.6 <- df.ex %>%
  dplyr::group_by(year, month, state) %>%
  dplyr::summarise(
                    rw.min = min(rw, na.rm = T),
                    rw.1stq = quantile(rw, 0.25, na.rm = T),
                    rw.mean = mean(rw, na.rm = T),
                    rw.median = median(rw, na.rm = T),
                    rw.3rdq = quantile(rw, 0.75, na.rm = T),
                    rw.max = max(rw, na.rm = T),
                    count = n()
                  )
            
              print(nrow(df.ex.6))
              
### Q6 Second part
             
            max.rw.mean<-max(df.ex.6$rw.mean, na.rm = TRUE)
            highest_mean <- df.ex.6 %>%
              select(year,month,state,rw.mean) %>%
              filter(rw.mean == max.rw.mean)
            print(highest_mean)
######################################################################################            
        
         
              
              