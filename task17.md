---
title: "Task 17"
author: "Victoria Lau"
date: "March 06, 2021"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---






```r
# Use this R-Chunk to import all your datasets!

waitlist<- read_csv(url("https://byuistats.github.io/M335/data/waitlist_DP_108.csv"))
waitlist
```

```
## # A tibble: 1,109 x 6
##    `Semester Term … `Person ID` `Course Sec` `Registration D… Status
##    <chr>                  <dbl> <chr>        <chr>            <chr> 
##  1 FA17                       1 FDMAT108-33  8/28/2017 13:55  Wait …
##  2 FA17                       1 FDMAT108-33  9/10/2017 15:13  Dropp…
##  3 FA17                       1 FDMAT108-33  9/10/2017 15:13  Regis…
##  4 FA17                       1 FDMAT108-33  10/25/2017 16:35 Withd…
##  5 WI17                       2 FDMAT108-A3  1/5/2017 16:46   Wait …
##  6 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Dropp…
##  7 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Regis…
##  8 FA16                       3 FDMAT108-15  7/19/2016 14:36  Wait …
##  9 FA16                       3 FDMAT108-15  9/1/2016 17:50   Dropp…
## 10 WI17                       4 FDMAT108-A3  12/1/2016 21:27  Regis…
## # … with 1,099 more rows, and 1 more variable: `Waitlist Reason` <chr>
```

## Background

Have you ever been on a wait-list for a class? It can be pretty nerve racking, especially if all your other classes and your work schedule depend on getting into that particular class in that particular time slot.

I obtained transnational data from the registration system for Math108 (Math in the Real World) sections I taught in Fall2016, Winter2017, and Fall2017. I wanted to do some analysis to help waitlisted students better understand their chances of getting in. To simplify this task, we will only look at 1 section: FDMAT108-18.

## Data Wrangling


```r
# Use this R-Chunk to clean & wrangle your data!
waitlist %>%
mutate(date = lubridate::mdy_hms('Registration Date'))
```

```
## # A tibble: 1,109 x 7
##    `Semester Term … `Person ID` `Course Sec` `Registration D… Status
##    <chr>                  <dbl> <chr>        <chr>            <chr> 
##  1 FA17                       1 FDMAT108-33  8/28/2017 13:55  Wait …
##  2 FA17                       1 FDMAT108-33  9/10/2017 15:13  Dropp…
##  3 FA17                       1 FDMAT108-33  9/10/2017 15:13  Regis…
##  4 FA17                       1 FDMAT108-33  10/25/2017 16:35 Withd…
##  5 WI17                       2 FDMAT108-A3  1/5/2017 16:46   Wait …
##  6 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Dropp…
##  7 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Regis…
##  8 FA16                       3 FDMAT108-15  7/19/2016 14:36  Wait …
##  9 FA16                       3 FDMAT108-15  9/1/2016 17:50   Dropp…
## 10 WI17                       4 FDMAT108-A3  12/1/2016 21:27  Regis…
## # … with 1,099 more rows, and 2 more variables: `Waitlist Reason` <chr>,
## #   date <dttm>
```

```r
waitlist %>% arrange(desc(mdy(waitlist$'Registration Date')))
```

```
## # A tibble: 1,109 x 6
##    `Semester Term … `Person ID` `Course Sec` `Registration D… Status
##    <chr>                  <dbl> <chr>        <chr>            <chr> 
##  1 FA17                       1 FDMAT108-33  8/28/2017 13:55  Wait …
##  2 FA17                       1 FDMAT108-33  9/10/2017 15:13  Dropp…
##  3 FA17                       1 FDMAT108-33  9/10/2017 15:13  Regis…
##  4 FA17                       1 FDMAT108-33  10/25/2017 16:35 Withd…
##  5 WI17                       2 FDMAT108-A3  1/5/2017 16:46   Wait …
##  6 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Dropp…
##  7 WI17                       2 FDMAT108-A3  1/9/2017 20:03   Regis…
##  8 FA16                       3 FDMAT108-15  7/19/2016 14:36  Wait …
##  9 FA16                       3 FDMAT108-15  9/1/2016 17:50   Dropp…
## 10 WI17                       4 FDMAT108-A3  12/1/2016 21:27  Regis…
## # … with 1,099 more rows, and 1 more variable: `Waitlist Reason` <chr>
```

```r
waitlist1 <- waitlist %>% 
  rename(
    Course_Sec = 'Course Sec',
    Waitlist_Reason = 'Waitlist Reason'
    )

waitlist2 <- waitlist1 %>% 
filter(Course_Sec == 'FDMAT108-18')

# waitlist2[order(as.Date(waitlist2$date, format="%m/%d/%Y %h/%m"))]
# View(waitlist2)
```

#Function 1

```r
registered_percentage <- function(p){
  (p/77) * 100
}
Y <- count(waitlist2, R = Waitlist_Reason == 'Waitlist Registered')  %>% 
  filter(R == TRUE) %>% 
  select(n) 
registered_percentage(Y)
```

```
##          n
## 1 20.77922
```

```r
#count(waitlist2, Status == 'Registered')
#count(waitlist2, Waitlist_Reason == 'Waitlist Registered')
```

#Function 2

```r
waitlist_percentage <- function(w){
  (w/67) *100
}
#count(waitlist2, Status == 'Wait List')
#count(waitlist2, Waitlist_Reason == 'Waitlist Registered')
W <- count(waitlist2, R = Waitlist_Reason == 'Waitlist Registered')  %>% 
  filter(R == TRUE) %>% 
  select(n) 
waitlist_percentage(Y)
```

```
##         n
## 1 23.8806
```

## Data Visualization


```r
# Use this R-Chunk to plot & visualize your data!
```

## Conclusions
