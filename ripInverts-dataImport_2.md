---
title: "Riparian Invertebrates: Data Import 2"
output: html_notebook
---

##Packages

```r
# packages
library(tidyverse)
library(readxl)
library(forcats)
```

##Import data 

```r
#importing all as text to begin with
sel_values <- read_excel("../data/Riparian Data August 2018.xlsx", sheet = 2, col_types = "text")

ers_data2 <- read_excel("../data/Riparian Data August 2018.xlsx", sheet = 4, col_types = "text") 
```





```r
skimr::skim(sel_values)
```

```
## Skim summary statistics
##  n obs: 79 
##  n variables: 9 
## 
## -- Variable type:character ------------------------------------------------
##           variable missing complete  n min max empty n_unique
##             Effort       0       79 79   1   2     0       12
##           Marsh No       0       79 79   1   2     0       24
##          Marsh SQI       0       79 79   1   3     0       20
##          Marsh SQS       0       79 79   1  18     0       29
##          NE Sample       0       79 79   3  26     0       79
##   Running Water No       0       79 79   1   2     0       31
##  Running Water SQI       0       79 79   3   3     0       56
##  Running Water SQS       0       79 79   1  18     0       69
##         Wetland No       0       79 79   1   2     0       43
```

```r
skimr::skim(ers_data2)
```

```
## Warning in min(characters, na.rm = TRUE): no non-missing arguments to min;
## returning Inf
```

```
## Warning in max(characters, na.rm = TRUE): no non-missing arguments to max;
## returning -Inf
```

```
## Skim summary statistics
##  n obs: 8050 
##  n variables: 22 
## 
## -- Variable type:character ------------------------------------------------
##                 variable missing complete    n min  max empty n_unique
##           Abundance_Data    3697     4353 8050   1   25     0      202
##          Author/Recorder    2171     5879 8050   5   79     0       13
##                  Country       0     8050 8050   5    7     0        2
##                   County     195     7855 8050   5   15     0       17
##                     Date       9     8041 8050   4   24     0      147
##     Handsearch & Pitfall    7223      827 8050   1    1     0        1
##  Handserach & excavation    8050        0 8050 Inf -Inf     0        0
##            Location_Name      14     8036 8050   2   30     0      188
##      NE Full Sample Name      44     8006 8050   7   26     0       79
##    Sadler & Bell method?    2120     5930 8050   1   19     0        6
##             Sample River       0     8050 8050   3   15     0       47
##          Sample_Duration    7425      625 8050   6   14     0        3
##          Sample_Grid_Ref    2173     5877 8050   6   12     0      170
##              Sample_Type       1     8049 8050   5   22     0       30
##             Species_name       0     8050 8050  11   30     0      657
##                Substrate    6425     1625 8050   4   24     0       26
##       Survey/Report_Name       0     8050 8050  10  130     0       18
##         Taxonomic_Family       3     8047 8050   2   16     0       70
##          Taxonomic_Order       0     8050 8050   7   13     0        7
##                  VC name    2440     5610 8050   6   20     0       20
##                VC number    3009     5041 8050   1    2     0       19
##                     Year     193     7857 8050   4   11     0       17
```


#Full sSample data

#### Get a handle on numeric data

Try to find all the numeric values

```r
#new column, convert abundance to numeric. 
ers_data2$abund_num <- as.numeric(ers_data2$Abundance_Data)
```

```
## Warning: NAs introduced by coercion
```

```r
skimr::skim(ers_data2$abund_num)
```

```
## 
## Skim summary statistics
## 
## -- Variable type:numeric --------------------------------------------------
##             variable missing complete    n  mean    sd p0 p25 p50 p75 p100
##  ers_data2$abund_num    4249     3801 8050 15.68 79.77  1   1   2   6 2311
##      hist
##  <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```

```r
#all data containing a non-numeric is now NA, all numeric is numeric
plot(ers_data2$abund_num)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
hist(ers_data2$abund_num, breaks = 100)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-2.png)

```r
hist(ers_data2$abund_num, breaks = 1000, xlim = c(0,100))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-3.png)



```r
#split numeric and character abundance values: 
#create numeric column
abund_num <- ers_data2 %>% 
  select(abund_num) %>%  
  filter(is.na(abund_num) == FALSE) 
abund_num <- abund_num$abund_num

length(unique(abund_num))
```

```
## [1] 186
```

```r
length(abund_num)
```

```
## [1] 3801
```


```r
ers_hist <- hist(abund_num, breaks = 100, plot = FALSE)
knitr::kable(tibble(ers_hist$breaks[-1], ers_hist$counts))
```



| ers_hist$breaks[-1]| ers_hist$counts|
|-------------------:|---------------:|
|                  20|            3389|
|                  40|             168|
|                  60|              70|
|                  80|              37|
|                 100|              25|
|                 120|              14|
|                 140|              16|
|                 160|               8|
|                 180|              11|
|                 200|               8|
|                 220|               5|
|                 240|               5|
|                 260|               6|
|                 280|               1|
|                 300|               6|
|                 320|               4|
|                 340|               2|
|                 360|               2|
|                 380|               3|
|                 400|               2|
|                 420|               2|
|                 440|               1|
|                 460|               1|
|                 480|               2|
|                 500|               2|
|                 520|               0|
|                 540|               0|
|                 560|               0|
|                 580|               0|
|                 600|               0|
|                 620|               0|
|                 640|               1|
|                 660|               2|
|                 680|               0|
|                 700|               0|
|                 720|               0|
|                 740|               0|
|                 760|               0|
|                 780|               1|
|                 800|               0|
|                 820|               0|
|                 840|               0|
|                 860|               0|
|                 880|               2|
|                 900|               0|
|                 920|               0|
|                 940|               1|
|                 960|               0|
|                 980|               0|
|                1000|               0|
|                1020|               0|
|                1040|               0|
|                1060|               0|
|                1080|               0|
|                1100|               0|
|                1120|               0|
|                1140|               0|
|                1160|               0|
|                1180|               0|
|                1200|               0|
|                1220|               0|
|                1240|               0|
|                1260|               0|
|                1280|               0|
|                1300|               0|
|                1320|               0|
|                1340|               0|
|                1360|               0|
|                1380|               0|
|                1400|               0|
|                1420|               0|
|                1440|               0|
|                1460|               0|
|                1480|               1|
|                1500|               0|
|                1520|               0|
|                1540|               0|
|                1560|               0|
|                1580|               0|
|                1600|               0|
|                1620|               0|
|                1640|               0|
|                1660|               1|
|                1680|               0|
|                1700|               0|
|                1720|               0|
|                1740|               0|
|                1760|               0|
|                1780|               0|
|                1800|               0|
|                1820|               0|
|                1840|               0|
|                1860|               0|
|                1880|               0|
|                1900|               0|
|                1920|               0|
|                1940|               0|
|                1960|               0|
|                1980|               0|
|                2000|               1|
|                2020|               0|
|                2040|               0|
|                2060|               0|
|                2080|               0|
|                2100|               0|
|                2120|               0|
|                2140|               0|
|                2160|               0|
|                2180|               0|
|                2200|               0|
|                2220|               0|
|                2240|               0|
|                2260|               0|
|                2280|               0|
|                2300|               0|
|                2320|               1|
The vast majority of abundance values aer below 20.  

#### Get a handle on non-numeric data


```r
#create a character abundance column
ers_data2 <- add_column(ers_data2, abund_char = ers_data2$Abundance_Data) 
  
ers_data2$abund_char <- replace(ers_data2$abund_char, 
                               is.na(ers_data2$abund_num)==FALSE, 
                               NA)
```





```r
length(unique(ers_data2$abund_char))
```

```
## [1] 17
```

```r
length(ers_data2$abund_char[which(is.na(ers_data2$abund_char)==FALSE)])
```

```
## [1] 552
```
552 observations; 14 unique abundance values



```r
ers_char <- data.frame(table(ers_data2$abund_char))
ers_char <- arrange(ers_char, -Freq)
knitr::kable(ers_char)
```



|Var1                      | Freq|
|:-------------------------|----:|
|present                   |  307|
|Abundance - 1 Count Adult |   64|
|very abund                |   47|
|abundant                  |   39|
|1f                        |   37|
|1m                        |   27|
|>10                       |    5|
|100+                      |    5|
|2f                        |    5|
|500+                      |    5|
|2m                        |    4|
|3f                        |    2|
|4f                        |    2|
|3m                        |    1|
|5f                        |    1|
|5m                        |    1|

##what about sample types

```r
knitr::kable(
  ers_data2 %>%
    count(Sample_Type) %>%
    arrange(-n)
)
```



|Sample_Type            |    n|
|:----------------------|----:|
|Pitfall Trap           | 1769|
|Hand Search (25min)    | 1242|
|Hand search            | 1217|
|Pitfall                |  989|
|Excavation             |  497|
|handsearch/excavation  |  405|
|Pitfall trap           |  307|
|Handsearch             |  153|
|pitfalls               |  153|
|Hand searching         |  144|
|20 minute timed search |  135|
|Quadrat                |  130|
|Sweep                  |  126|
|handsearch             |  124|
|30 minute timed sample |  111|
|Casual record          |   88|
|timed hand collection  |   76|
|Aquatic                |   69|
|Field Observation      |   64|
|timed hand search      |   44|
|excavation             |   38|
|hand collection        |   37|
|Pitfalls               |   36|
|20 minute dig          |   34|
|pitfall                |   29|
|Hand Searching         |   13|
|timed dig              |   13|
|Netting                |    3|
|Splashing              |    2|
|Water Trap             |    1|
|NA                     |    1|


```r
unique(ers_data2$Sample_Type)
```

```
##  [1] "handsearch"             "Pitfall trap"          
##  [3] "Pitfall"                "Hand search"           
##  [5] "20 minute dig"          "handsearch/excavation" 
##  [7] "Field Observation"      "Sweep"                 
##  [9] "Hand Search (25min)"    "Hand Searching"        
## [11] "timed hand collection"  "Hand searching"        
## [13] "Aquatic"                "Netting"               
## [15] "excavation"             "pitfalls"              
## [17] "30 minute timed sample" "Pitfalls"              
## [19] "hand collection"        "pitfall"               
## [21] "Pitfall Trap"           "20 minute timed search"
## [23] "timed hand search"      "Casual record"         
## [25] "Quadrat"                "Excavation"            
## [27] "Water Trap"             "Handsearch"            
## [29] "Splashing"              "timed dig"             
## [31] NA
```



```r
table(tolower(ers_data2$Sample_Type))
```

```
## 
##          20 minute dig 20 minute timed search 30 minute timed sample 
##                     34                    135                    111 
##                aquatic          casual record             excavation 
##                     69                     88                    535 
##      field observation        hand collection            hand search 
##                     64                     37                   1217 
##    hand search (25min)         hand searching             handsearch 
##                   1242                    157                    277 
##  handsearch/excavation                netting                pitfall 
##                    405                      3                   1018 
##           pitfall trap               pitfalls                quadrat 
##                   2076                    189                    130 
##              splashing                  sweep              timed dig 
##                      2                    126                     13 
##  timed hand collection      timed hand search             water trap 
##                     76                     44                      1
```


Clean up sample type coding

```r
ers_data2 <- ers_data2 %>% 
  mutate(Sample_Type_old = Sample_Type, 
         Sample_Type = recode_factor(tolower(ers_data2$Sample_Type),
             `20 minute dig` = "excavation",
             `20 minute timed search` = "hand_search",
             `30 minute timed sample` = "unknown",
             aquatic = "aquatic",
             `casual record` = "ad-hoc",
             excavation = "excavation",
             `field observation` = "ad-hoc",
             `hand collection` = "hand_search",
             `hand search` = "hand_search",
             `hand search (25min)` = "hand_search",
             `hand searching` = "hand_search",
              handsearch = "hand_search",
             `handsearch/excavation` = "excavation",
             netting = "sweep",
             pitfall = "pitfall",
             `pitfall trap` = "pitfall",
             pitfalls = "pitfall",
             quadrat = "hand_search",
             splashing = "splashing",
             sweep = "sweep",
             `timed dig` = "excavation",
             `timed hand collection` = "hand_search",
             `timed hand search` = "hand_search",
             `water trap` = "water_trap"
             ))

table(tolower(ers_data2$Sample_Type))
```

```
## 
##      ad-hoc     aquatic  excavation hand_search     pitfall   splashing 
##         152          69         987        3315        3283           2 
##       sweep     unknown  water_trap 
##         129         111           1
```




```r
table(tolower(ers_data2$Sample_Type_old))
```

```
## 
##          20 minute dig 20 minute timed search 30 minute timed sample 
##                     34                    135                    111 
##                aquatic          casual record             excavation 
##                     69                     88                    535 
##      field observation        hand collection            hand search 
##                     64                     37                   1217 
##    hand search (25min)         hand searching             handsearch 
##                   1242                    157                    277 
##  handsearch/excavation                netting                pitfall 
##                    405                      3                   1018 
##           pitfall trap               pitfalls                quadrat 
##                   2076                    189                    130 
##              splashing                  sweep              timed dig 
##                      2                    126                     13 
##  timed hand collection      timed hand search             water trap 
##                     76                     44                      1
```



does data type relate to sample type

```r
sample_data_types <-
  ers_data2 %>%
    group_by(Sample_Type) %>%
    count("abund_character" = is.na(abund_char)==FALSE, 
        "abund_number" = is.na(abund_num)==FALSE) %>% 
    filter(abund_character == TRUE | abund_number == TRUE) 

sample_data_types$abund_character[which(sample_data_types$abund_character==TRUE)] <- 
  sample_data_types$n[which(sample_data_types$abund_character==TRUE)]
  
sample_data_types$abund_number[which(sample_data_types$abund_number==TRUE)] <- 
  sample_data_types$n[which(sample_data_types$abund_number==TRUE)]
  
#sample_data_types$n <- NULL

knitr::kable(sample_data_types)
```



|Sample_Type | abund_character| abund_number|    n|
|:-----------|---------------:|------------:|----:|
|excavation  |               0|          409|  409|
|excavation  |              85|            0|   85|
|hand_search |               0|         1710| 1710|
|hand_search |              83|            0|   83|
|ad-hoc      |              64|            0|   64|
|sweep       |               0|            2|    2|
|sweep       |               1|            0|    1|
|pitfall     |               0|         1676| 1676|
|pitfall     |             319|            0|  319|
|splashing   |               0|            2|    2|
|water_trap  |               0|            1|    1|
|NA          |               0|            1|    1|


##Fix sample timing

```r
table(ers_data2$Sample_Type_old, ers_data2$Sample_Duration)
```

```
##                         
##                          2 weeks 80-100 80-100 minutes
##   20 minute dig                0      0              0
##   20 minute timed search       0      0              0
##   30 minute timed sample       0      0              0
##   Aquatic                      0      0              0
##   Casual record                0      0              0
##   excavation                   0      0              0
##   Excavation                   0      0              0
##   Field Observation            0      0              0
##   hand collection              0      0              0
##   Hand search                  0      0              0
##   Hand Search (25min)          0     12            317
##   Hand searching               0      0              0
##   Hand Searching               0      0              0
##   handsearch                   0      0              0
##   Handsearch                   0      0              0
##   handsearch/excavation        0      0              0
##   Netting                      0      0              0
##   pitfall                      0      0              0
##   Pitfall                      0      0              0
##   Pitfall trap                 0      0              0
##   Pitfall Trap               296      0              0
##   pitfalls                     0      0              0
##   Pitfalls                     0      0              0
##   Quadrat                      0      0              0
##   Splashing                    0      0              0
##   Sweep                        0      0              0
##   timed dig                    0      0              0
##   timed hand collection        0      0              0
##   timed hand search            0      0              0
##   Water Trap                   0      0              0
```


```r
ers_data2$Sample_Duration[which(ers_data2$Sample_Type_old %in% c("20 minute dig", "20 minute timed search"))] <- "20 min"

ers_data2$Sample_Duration[which(ers_data2$Sample_Type_old %in% c("30 minute timed sample"))] <- "30 min"

ers_data2 <- ers_data2 %>% 
  mutate(Sample_Duration = recode_factor(Sample_Duration,
                                         `80-100`  = "80-100 min",
                                         `80-100 minutes` = "80-100 min"))




table(ers_data2$Sample_Type_old, ers_data2$Sample_Duration)
```

```
##                         
##                          80-100 min 20 min 30 min 2 weeks
##   20 minute dig                   0     34      0       0
##   20 minute timed search          0    135      0       0
##   30 minute timed sample          0      0    111       0
##   Aquatic                         0      0      0       0
##   Casual record                   0      0      0       0
##   excavation                      0      0      0       0
##   Excavation                      0      0      0       0
##   Field Observation               0      0      0       0
##   hand collection                 0      0      0       0
##   Hand search                     0      0      0       0
##   Hand Search (25min)           329      0      0       0
##   Hand searching                  0      0      0       0
##   Hand Searching                  0      0      0       0
##   handsearch                      0      0      0       0
##   Handsearch                      0      0      0       0
##   handsearch/excavation           0      0      0       0
##   Netting                         0      0      0       0
##   pitfall                         0      0      0       0
##   Pitfall                         0      0      0       0
##   Pitfall trap                    0      0      0       0
##   Pitfall Trap                    0      0      0     296
##   pitfalls                        0      0      0       0
##   Pitfalls                        0      0      0       0
##   Quadrat                         0      0      0       0
##   Splashing                       0      0      0       0
##   Sweep                           0      0      0       0
##   timed dig                       0      0      0       0
##   timed hand collection           0      0      0       0
##   timed hand search               0      0      0       0
##   Water Trap                      0      0      0       0
```


## Look at surveys



```r
names(ers_data2)
```

```
##  [1] "Species_name"            "Taxonomic_Order"        
##  [3] "Taxonomic_Family"        "Survey/Report_Name"     
##  [5] "Author/Recorder"         "Sample River"           
##  [7] "County"                  "VC name"                
##  [9] "VC number"               "Country"                
## [11] "Substrate"               "NE Full Sample Name"    
## [13] "Location_Name"           "Sample_Grid_Ref"        
## [15] "Date"                    "Year"                   
## [17] "Sample_Type"             "Sadler & Bell method?"  
## [19] "Handsearch & Pitfall"    "Handserach & excavation"
## [21] "Sample_Duration"         "Abundance_Data"         
## [23] "abund_num"               "abund_char"             
## [25] "Sample_Type_old"
```

```r
table(ers_data2$`NE Full Sample Name`)
```

```
## 
##                 1997 Ashop        Allen 1987 POSSIBLE 
##                         56                         61 
##                Alport 1999               Beamish 2013 
##                         75                        127 
##                 Camel 1999                 Camel 2003 
##                         51                         85 
##            Camel tributary               Carey 1 2003 
##                         26                         20 
##            Creedy Yeo 1999     Dane East of Congleton 
##                         78                         55 
##     Dane West of Congleton                  Dove 2013 
##                        106                        158 
##    Eden Read POSSIBLE 2000    Eggleston Burn POSSIBLE 
##                         64                         84 
##                  Erme 1999                   Exe 1999 
##                         57                         73 
##                   Fal 2002               Fowey 1 2003 
##                         19                         22 
##               Fowey 2 2003         Gauntless POSSIBLE 
##                         25                         29 
##           Harwood POSSIBLE                Lugg 2014 1 
##                         50                        343 
##                Lugg 2014 2                Lugg 2014 3 
##                        127                        257 
##                Lugg 2014 4                Lynher 2002 
##                        275                         13 
##            Mawddach 1 2003                Monnow 2015 
##                         36                        184 
##                 Otter 1999                Ottery 2003 
##                         73                         44 
##                  Plym 2002             Rheidol 1 2004 
##                         28                        112 
##             Rheidol 2 2004             Rheidol 3 2004 
##                         57                         72 
##   River Frome Moreton 2017   River Frome Rushton 2017 
##                        219                        208 
## River Frome Woodsford 2017            River Tame 2000 
##                        142                         31 
##      Severn Llandinam 1999               Severn1 2002 
##                        148                        160 
##              Severn1a 2002               Severn2 2002 
##                        161                        110 
##               Severn2 2003              Severn2a 2002 
##                        100                        152 
##               Severn3 2003        South Tyne POSSIBLE 
##                         42                         50 
##                 Tamar 2002                   Taw 1999 
##                         27                         34 
##                Tees 1 2008                Tees 2 2008 
##                         58                         94 
##       Teign & Bovey 1998 T       Teign & Bovey B 1998 
##                        105                         97 
##                 Teign 1999         Teme 1998 POSSIBLE 
##                         93                         44 
##              Thrushel 1999                  Till 2005 
##                         46                        102 
##                  Till 2013              Torridge 1999 
##                        162                         57 
##                Towy 2 2003                Towy 4 2002 
##                        104                        176 
##                Towy 5 2002                 Towy1 2003 
##                         59                        176 
##                   Towy1998                 Towy3 2003 
##                         66                        133 
##                 Towy6 2002                   Tywi1998 
##                         99                         67 
##                    Usk1998              Waer POSSIBLE 
##                        119                         45 
##                   Wear2005          Wooler Water 2013 
##                         97                         93 
##                   Wye 2014                 Wye 2014 3 
##                        106                        222 
##                 Wye 2014 4                 Wye 2014 5 
##                        246                         92 
##                 Wye 2014 6                 Wye 2014 7 
##                        318                        226 
##                    Wye1998                 Yarty 1999 
##                        101                         63 
##                 Yealm 2003 
##                         14
```


#Selected Values

```r
sel_values
```

```
## # A tibble: 79 x 9
##    `NE Sample`    Effort `Wetland No` `Running Water N~ `Running Water SQ~
##    <chr>          <chr>  <chr>        <chr>             <chr>             
##  1 Rheidol 3 2004 5      30           22                332               
##  2 Towy 1998      8      26           26                331               
##  3 Tywi 1998      7      23           23                322               
##  4 Towy 3 2003    10     30           28                314               
##  5 Towy 4 2002    8      36           36                311               
##  6 Towy 6 2003    9      30           30                310               
##  7 Towy 1 2003    6      32           31                303               
##  8 Wye 1998       9      36           30                297               
##  9 Severn 1 2002  12     31           30                297               
## 10 Usk 1998       13     37           33                294               
## # ... with 69 more rows, and 4 more variables: `Running Water SQS` <chr>,
## #   `Marsh No` <chr>, `Marsh SQI` <chr>, `Marsh SQS` <chr>
```


