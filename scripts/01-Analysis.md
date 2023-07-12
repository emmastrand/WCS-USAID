WCS USAID Project Analysis
================
Author: Emma Strand; <https://linktr.ee/emmastrand>

## <a name="libraries"></a> **Load all libraries**

``` r
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(Hmisc)
library(writexl)
library(naniar)
```

## Import dataset

``` r
## when running future iterations of raw data file, replace the file name below 
data <- read_excel("data/LengthData_20230712.xlsx", sheet = "Clean_data",
                                col_types = c("date", "text", "text", "text", "text", 
                                              "text", "text", "numeric", "numeric", "text",
                                              "text", "numeric", "numeric", "numeric")) %>%
  dplyr::rename(Weight_kg = `Weight(Kg)`) %>% dplyr::rename(Weight_g = `Weight( Grams`)

nrow(data) ## 957 20230712 download from google drive 
```

    ## [1] 957

### Cleaning dataset

**Month and Year**

``` r
#unique(data$Month) 
## "Sept"      "September" "Oct"       "October"   "August"    "November" 

## changing all Sept and Oct entries to read September and October 
## $ indicates end of a phrase; otherwise all 'Sept' in 'September' would also be changed 
data <- data %>%
  mutate(Month = gsub("Sept$", "September", Month),
         Month = gsub("Oct$", "October", Month))

unique(data$Month) 
```

    ## [1] "September" "October"   "August"    "November"

``` r
## end result 
# "September" "October"   "August"    "November" 

unique(data$Year) ## all 2022 
```

    ## [1] "2022"

**Landing site**

``` r
unique(data$`Landing Site`)
```

    ## [1] "Vanga"    "Shimoni"  "Mkwiro"   "Kibuyuni" "Wasini"

**Date Collector**

``` r
unique(data$`Data Collector`)
```

    ## [1] "Sofia Saidi"     "Roselyne Mwakio" "Roselyn Mwakio"

**Group**

``` r
unique(data$Group)
```

    ## [1] "1" "4" "2" "3" "5" "6" "7" "8"

**Fishing Gear**

``` r
data <- data %>%
  mutate(Fishgear = gsub("SG/HS", "HS/SG", Fishgear),
         Fishgear = gsub("ringnet", "Ring net", Fishgear),
         Fishgear = gsub("reefseine", "Reef seine", Fishgear),
         Fishgear = gsub("Reefseine", "Reef seine", Fishgear),
         Fishgear = gsub("Ringnet", "Ring net", Fishgear),
         Fishgear = gsub("Reef net", "Reef Net", Fishgear),
         Fishgear = gsub("speargun", "Speargun", Fishgear),
         Fishgear = gsub("reefsen", "Reef seine", Fishgear),
         Fishgear = gsub("Handline", "Hand line", Fishgear),
         Fishgear = gsub("monofilament", "Monofilament", Fishgear))

unique(data$Fishgear)
```

    ##  [1] "Reef seine"        "Speargun"          "HS/SG"            
    ##  [4] "Malema"            "Basket trap"       "Ring net"         
    ##  [7] "Traps"             "Hand line"         "Gillnet"          
    ## [10] "Reef Net"          "Traps & handlines" "Monofilament"

**\# Boats and Fishers**

``` r
range(data$`# Boats`) ##0-2
```

    ## [1] 0 2

``` r
hist(data$`#Fishers`) ##1-35
```

![](01-Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
hist(data$Length) ## 7.6 - 61.5 
```

![](01-Analysis_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
hist(data$Weight_kg) ## 0 - 1.113
```

![](01-Analysis_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
data <- data %>%
  ## changing all NA's to zero in Weight_g column 
  mutate(Weight_g = coalesce(Weight_g, 0)) 

hist(data$Weight_g) ## 0 - 1113 
```

![](01-Analysis_files/figure-gfm/unnamed-chunk-8-4.png)<!-- --> **Fish
ID**

``` r
unique(data$Family)
```

    ##  [1] "Acanthuridae"    "Apogonidae"      "Aulostomidae"    "Belonidae"      
    ##  [5] "Caesionidae"     "Carangidae"      "Chaetodontidae"  "Dasyatidae"     
    ##  [9] "Dorosomatidae"   "Engraulidae"     "Ephippidae"      "Epinephelidae"  
    ## [13] "Gerreidae"       "Haemulidae"      "Hemiramphidae"   "Holocentridae"  
    ## [17] "Labridae"        "Lethrinidae"     "Lutjanidae"      "Monodactylidae" 
    ## [21] "Mullidae"        "Nemipteridae"    "Platycephalidae" "Pomacentridae"  
    ## [25] "Priacanthidae"   "Scaridae"        "Scombridae"      "Serranidae"     
    ## [29] "Siganidae"       "Sillaginidae"    "Sphyraenidae"    "Synodontidae"   
    ## [33] "Terapontidae"

``` r
unique(data$Species)
```

    ##   [1] "Acanthurus dussumieri"       "Acanthurus nigricauda"      
    ##   [3] "Ctenochaetus striatus"       "Naso annulatus"             
    ##   [5] "Naso hexacanthus"            "Cheilodipterus arabicus"    
    ##   [7] "Taeniamia mozambiquensis"    "Aulostomus chinensis"       
    ##   [9] "Strongylura leiura"          "Tylosurus crocodilus"       
    ##  [11] "Pterocaesio chrysozona"      "Alepes djedaba"             
    ##  [13] "Atule mate"                  "Carangoides ciliarius"      
    ##  [15] "Carangoides gymnosthethus"   "Carangoides malabaricus"    
    ##  [17] "Decapterus kurroides"        "Rastrelliger kanagurta"     
    ##  [19] "Scomberoides tol"            "Chaetodon auriga"           
    ##  [21] "Taeniura lymma"              "Sardinella albella"         
    ##  [23] "Sardinella longiceps"        "Sardinella neglecta"        
    ##  [25] "Encrasicholina heteroloba"   "Platax teira"               
    ##  [27] "Cephalopholis boenak"        "Gerres longirostris"        
    ##  [29] "Gerres oblongus"             "Gerres oyena"               
    ##  [31] "Plectorhinchus centurio"     "Plectorhinchus chubbi"      
    ##  [33] "Plectorhinchus gaterinus"    "Plectorhinchus schotaf"     
    ##  [35] "Plectorhinchus sordidus"     "Plectorhinchus vittatus"    
    ##  [37] "Hemiramphus far"             "Hemiramphus marginatus"     
    ##  [39] "Hyporhamphus affinis"        "Hyporhamphus dussumieri"    
    ##  [41] "Myripristis berndti"         "Sargocentron caudimaculatum"
    ##  [43] "Sargocentron praslin"        "Anampses caeruleopunctatus" 
    ##  [45] "Cheilinus trilobatus"        "Cheilio inermis"            
    ##  [47] "Gomphosus caeruleus"         "Halichoeres hortulanus"     
    ##  [49] "Lethrinus borbonicus"        "Lethrinus harak"            
    ##  [51] "Lethrinus lentjan"           "Lethrinus mahsena"          
    ##  [53] "Lethrinus microdon"          "Lethrinus nebulosus"        
    ##  [55] "Lethrinus obsoletus"         "Lethrinus olivaceus"        
    ##  [57] "Lethrinus rubrioperculatus"  "Lethrinus variegatus"       
    ##  [59] "Monotaxis grandoculis"       "Lutjanus argentimaculatus"  
    ##  [61] "Lutjanus fulviflamma"        "Lutjanus kasmira"           
    ##  [63] "Lutjanus lutjanus"           "Monodactylus argenteus"     
    ##  [65] "Mulloidichthys vanicolensis" "Parupeneus barberinus"      
    ##  [67] "Parupeneus cyclostomus"      "Parupeneus heptacanthus"    
    ##  [69] "Parupeneus indicus"          "Parupeneus macronemus"      
    ##  [71] "Upeneus margarethae"         "Upeneus saiab"              
    ##  [73] "Upeneus sulphureus"          "Upeneus vittatus"           
    ##  [75] "Scolopsis bimaculata"        "Scolopsis ghanam"           
    ##  [77] "Papilloculiceps longiceps"   "Abudefduf vaigiensis"       
    ##  [79] "Priacanthus hamrur"          "Calotomus carolinus"        
    ##  [81] "Leptoscarus vaigiensis"      "Scarus ghobban"             
    ##  [83] "Scarus globiceps"            "Scarus psittacus"           
    ##  [85] "Scarus rubroviolaceus"       "Euthynnus affinis"          
    ##  [87] "Scomberomorus commerson"     "Cephalopholis spiloparaea"  
    ##  [89] "Epinephelus merra"           "Epinephelus rivulatus"      
    ##  [91] "Epinephelus spilotoceps"     "Epinephelus tauvina"        
    ##  [93] "Siganus argenteus"           "Siganus canaliculatus"      
    ##  [95] "Siganus stellatus"           "Siganus sutor"              
    ##  [97] "Sillago sihama"              "Sphyraena barracuda"        
    ##  [99] "Sphyraena flavicauda"        "Sphyraena obtusata"         
    ## [101] "Saurida gracilis"            "Terapon jarbua"

This dataset was pretty clean prior to me starting analyses!

Exporting my clean set

``` r
data %>% write_xlsx("data/LengthData_20230712_EScleaned.xlsx")
```

## Length based summary
