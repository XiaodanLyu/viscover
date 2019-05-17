## Installation

Run the following from an R console:

``` r
if(!require("devtools")) 
  install.packages("devtools")
devtools::install_github("XiaodanLyu/viscover")
```

## Web Scraping

This tiny package is developed to web-scrape [cropland data
layer](https://nassgeodata.gmu.edu/CropScape/) (CDL) and [soil data
layer](https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx)
(SDL) at point-level and small domain.

### Point level

``` r
library(viscover)
GetCDLValue(year = 2018, lon = -93.65, lat = 42.03)
#> $value
#> [1] "124"
#> 
#> $category
#> [1] "Developed/High Intensity"
#> 
#> $color
#> [1] "#9C9C9C"
GetSDLValue(lon = -93.65, lat = 42.03)
#> Loading required namespace: rgeos
#> single result set, returning a data.frame
#>   areasymbol musym   mukey                               muname muacres
#> 1      IA169   L55 2800480 Nicollet loam, 1 to 3 percent slopes   45662
```

### Small domain

``` r
setwd("C:/Users/lyux/")
sp::bbox(poly)
#>         min       max
#> x -93.67166 -93.65910
#> y  42.05230  42.07128
GetCDLFile(year = 2018, b = poly)
#> class       : RasterLayer 
#> dimensions  : 72, 37, 2664  (nrow, ncol, ncell)
#> resolution  : 30, 30  (x, y)
#> extent      : 191325, 192435, 2119095, 2121255  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs 
#> data source : C:\Users\lyux\Temp\tmp.tif 
#> names       : tmp 
#> values      : 0, 255  (min, max)
```

## Visualize

### Shiny app

This package embeds a shiny tool to visualize the two data layers. You
can run the tool from R console using the following

``` r
runTool()
```

Or you can find a live tool at <https://lyux.shinyapps.io/viscover/>.

Below is a video demo of using the tool.

<figure class="video_container">
<iframe src="https://player.vimeo.com/video/321794430" width="640" height="348" frameborder="0" allow="autoplay; fullscreen" allowfullscreen> </iframe>
</figure>
