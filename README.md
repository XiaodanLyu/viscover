# viscover
an R package to visualize soil and crop data and their overlay

## Installation

Run the following from an R console:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("XiaodanLyu/viscover")
```

## Overview

- To run the `viscover` web tool:

```r
viscover::runTool()
```

- To fetch CDL data by coordinate or bounding box:

```r
help(GetCDLFile)
help(GetCDLValue)
```

- CDL color scheme and palette:

```r
help(cdl.dbf)
help(cdlpal)
```

- Overlay raster tile and polygon:

```r
help(TileinPoly)
```

