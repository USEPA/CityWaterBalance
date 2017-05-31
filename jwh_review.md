---
title: CityWaterBalance package review
author: Jeff Hollister
output:
  pdf_document: default
---

# Install

No issues with install, load, or opening help.  

# README

Looks good.  Few small changes in [PR]().

# DESCRIPTION

Beef up the description field a bit.  Pretty sure you will get push back if this
goes to CRAN with description == title.  Also don't forget to bump to a first 
version and drop the .9000.  I use 0.1.0 for my first versions.

# Vignette

Made a couple of changes on the vignette itself and included in [PR]().

Some additional comments:

1. Is there a citation for this figure/model.  You should be able to include 
citations in your vignettes.  Look at 
<http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html> for 
details.  If only a few citations might be easiest to include reference: in your 
YAML.

2. Line 143, use a different object name.  Confusing to have both as `wu`.

3. Line 158, avoid names of existing functions (`data`).  Also this is throwing 
an error for me:

```
Error in try.xts(x, error = "must be either xts-coercible or timeBased") : 
  must be either xts-coercible or timeBased
```
4. Line 188-189 see above re: naming

5. General comment - In examples it may be best to name your arguments.  It is a
bit clearer and allows users to see which arguments are in use without having to
open help.

6. General comment - Make sure references to arguments, object, function names 
are wrapped in \`. It helps a lot when reading to see which words relate to the 
code itself.  I also like to inlcude the parenthesis when refering to a 
function, like: `ls()`.

# Code

For a future version, might want to think about converting plots to S3 methods (not necessary now).   At a minimum have plotWaterBalance or plotStreamflow work more seamlessly with the output of CityWaterBalance.  For instance, you currently require specification of the list item (e.g. global_flows).  If that is the item that will always be plotted, you could pull it out in the code, or if other items could be plotted as part of plotWaterBalance you could have a function that works like

```
plotWaterBalance(m, "global_flows")
```







