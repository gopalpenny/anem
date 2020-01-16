# anem
2d steady state groundwater flow using the analytical element method

## Overview

This package was created to evaluate the hydraulic relationships among wells, in order to estimate the effect of a group of wells on drawdown at these wells or other wells. It it based on method of images from the analytical element modeling approach, in which the 2-dimensional characteristics of aquifers are reproduced by strategically placing wells within the domain.

This package models *simple* aquifer and well configurations. The boundaries of the aquifer can be specified as *no flow* or *constant head* boundaries, and the corners of the aquifer must be right angles. For fully bounded aquifers, this means that the aquifer must be a rectangle. The constant head boundaries take the head of the undisturbed aquifer, h0.

## Installation

You can install directly from github using `devtools`:

```
devtools::install_github("https://github.com/gopalpenny/anem")
```

## Usage

Use of the package is further described in the [package vignette](github.com)

## Shiny app

The basic functionality of the package is also implemented in a Shiny application: <https://gopalpenny.shinyapps.io/anem-app/>
