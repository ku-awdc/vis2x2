# vis2x2
Visualisation of two by two tables for diagnostic test evaluation

## Installation

The eaiest way is:

1. Download and install GitHub desktop from https://desktop.github.com
1. Open GitHib desktop and select "File->Clone Repository" then type ku-awdc/vis2x2 in the dialog box, then press Clone
1. Find the repository on your hard drive, and then open vis2x2.Rproj in RStudio
1. Under the "Build" tab (usually top right of your screen) press "Install and Restart"

The package should install and then you should be able to do e.g.:

    library("vis2x2")
    draw_2x2(matrix(1:4, nrow=2))
    launch_shiny("example")
  
    
## Modification

To edit code:

1.  Make sure your copy is up to date by clicking "Fetch origin" in GitHub desktop
1.  Edit whatever code you want to in RStudio (R code in the R/ directory, shiny apps in the inst/shiny/ directory)
1.  If you have changed any documentation then make sure to run "Document" (under the "More" dropdown menu in the "Build tab")
1.  Run "Install and Restart" to make sure your changes haven't completely broken the package
1.  Go back to GitHub desktop, add a summary and (optionally) description of what you changed
1.  Click commit to master
1.  Click "Push to origin"


## Testing the shiny app

Running launch_shiny() launches the shiny app in the installed version of the package, which is not necessarily the same as the GitHub repository unless you have re-installed the package since you last made a change. If you want to test small changes to a shiny app without installing the package, you can do e.g.:

    library('shiny')
    runApp(appDir="inst/shiny/example")
    
    
