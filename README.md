# CS-424_Project-01

In this project I build a ShinyApps.io dashboard (https://mzimin2.shinyapps.io/CS-424_Project-01/) to display the data from https://www.eia.gov/electricity/data/state/, to which my Professor (Dr. Andy Johnson) preprocessed the data into a CSV file for us.

This project had 3 parts:
1) Show the data in 6 different ways (2 using bar charts, 2 using line graphs, and 2 using tables) whith each pair having the data representing both the "Production Amount" and "Percent of Total Production." And, the user should be able to pick which energy source (or all sources) to view.
2) Split the screen in half so the user can compare the data by selecting the State, Energy Source, and Year for two "regions," and use the 6 methods for visualization from number 1.
3) Split the screen in half so that the user can compare using heat maps of the Energy Source and Year for 2 "regions."

---

# Cloning this repository and getting this to work on a local machine
Step 1) install Anaconda via https://www.anaconda.com/products/individual. Select the correct download for you local machine's Operating System (i.e., Linux, macOS, Windows, etc.) and follow the installation turorial to successfully install Anaconda.

Step 2) open Anaconda and click on the RStudio app box, then RStudio should open.

Step 3) To actually clone the this repository, you can either do it by downloading the repo as a .zip file then unzipping it or through your local machine's terminal by first clicking on the green code button and then copy/paste the html or ssh clone repo link into the terminal app. 

Step 4) Go back to RStudio and in the file navigation pane (bottom right) navigate to the location your un-zipped folder of this repo is. Navigate inside this folder and open the app.R file. You should be able to run it without errors, but if errors are present use the following commands:

Do this first before trying the conda option, by adding the following line above the library(...) line of code.
<br>install.packages("ggplot2")
<br>install.packages("rgeos")
<br>install.packages("ggmap")
<br>install.packages("maps")
<br>install.packages("mapdata")
<br>install.packages("maptools")
<br>install.packages("ggthemes")
<br>install.packages("sp")
<br>install.packages("stringr")
<br>install.packages("plyr")

If the above doesn't work then use your local machine's terminal and type the following commands:

<br>conda install -c conda-forge r-rgeos
<br>conda install -c conda-forge r-ggmap
<br>conda install -c conda-forge r-mapdata
<br>conda install -c conda-forge r-maptools
<br>conda install -c conda-forge r-ggthemes
<br>conda install -c r r-maps

Step 5) If you didn't have any errors or you solved the errors you had before, then you can run the app.R file by pressing the green run button near the top-right of the left half of the RStudio window.

And there you have it, this repo should be running locally on your machine and a Shiny App should be popping up in a new window after RStudio compiles it!




