library( tidyverse)
library( remotes)
library( htmlwidgets)


##
## globejs approach
##


# install.packages("remotes")
#remotes::install_github("JohnCoene/globe4r")
library(globe4r)

fabs <- read_csv( "fabs globe widget.csv") |> 
  mutate( `Widget Label` = paste0( Company, "<br>", `Fab Name`))


g <- create_globe() %>%
  bars_data(fabs) %>%          # supply your data
  bars_lat("Latitude") %>%
  bars_lon("Longitude") %>%
  bars_label("Widget Label") %>%     # <-- hover tooltip text
  bars_radius(0.1) |> 
  bars_altitude(0.1)  |> 
  globe_title( "Global Fabs") # small “pin” height
g

saveWidget(g, "globe.html", selfcontained = TRUE)



##
## globejs approach
##


# install.packages("threejs")  # if needed
library(threejs)

data <- read_csv( "fabs globe widget.csv")

w <- globejs(
  lat  = data$Latitude,
  long = data$Longitude,
  value = 42,   # bar height; use your own values if you have them
  pointsize = 0.6,
  color = "#ff3b30",
  atmosphere = TRUE
)

# Save/share as a single HTML file
htmlwidgets::saveWidget(w, "globe.html", selfcontained = TRUE)




