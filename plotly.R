library(plotly)
library(rjson)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/election.geojson'
geojson <- rjson::fromJSON(file=url)
url2<- "https://raw.githubusercontent.com/plotly/datasets/master/election.csv"
df <- read.csv(url2)
g <- list(
  fitbounds = "locations",
  visible = FALSE
)
fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=geojson,
  locations=df$district,
  z=df$Bergeron,
  colorscale="Viridis",
  featureidkey="properties.district"
)
fig <- fig %>% colorbar(title = "Bergeron Votes")
fig <- fig %>% layout(
  mapbox=list(
    style="carto-positron",
    zoom =9,
    center=list(lon=-73.7073, lat=45.5517))
)
fig
