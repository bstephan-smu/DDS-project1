#Map for Breweries Per State

per_state$State <- trimws(per_state$State)
per_state$hover  <- with(per_state, paste(State, '<br>', "Breweries", breweries_per_state))
fig1_2 <- plot_geo(per_state, locationmode = 'USA-states') %>%
  add_trace(
    z = ~breweries_per_state, text = ~hover, locations = ~State,
    color = ~breweries_per_state, colors = 'Purples'
  ) %>%
  colorbar(title = "Breweries Per State") %>%
  layout(
    title = 'Breweries Per State in Map<br>(Hover for breakdown)',
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

ggplotly(fig1_2)

## Export file
#Sys.setenv("plotly_username"="XXX")
#Sys.setenv("plotly_api_key"="XXX")
library(plotly)
chart_link = api_create(fig1_2, filename="choropleth-brew")
chart_link

#Map for Median IBU per State

medians$state <- trimws(medians$state)
medians$hover  <- with(medians, paste(state, '<br>', "IBU", median_ibu))
fig2_2 <- plot_geo(medians, locationmode = 'USA-states') %>%
  add_trace(
    z = ~median_ibu, text = ~hover, locations = ~state,
    color = ~median_ibu, colors = 'Greens'
  ) %>%
  colorbar(title = "Median IBU per State") %>%
  layout(
    title = 'Median IBU per State in Map<br>(Hover for breakdown)',
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

ggplotly(fig2_2)

chart_link = api_create(fig2_2, filename="choropleth-ibu")
chart_link

#Map for Median ABV per State

medians$state <- trimws(medians$state)
medians$hover  <- with(medians, paste(state, '<br>', "ABV", median_abv))
fig3_2 <- plot_geo(medians, locationmode = 'USA-states') %>%
  add_trace(
    z = ~median_abv, text = ~hover, locations = ~state,
    color = ~median_abv, colors = 'Reds'
  ) %>%
  colorbar(title = "Median ABV per State") %>%
  layout(
    title = 'Median ABV per State in Map<br>(Hover for breakdown)',
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

ggplotly(fig3_2)


chart_link = api_create(fig3_2, filename="choropleth-abv")
chart_link

