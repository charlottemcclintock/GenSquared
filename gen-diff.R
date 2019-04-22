
# Charlotte McClintock

# ...........................................................................................

# load libraries
library(tidyverse)
library(plotly)

# ...........................................................................................

df <- read_csv("data.csv")

p1 <- plot_ly(df, color = I("gray80")) %>%
  add_segments(x = ~mom, xend = ~daughter, y = ~country, yend = ~country, showlegend = FALSE) %>%
  add_markers(x = ~mom, y = ~country, name = "Mother", color = I("purple")) %>%
  add_markers(x = ~daughter, y = ~country, name = "Daughter", color = I("pink")) %>%
  layout(
    title = "Gender educational disparity",
    xaxis = list(title = "Mean Years of Education"),
    margin = list(l = 65)
  )
p1

p2 <- plot_ly(df, color = I("gray80")) %>%
  add_segments(x = ~dad, xend = ~son, y = ~country, yend = ~country, showlegend = FALSE) %>%
  add_markers(x = ~dad, y = ~country, name = "Father", color = I("navy")) %>%
  add_markers(x = ~son, y = ~country, name = "Son", color = I("blue")) %>%
  layout(
    title = "Gender educational disparity",
    xaxis = list(title = "Mean Years of Education"),
    margin = list(l = 65)
  )
p2

p <- subplot(p1, p2)
p


library(httr)

urlfile <- 'https://raw.githubusercontent.com/charlottemcclintock/GenSquared/master/data.csv'
df <- read.csv(urlfile)

p <- plot_ly(df, color = I("gray80")) %>%
  add_segments(x = ~mom, xend = ~daughter, y = ~country, yend = ~country, showlegend = FALSE) %>%
  add_markers(x = ~mom, y = ~country, name = "Mother", color = I("purple")) %>%
  add_markers(x = ~daughter, y = ~country, name = "Daughter", color = I("pink")) %>%
  add_segments(x = ~dad, xend = ~son, y = ~country, yend = ~country, showlegend = FALSE) %>%
  add_markers(x = ~dad, y = ~country, name = "Father", color = I("navy")) %>%
  add_markers(x = ~son, y = ~country, name = "Son", color = I("blue")) %>%
  layout(
    title = "Gender educational disparity",
    xaxis = list(title = "Mean Years of Education"),
    margin = list(l = 65)
  )
p
