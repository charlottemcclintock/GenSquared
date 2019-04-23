
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

# ...........................................................................................

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



# ...........................................................................................

df$arb <- 15

# first working with both cnum
df$cnum <- as.numeric(as.factor(df$country))
p <- plot_ly(df, color = I("gray80")) %>%
  add_segments(x = ~mom, xend = ~daughter, y = ~cnum+.2, yend = ~cnum+.2, showlegend = FALSE) %>%
  add_markers(x = ~mom, y = ~cnum+.2, name = "Mother", color = I("purple"), size=2) %>%
  add_markers(x = ~daughter, y = ~cnum+.2, name = "Daughter", color = I("pink"), size=2) %>%
  add_segments(x = ~dad, xend = ~son, y = ~cnum-.1, yend = ~cnum-.1, showlegend = FALSE) %>%
  add_markers(x = ~dad, y = ~cnum-.1, name = "Father", color = I("navy"), size=2) %>%
  add_markers(x = ~son, y = ~cnum-.1, name = "Son", color = I("blue"), size=2) %>%
  add_markers(x = ~arb, y = ~country, name = "", color = I("white"), yaxis = "y2") %>%
  layout(
    title = "Gender educational disparity",
    xaxis = list(title = "Mean Years of Education"),
    margin = list(l = 150),
    yaxis=list(title="", tickfont=list(color="white")),
    yaxis2 = list(overlaying = "y", side = "left", title = "")
  ) 
p

# ...........................................................................................

df <- read_csv("gen.csv")

p <- plot_ly(df, color = I("gray80")) %>%
  add_segments(x = ~mom, xend = ~daughter, y = ~country, yend = ~country, showlegend = FALSE) %>%
  add_markers(x = ~mom, y = ~country, name = "Mother", color = I("purple")) %>%
  add_markers(x = ~daughter, y = ~country, name = "Daughter", color = I("pink")) %>%
  add_segments(x = ~dad, xend = ~son, y = ~country, yend = ~country, showlegend = FALSE, yaxis = "y2") %>%
  add_markers(x = ~dad, y = ~country, name = "Father", color = I("navy"), yaxis = "y2") %>%
  add_markers(x = ~son, y = ~country, name = "Son", color = I("blue"), yaxis = "y2") %>%
  layout(
    title = "Gender educational disparity",
    xaxis = list(title = "Mean Years of Education"),
    margin = list(l = 150),
    yaxis=list(title="", tickfont=list(color="white")),
    yaxis2 = list(overlaying = "y", side = "left", title = "")
  ) 
p

