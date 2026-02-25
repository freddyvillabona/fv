
names <- c(
  "Hantavirus", "Tularemia", "Dengue", "Ebola", "E. coli", 
  "Tuberculosis", "Salmonella", "Vaccinia", "Brucella"
)


# Name is an ordered factor. We do this to ensure the bars are sorted.
data <- data.frame(
  count = c(6, 7, 7, 9, 11, 15, 17, 18, 54), 
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)


# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

plt <- ggplot(data) +
  geom_col(aes(count, name), fill = BLUE, width = 0.6) 

plt


# Name is an ordered factor. We do this to ensure the bars are sorted.
data <- data.frame(
  count = c(6, 7, 7, 9, 11, 15, 17, 18, 54), 
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)



# Load packages
library(tidyverse)
library(fuzzyjoin)
library(ggstream)
library(colorspace)
library(ggtext)
library(cowplot)


theme_set(theme_minimal(base_family = "Reem Kufi", base_size = 12))

theme_update(
  plot.title = element_text(
    size = 25,
    face = "bold",
    hjust = .5,
    margin = margin(10, 0, 30, 0)
  ),
  plot.caption = element_text(
    size = 9,
    color = "grey40",
    hjust = .5,
    margin = margin(20, 0, 5, 0)
  ),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  plot.background = element_rect(fill = "grey88", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  strip.text.y = element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 9, color = "grey40"),
  legend.box.margin = margin(t = 30), 
  legend.background = element_rect(
    color = "grey40", 
    size = .3, 
    fill = "grey95"
  ),
  legend.key.height = unit(.25, "lines"),
  legend.key.width = unit(2.5, "lines"),
  plot.margin = margin(rep(20, 4))
)

df_char_vis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')


df_best_chars <- tibble(
  rank = 1:10,
  char_popular = c("Wolverine", "Magneto", 
                   "Nightcrawler", "Gambit",
                   "Storm", "Colossus",
                   "Phoenix", "Professor X",
                   "Iceman", "Rogue")
)



df_smooth <- df_best_chars %>%
  group_by(character, char_popular, costume, rank) %>%
  slice(1:4) %>%
  mutate(
    issue = c(
      min(df_best_chars$issue) - 20,
      min(df_best_chars$issue) - 5,
      max(df_best_chars$issue) + 5,
      max(df_best_chars$issue) + 20
    ),
    speech = c(0, .001, .001, 0),
    thought = c(0, .001, .001, 0),
    narrative = c(0, .001, .001, 0),
    depicted = c(0, .001, .001, 0)
  )


## factor levels for type of appearance
levels <- c("depicted", "speech", "thought", "narrative")


## factorized data in long format
df_best_stream_fct <- df_best_stream %>% 
  bind_rows(df_smooth) %>%
  mutate(
    costume = if_else(costume == "Costume", "costumed", "casual"),
    char_costume = if_else(
      char_popular == "Storm",
      glue::glue("{char_popular} ({costume})"),
      glue::glue("{char_popular} ({costume})   ")
    ),
    char_costume = fct_reorder(char_costume, rank)
  ) %>% 
  pivot_longer(
    cols = speech:depicted,
    names_to = "parameter",
    values_to = "value"
  ) %>% 
  mutate(parameter = factor(parameter, levels = levels))


# Define the color palette
pal <- c(
  "#FFB400", lighten("#FFB400", .25, space = "HLS"),
  "#C20008", lighten("#C20008", .2, space = "HLS"),
  "#13AFEF", lighten("#13AFEF", .25, space = "HLS"),
  "#8E038E", lighten("#8E038E", .2, space = "HLS"),
  "#595A52", lighten("#595A52", .15, space = "HLS")
)

# These are going to be labels added to each panel
labels <- tibble(
  issue = 78,
  value = c(-21, -19, -14, -11),
  parameter = factor(levels, levels = levels),
  label = c("Depicted", "Speech\nBubbles", "Thought\nBubbles", "Narrative\nStatements")
)

# These are going to be the text annotations
# If you wonder about the '**' or the '<sup>' within the text, let me tell you
# this is just Markdown syntax used by the ggtext library to make custom text
# annotations very easy!
texts <- tibble(
  issue = c(295, 80, 245, 127, 196),
  value = c(-35, 35, 30, 57, 55),
  parameter = c("depicted", "depicted", "thought", "speech", "speech"),
  text = c(
    '**Gambit** was introduced for the first time in issue #266 called "Gambit: Out of the Frying Pan"— nevertheless, he is the **4<sup>th</sup> most popular X-Men character**!',
    '**Wolverine is the most popular X-Men** and has a regular presence in the X-Men comics between 1975 and 1991.',
    '**Storm** is by far the most thoughtful of the five most popular X-Men characters, especially in issues #220, #223 and #265. Storm **ranks 5<sup>th</sup>**.',
    "**Magneto** was ranked by IGN as the *Greatest Comic Book Villain of All Time*. And even though he only appears from time to time he **ranks 2<sup>nd</sup>**—<br>4 ranks higher than his friend and opponent Professor X!",
    'The **3<sup>rd</sup> most popular X-men character Nightcrawler** gets injured during the "Mutant Massacre"  and fell into a coma after an attack from Riptide in issue #211.'
  ),
  char_popular = c("Gambit", "Wolverine", "Storm", "Magneto", "Nightcrawler"),
  costume = "costumed",
  vjust = c(.5, .5, .4, .36, .38)
) %>% 
  mutate(
    parameter = factor(parameter, levels = levels),
    char_costume = if_else(
      char_popular == "Storm",
      glue::glue("{char_popular} ({costume})"),
      glue::glue("{char_popular} ({costume})   ")
    ),
    char_costume = factor(char_costume, levels = levels(df_best_stream_fct$char_costume))
  )


g <- df_best_stream_fct %>% 
  ggplot(
    aes(
      issue, value, 
      color = char_costume, 
      fill = char_costume
    )
    
  ) +
  geom_stream(
    geom = "contour",
    color = "white",
    size = 1.25,
    bw = .45 # Controls smoothness
  ) +
  geom_stream(
    geom = "polygon",
    bw = .45,
    size = 0
  ) +
  scale_color_manual(
    expand = c(0, 0),
    values = pal,
    guide = "none"
  ) +
  scale_fill_manual(
    values = pal,
    name = NULL
  ) +
  facet_grid( ## needs facet_grid for space argument
    parameter ~ ., 
    scales = "free_y", 
    space = "free"
  )

g
