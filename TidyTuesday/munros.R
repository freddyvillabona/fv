tuesdata <- tidytuesdayR::tt_load(2025, week = 33)

scottish_munros <- tuesdata$scottish_munros

library(tidyverse)
library(ggridges)
library(patchwork)

munros <- scottish_munros %>%
  mutate(Type = case_when(
    str_detect(Name, "Stob") ~ "Stob",
    str_detect(Name, "Meall|Mheall|Mill|Mhill") ~ "Meall",
    str_detect(Name, "Sgurr|Sgorr|Sgor") ~ "Sgùrr",
    str_detect(Name, "Carn|Cairn|Chairn|Chuirn") ~ "Càrn",
    str_detect(Name, "Ben|Beinn|Bheinn") ~ "Beinn"),
    Type = factor(Type, levels = c("Meall", "Creag", "Sgùrr", "Beinn", "Càrn", "Stob"))
  ) %>%
  drop_na(Type) %>%
  group_by(Type)

means <- munros %>%
  summarise(height = mean(Height_m),
            number = n())

bg <- "#efe5dc"
p <- ggplot(munros, aes(Height_m, Type, fill = Type)) +
  geom_density_ridges(scale = 1.5, rel_min_height = 0.005,
                      quantile_lines = TRUE, quantile_fun = mean,
                      vline_linetype = "dashed",
                      aes(color = "Mean height (m)")) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_color_manual(name = NULL, values = c("Mean height (m)" = "black")) +
  scale_fill_brewer(palette = 9) +
  labs(x = "Height (m)", y = NULL) +
  guides(fill = "none",
         color = guide_legend(override.aes=list(fill=bg))) +
  theme(legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.position = "inside",
        legend.position.inside = c(0.8,0.09))

# combination

munros_long <- munros %>%
  pivot_longer(7:17, names_to = "Year", values_to = "Classification") %>%
  mutate(Year = as.integer(Year)) %>%
  drop_na(Classification)

p3 <- ggplot(filter(munros_long, Year == 2021)) +
  geom_bar(aes(x = Type, fill = Classification), colour = "black",
           width = 0.5) +
  labs(x = NULL, y = "Number") +
  scale_fill_manual(values = tidyplots::colors_discrete_friendly_long) +
  theme(axis.text.y = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85,0.8)) +
  coord_flip()

combined_plot <- p + p3 & theme(plot.margin = margin(40,10,10,10),
                                plot.background = element_rect(fill = bg, color = bg),
                                panel.background = element_rect(fill = bg, color = bg),
                                legend.background = element_rect(fill = bg, color = bg),
                                panel.grid = element_blank(),
                                axis.ticks = element_blank(),
                                axis.text = element_text(size = 12),
                                axis.title.x = element_text(size = 14, hjust = 1),
                                plot.title = element_text(face = "bold", size = 16, hjust = 0, vjust = 5),
                                plot.title.position = "plot") &
  plot_annotation(title = "Scotland's Munros and Munro Tops",
                  subtitle = glue::glue("A Munro is a Scottish mountain and a Munro Top is a subsidiary summit of a Munro not considered a distinct mountain in its own right.
                  The charts below show the height distributions (in metres) and the number of Munros and Munro Tops by commonly used Gaelic names.
                  The most common name used to describe a Munro is Beinn (or Ben), meaning 'mountain' or 'peak'."),
                  caption = "Created by jessimoore@bsky.social for #TidyTuesday. Source: The Database of British and Irish Hills v18.2, www.hills-database.co.uk")

combined_plot
