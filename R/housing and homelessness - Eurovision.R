library(tidyverse)
library(ggfittext)
library(ggtext)
library(IMD)

source("https://github.com/matthewgthomas/brclib/raw/master/R/colours.R")

# ---- Load data ----
# source("R/load Ukraine visa data - scraped.R")

homelessness_total <- read_csv("data/homelessness/ukraine-homelessness-summary.csv")

# homelessness_trends <- read_csv("data/homelessness/ukraine-homelessness-trends.csv")

# ---- Wrangling ----
# unique(homelessness_total$Date_text)

homelessness_total <- 
  homelessness_total |> 
  mutate(
    Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July", "26 August", "23 September", "21 October", "18 November", "30 December", "27 January", "24 February", "24 March", "21 April")),
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Total Ukrainian households owed a prevention or relief duty`
  )

# ---- Absolute numbers on each scheme, by reason ----
homelessness_total |> 
  select(
    Date_text, 
    `Homes for Ukraine: Accommodation not available/suitable` = `Homes for Ukraine Scheme: Accommodation not available or suitable on arrival`, 
    `Homes for Ukraine: Accommodation/arrangement broken down` = `Homes for Ukraine Scheme: Accommodation or arrangement broken down`, 
    `Homes for Ukraine: Rejected sponsor's offer` = `Homes for Ukraine Scheme: Rejected sponsors offer`,
    `Family Scheme: Accommodation/arrangement broken down` = `Family Scheme: Accommodation or arrangement broken down`,
    `Family Scheme: Accommodation not available/suitable` = `Family Scheme: Accommodation not available or suitable on arrival`
  ) |> 
  pivot_longer(cols = -Date_text, names_to = "Scheme", values_to = "n") |> 
  
  # mutate(
  #   label = case_when(
  #     Date_text == "26 August" & Scheme == "Homes for Ukraine Scheme: Accommodation not available or suitable on arrival" ~ Scheme,
  #     TRUE ~ NA_character_
  #   )
  # ) |> 
  
  mutate(
    Date_text_short = Date_text |> 
      str_remove("ember|tember|ober|ust|ruary|uary|y$|e$|ch$|il$") |> 
      factor(levels = c("3 Jun", "1 Jul", "29 Jul", "26 Aug", "23 Sep", "21 Oct", "18 Nov", "30 Dec", "27 Jan", "24 Feb", "24 Mar", "21 Apr"))
  ) |> 
  
  ggplot(aes(x = Date_text_short, y = n, group = Scheme)) +
  geom_line(aes(colour = Scheme), size = 1.1) +
  geom_point(aes(fill = Scheme), size = 2.5, pch = 21, colour = "white") +
  
  # geom_label(aes(label = label)) +
  
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_color_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light)) +
  scale_fill_manual(values = c(get_brc_colours()$teal, get_brc_colours()$teal_light, get_brc_colours()$red, get_brc_colours()$red_dark, get_brc_colours()$red_light)) +
  
  theme_classic() +
  guides(fill=guide_legend(nrow=5,byrow=TRUE), color = guide_legend(nrow=5,byrow=TRUE))+
  theme(
    legend.position = "bottom",
    # legend.box = "vertical",
    # legend.margin = margin(),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(size = 12)
  ) +
  labs(
    title = str_glue(
      "People arriving on the <span style='color:{get_brc_colours()$red}; font-weight:bold'>Homes for Ukraine Scheme</span> increasingly face homelessness<br/>
      <span style='font-size:10pt; color:#737373; font-weight:bold'>Number of Ukraine refugee housholds at risk of homelessness on the <span style='color:{get_brc_colours()$teal}; font-weight:bold'>Family Scheme</span> and the <span style='color:{get_brc_colours()$red}'>Homes for Ukraine Scheme</span></span>"
    ),
    x = NULL,
    y = NULL,
    caption = "British Red Cross analysis of DLUHC data"
  )

ggsave("images/homelessness - count by scheme and reason.png", width = 150, height = 150, units = "mm")

# ---- Monthly growth rates ----
homelessness_total |> 
  select(Date, Homeless = `Total Ukrainian households owed a prevention or relief duty`) |> 
  arrange(Date) |> 
  mutate(
    `% change` = (Homeless - lag(Homeless)) / lag(Homeless)
  )
