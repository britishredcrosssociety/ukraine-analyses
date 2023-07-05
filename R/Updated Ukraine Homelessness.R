library(tidyverse)

view(homelessness_total)
OnlyN <- homelessness_total %>%
  rename('Homeless' = "Total Ukrainian households owed a prevention or relief duty") %>% 
  select(Date,`Homeless`,`Household with dependent children (total)`)

OnlyN |>
  ggplot(aes(Date, Homeless)) +
  geom_line(aes(colour = 'red'), show.legend = FALSE) +
  geom_point(aes(size = Homeless, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Homeless)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Ukrainian Households at Risk or Experiencing Homelessness', 
       x = 'June 3, 2022 - June 16, 2023', 
       y = 'Number of Households', 
       caption = 'BRC Analysis of DLUHC Data as of June 2023. Data collected since 24 Feb 2022')

ggsave("images/total homelessness.png", width = 500, height = 500, units = "mm")





----#Calculating Growth Rate from OnlyN#----
conflicted::conflicts_prefer(dplyr::lag)

OnlyN <- OnlyN |> 
  select(Date, Homeless) |> 
  arrange(Date) |> 
  mutate(`GRTotalN` = ((Homeless - lag(Homeless)) / lag(Homeless)*100)) 

OnlyN |>
  select(Date,Homeless) |>
  arrange(Date) |>
  mutate('GRLeadTN' = (Homeless -lead(Homeless))/lead(Homeless)*100)

----#Graph of Growth Rate/ Change per Month#----
OnlyN |>
  ggplot(aes(Date, GRTotalN)) +
  geom_line(aes(colour = 'red'), show.legend = NULL) +
  geom_point(aes(size = 1, alpha = 0.5, colour = 'red'), show.legend = NULL) +
  geom_text(aes(label = scales::comma(GRTotalN)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Month-by-Month % Change in Homelessness Among Ukrainian Households Arrived in the UK', x = 'June 3 2022 - June 16 2023', y = 'Percentage', caption = 'Data collected since 24 Feb 2022')


----#Kids Calculation#----
Kidsinhouse <- homelessness_total %>%
  rename('KidsinhomeN' = "Household with dependent children (total)") %>%
  select(Date, 'KidsinhomeN')

----#Kids Graph#----
Kidsinhouse |>
  ggplot(aes(Date, KidsinhomeN)) +
  geom_line(aes(colour = 'red'), show.legend = FALSE) +
  geom_point(aes(size = KidsinhomeN, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(KidsinhomeN)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Ukrainian Households with Dependent Children at Risk or Experiencing Homelessness', 
       x = 'June 3, 2022 - June 16, 2023', 
       y = 'Number of Households', 
       caption = 'BRC Analysis of DLUHC Data as of June 2023. Data collected from 24 Feb 2022')

ggsave("images/total homelessness kids.png", width = 250, height = 150, units = "mm")


----#Kids % Graph#----
Kidspercent <- homelessness_total %>%
  rename(Householdkidpercent = "Household with dependent children (%)") %>%
  select(Date, Householdkidpercent)

Kidspercent |>
  mutate(Householdkidpercent2 = Householdkidpercent * 100) |>
  ggplot(aes(Date, Householdkidpercent2)) +
  geom_line(aes(colour = 'red'), show.legend = FALSE) +
  geom_point(aes(size = Householdkidpercent2, alpha = 0.4, colour = 'red'), show.legend = FALSE) +
  geom_text(aes(label = scales::comma(Householdkidpercent2)), show.legend = FALSE, size = rel(4)) +
  theme_classic() +
  labs(title = 'Ukrainian Households with Dependent Children (%) at Risk or Experiencing Homelessness', 
       x = 'June 3, 2022 - June 16, 2023', 
       y = 'Percentage of Households with Dependent Children (%)', 
       caption = 'BRC Analysis of DLUHC Data as of June 2023. Data collected from 24 Feb 2022')


----#Updated Map for BRC#----
library(tidyverse)
library(demographr)
library(geographr)
library(viridis)
library(sf)

homelessness_latest <- 
  homelessness_24feb_16jun |> 
  select(lad_code, homeless = `Total Ukrainian households owed a prevention or relief duty`) |> 
  left_join(demographr::households21_ltla21, by = c("lad_code" = "ltla21_code")) |> 
  mutate(homeless_per_100000 = homeless / households * 100000)


homelessness_latest_shp <- 
  geographr::boundaries_ltla21 |> 
  filter(str_detect(ltla21_code, "^E")) |> 
  left_join(homelessness_latest, by = c("ltla21_code" = "lad_code"))

# ---- Map showing absolute numbers of Ukraine homelessness by Local Authority ----

homelessness_latest_shp |> 
  ggplot() +
  geom_sf(aes(fill = homeless),colour = "#5c747a") +
  #geom_sf_label(aes(check_overlap = TRUE), size = 1, colour = 'black')
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
  scale_fill_viridis(na.value = "transparent", option = "magma", alpha = 0.7, begin = 0.1, end = 0.9, discrete = FALSE, direction = -1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(title = "Homelessness among Ukrainians by Local Authorities", 
       #subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and absolute numbers of homelessness. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people). Greatest number of homelessness seen in London, East and South-East England\n", 100),
       fill = "Number of Ukrainian households at risk of homelessness",
       caption = "Source: BRC Analysis of DLUHC Data as of April, 2023. Data collected since February 24, 2022")

##For the above graph, labels not being added to the regions. Ask Matt why this is and go from there. Should this be added with geom_text? How would that work for this graph?##

----#Map with Percentage#----
homelessness_latest_shp |> 
  filter(ltla21_code != "E06000053") |> 
  ggplot() +
  geom_sf(
    aes(fill = homeless_per_100000),colour = "#5c747a") +
  scale_fill_gradient(low = "#f6f6f6", high = "#ee2a24") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(
        b = 0.3,
        t = 0.2,
        l = 2,
        unit = "cm"
      )
    ),
    plot.caption = element_text(hjust = 0.5)
  ) +
  labs(title = "Homelessness among Ukrainians by Local Authorities, per 100,000 Households",
       #subtitle = str_wrap("Showing the subset of Local Authorities where people arriving on the Ukraine visa schemes are living and rates of homelessness per 100,00 households. Local Authorities are shaded by number of Ukranian arrivals (darker colours mean more people).\n", 100),
       fill = "Proportion of Ukrainian households at risk of homelessness \n(per 100,000 households)",
       caption = "Source: British Red Cross analysis of DLUHC data as of April 2023. Data collected since February 2022")

##For this graph as well, label repel doesnt work as well?##

----#Byscheme analysis, not highlighted##----

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
    Date_text = factor(Date_text, levels = c("3 June", "1 July", "29 July", "26 August", "23 September", "21 October", "18 November", "30 December", "27 January", "24 February", "24 March", "21 April", "19 May", "16 June")),
    `% in temporary accommodation` = `Temporary Accommodation Snapshot` / `Total Ukrainian households owed a prevention or relief duty`
  )

# ---- Absolute numbers on each scheme, by reason ----
ByScheme <- (homelessness_total |> 
  select(
    Date_text, 
    `Homes for Ukraine: Accommodation not available/suitable` = `Homes for Ukraine Scheme: Accommodation not available or suitable on arrival`, 
    `Homes for Ukraine: Accommodation/arrangement broken down` = `Homes for Ukraine Scheme: Accommodation or arrangement broken down`, 
    `Homes for Ukraine: Rejected sponsor's offer` = `Homes for Ukraine Scheme: Rejected sponsors offer`,
    `Family Scheme: Accommodation/arrangement broken down` = `Family Scheme: Accommodation or arrangement broken down`,
    `Family Scheme: Accommodation not available/suitable` = `Family Scheme: Accommodation not available or suitable on arrival`
  ) |> 
  pivot_longer(cols = -Date_text, names_to = "Scheme", values_to = "n") |> 
  mutate(
    Date_text_short = Date_text |> 
      str_remove("ember|tember|ober|ust|ruary|uary|y$|e$|ch$|il$") |> 
      factor(levels = c("3 Jun", "1 Jul", "29 Jul", "26 Aug", "23 Sep", "21 Oct", "18 Nov", "30 Dec", "27 Jan", "24 Feb", "24 Mar", "21 Apr", "19 Mayy", "16 Jun"))
  ) |> 
  
  ggplot(aes(x = Date_text, y = n, group = Scheme)) +
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
  ))

ByScheme + theme(axis.text.x = element_text(angle = 80, vjust = 0.5, hjust=0.5))
