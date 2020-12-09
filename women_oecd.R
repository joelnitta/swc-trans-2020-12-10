library(tidyverse)
library(readxl)
library(janitor)
library(ggtext)

women_researchers <- read_excel("data/922018041p1g012.xlsx", range = "A13:D45") %>%
  clean_names() %>%
  rename(country = row_labels) %>%
  bind_rows(
    summarize(., across(contains("sector"), mean)) %>%
      mutate(country = "**OECD mean**")
  ) %>%
  rowwise() %>%
  mutate(total = sum(c_across(contains("sector")))) %>%
  ungroup %>%
  mutate(country = ifelse(country == "Japan", "**Japan**", country)) %>%
  mutate(country = factor(country) %>% fct_reorder(total)) %>%
  arrange(total) %>%
  select(-total) %>%
  pivot_longer(names_to = "sector", values_to = "percent", -country) %>%
  mutate(
    sector = str_remove_all(sector, "_sector") %>%
           str_replace_all("business_enterprise", "business") %>%
           str_replace_all("_", " ") %>%
           str_to_title(),
    percent = 0.01 * percent
  )

ggplot(women_researchers, aes(y = country, x = percent, fill = sector)) +
  labs(
    fill = "Sector",
    title = "Women Researchers",
    subtitle = "as a percentage of total researchers",
    caption = "OECD Science, Technology and Innovation Outlook (2018)\nhttps://doi.org/10.1787/sti_in_outlook-2018-en") +
  geom_col(width = 0.5, position = "dodge") +
  scale_x_continuous(label = scales::percent, expand = expansion(mult = c(0, .1))) +
  scale_fill_brewer(type = "qual", palette = 2) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_markdown(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
    )


ggsave("images/women_researchers_oecd.png")
