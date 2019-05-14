
library(tidyverse)
library(WDI)
theme_set(theme_light())
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


student_teacher_ratio_2015 <- student_ratio %>%
  filter(indicator == "Primary Education",
         year == 2015,
         !is.na(student_ratio))
student_teacher_ratio_2015 %>%
  arrange(desc(student_ratio)) %>%
  slice(c(1:10, seq(dplyr::n() - 10, dplyr::n()))) %>%
  mutate(country = fct_reorder(country, student_ratio)) %>%
  ggplot(aes(country, student_ratio)) +
  geom_point() +
  coord_flip() +
  expand_limits(y = 0) +
  labs(title = "Countries with the highest and lowest student/teacher ratios",
       x = "",
       y = "Student/teacher ratio")


library(WDI)
WDIsearch("public.*education") %>%
  as.data.frame() %>%
  tbl_df() %>%
  arrange(str_length(name)) %>%
  View()

indicators_raw <- WDI(indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "SE.ADT.LITR.ZS",
                                    "SE.XPD.TOTL.GD.ZS",
                                    "SE.SEC.NENR.MA", "SE.SEC.NENR.FE"),
                      start = 2015, end = 2015, extra = TRUE) %>%
  tbl_df()
indicators <- indicators_raw %>%
  select(country_code = iso3c,
         region,
         NY.GDP.PCAP.CD:SE.SEC.NENR.FE) %>%
  mutate(country_code = as.character(country_code))
student_teacher_ratio_2015 %>%
  ggplot(aes(student_ratio)) +
  geom_histogram() +
  scale_x_log10()


student_teacher_ratio_2015 %>%
  inner_join(indicators, by = "country_code") %>%
  arrange(desc(SP.POP.TOTL)) %>%
  ggplot(aes(NY.GDP.PCAP.CD, student_ratio)) +
  geom_point(aes(size = SP.POP.TOTL, color = region)) +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(labels = scales::comma_format(), range = c(.25, 12)) +
  labs(x = "GDP per capita",
       y = "Student/teacher ratio in primary education",
       subtitle = "In 2015",
       title = "GDP per capita and student/teacher ratio are negatively correlated",
       color = "Region",
       size = "Population")


### Other indicators

student_teacher_ratio_2015 %>%
  inner_join(indicators, by = "country_code") %>%
  filter(!is.na(SE.ADT.LITR.ZS)) %>%
  mutate(literacy = SE.ADT.LITR.ZS / 100) %>%
  ggplot(aes(student_ratio, literacy)) +
  geom_point() +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::percent_format())


joined <- student_teacher_ratio_2015 %>%
  inner_join(indicators, by = "country_code") %>%
  mutate(secondary_enrollment = (SE.SEC.NENR.MA + SE.SEC.NENR.FE) / 2)
joined %>%
  arrange(desc(SP.POP.TOTL)) %>%
  ggplot(aes(NY.GDP.PCAP.CD, secondary_enrollment)) +
  geom_point() +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  labs(x = "GDP per capita",
       y = "Secondary school enrollment")

joined %>%
  arrange(desc(SP.POP.TOTL)) %>%
  ggplot(aes(student_ratio, secondary_enrollment)) +
  geom_point() +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  labs(x = "Student/teacher ratio in primary school",
       y = "Secondary school enrollment")

joined %>%
  transmute(student_ratio,
            secondary_enrollment,
            log2_gdp = log2(NY.GDP.PCAP.CD)) %>%
  cor(use = "pairwise.complete.obs")



joined %>%
  lm(secondary_enrollment ~ student_ratio + log2(NY.GDP.PCAP.CD),
     data = .) %>%
  summary()




### Appendix: Primary vs secondary education

secondary_primary_education <- student_ratio %>%
  filter(year == 2015,
         !is.na(student_ratio),
         indicator %in% c("Primary Education", "Secondary Education")) %>%
  group_by(country) %>%
  filter(dplyr::n() == 2) %>%
  ungroup()
secondary_primary_education %>%
  inner_join(indicators, by = "country_code") %>%
  arrange(desc(SP.POP.TOTL)) %>%
  ggplot(aes(NY.GDP.PCAP.CD, student_ratio)) +
  geom_point(aes(size = SP.POP.TOTL, color = region)) +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(labels = scales::comma_format(), range = c(.25, 12)) +
  facet_wrap(~ indicator) +
  labs(x = "GDP per capita",
       y = "Student/teacher ratio in primary education",
       subtitle = "In 2015",
       title = "GDP per capita and student/teacher ratio are negatively correlated",
       color = "Region",
       size = "Population")
secondary_primary_education %>%
  select(indicator, country, student_ratio) %>%
  mutate(indicator = snakecase::to_snake_case(indicator)) %>%
  spread(indicator, student_ratio) %>%
  mutate(ratio = secondary_education / primary_education) %>%
  ggplot(aes(primary_education, secondary_education)) +
  geom_point() +
  geom_text(aes(label = country), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() +
  scale_y_log10()




