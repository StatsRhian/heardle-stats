library("tidyverse")
heardle = readr::read_csv("heardle_tagged.csv", na = "-")

#heardle =
#heardle |>
#  mutate(tag = map_chr(Artist, get_top_tag))


# Winner
heardle %>%
  mutate(Rhian = replace(Rhian, Rhian == 0, 7)) %>%
  mutate(Keith = replace(Keith, Keith == 0, 7)) %>%
  mutate(result = sign(Keith - Rhian)) %>%
  filter(result != 0) %>%
  group_by(tag) %>%
  count(result) %>%
  mutate(n = n * sign(result)) %>%
  mutate(tag = stringr::str_to_title(tag)) %>%
  filter(!is.na(tag)) %>%
  ggplot(aes(x = tag, y = n, fill = as.factor(result))) +
  geom_col() +
  theme_minimal() +
  scale_fill_manual(labels = c("1" = "Rhian", "-1" = "Keith"),
                    values = c("1" = "hotpink4", "-1" = "grey20"),
                    name = emo::ji("crown")) +
  xlab("Genre") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "Comparison of winner by genre",
       subtitle = "Draws were removed")

# Correct or not?
heardle %>%
  pivot_longer(c("Rhian", "Keith"), names_to = "Name", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  mutate(win = Score != 0) %>%
  group_by(tag, Name) %>%
  count(win) %>%
  mutate(n = if_else(win, n, -n)) %>%
  mutate(tag = stringr::str_to_title(tag)) %>%
  ggplot(aes(x = tag, y = n, fill = as.factor(win))) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~Name) +
  scale_fill_manual(labels = c("TRUE" = emo::ji("check"), "FALSE" = emo::ji("x")),
                    values = c("FALSE" = "firebrick", "TRUE" = "forestgreen"),
                    name = "Success?") +
  xlab("Genre") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  labs(title = "Did you get it?",
       subtitle = "Number of guesses are discounted")

# Most failed genres

heardle %>%
  pivot_longer(c("Rhian", "Keith"), names_to = "Name", values_to = "Score") %>%
#  filter(Name == "Keith") %>%
  filter(!is.na(Score)) %>%
  mutate(win = Score != 0) %>%
  filter(win == FALSE) %>%
  group_by(tag, Name) %>%
  count(win) %>%
  mutate(tag = fct_reorder(tag, n)) %>%
  mutate(tag = stringr::str_to_title(tag)) %>%
  ggplot(aes(x = tag, y = n, fill = as.factor(win))) +
  geom_col() +
  theme_minimal() #+
  #facet_wrap(~Name)
