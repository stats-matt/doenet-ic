irt_data <-
  events_clean %>%
  dplyr::select(userId, version, credit.prob.1, credit.prob.2, credit.prob.3)

p1 <-
  irt_data %>%
  filter(!is.na(credit.prob.1)) %>%
  dplyr::select(-credit.prob.2, -credit.prob.3) %>%
  distinct() %>%
  slice_max(n = 1, order_by = credit.prob.1)

p2 <-
  irt_data %>%
  filter(!is.na(credit.prob.2)) %>%
  dplyr::select(-credit.prob.1, -credit.prob.3) %>%
  distinct() %>%
  slice_max(n = 1, order_by = credit.prob.2)

p3 <-
  irt_data %>%
  filter(!is.na(credit.prob.3)) %>%
  dplyr::select(-credit.prob.1, -credit.prob.2) %>%
  distinct() %>%
  slice_max(n = 1, order_by = credit.prob.3)

irt_data <-
  left_join(p1, p2) %>%
  left_join(p3)

irt_data <-
  irt_data %>%
  mutate(
    p1 =
      case_when(credit.prob.1 >= 0.5 ~ 1,
                TRUE ~ 0),
    p2 =
      case_when(credit.prob.2 >= 0.5 ~ 1,
                TRUE ~ 0),
    p3 =
      case_when(credit.prob.3 >= 0.5 ~ 1,
                TRUE ~ 0)
  ) %>%
  ungroup() %>%
  dplyr::select(p1, p2, p3)



mod.rasch <-
  rasch(irt_data, constraint = cbind(ncol(irt_data) + 1, 1))
plot(mod.rasch)

summary(mod.rasch)
coef(mod.rasch, prob = TRUE, order = TRUE)

mod.1pl <- rasch(irt_data)
plot(mod.1pl)

summary(mod.1pl)
coef(mod.1pl, prop = TRUE, order = TRUE)

mod.2pl <- ltm(irt_data ~ z1)
plot(mod.2pl)

summary(mod.2pl)
coef(mod.2pl, prob = TRUE, order = TRUE)

mod.3pl <- tpm(irt_data)
plot(mod.3pl)
summary(mod.3pl)
coef(mod.3pl, prob = TRUE)

factor.scores(mod.rasch)
factor.scores(mod.1pl)
factor.scores(mod.2pl)
