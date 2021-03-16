#+ warning=F, message=F, fig.width=10

library(tidyverse)
library(patchwork)
library(ggtext)
library(lme4)
library(lmerTest)
library(emmeans)
library(effects)
library(visreg)
library(broom)
library(broom.mixed)
library(glue)
library(hagenutils)
library(gossipdata2008)

# Functions ---------------------------------------------------------------

pos_mean <- function(x) mean(x[x>=0])
neg_mean <- function(x) -mean(x[x<0])

# Phase I rating summary --------------------------------------------------

ratings_summary <-
  ratings %>%
  dplyr::select(starts_with('Q')) %>%
  map_df(
    ~c(
      Positive_rating = pos_mean(.x),
      Negative_rating = neg_mean(.x),
      Positive_family_rating = pos_mean(.x[ratings$Condition == 'Family']),
      Negative_family_rating = neg_mean(.x[ratings$Condition == 'Family']),
      Positive_office_rating = pos_mean(.x[ratings$Condition == 'Office']),
      Negative_office_rating = neg_mean(.x[ratings$Condition == 'Office'])
    )
  ) %>%
  as_tibble(rownames = 'Question')

ratings_summary <- 
  bind_cols(questions, ratings_summary[-1]) %>% 
  mutate(
    Passed_Pos = Positive_rating > 5,
    Passed_Neg = Negative_rating < 5,
    Passed_Pos_Domain = case_when(
      Domain == 'Work' ~ Positive_office_rating > Positive_family_rating,
      Domain == 'Family' ~ Positive_family_rating > Positive_office_rating
    ),
    Passed_Neg_Domain = case_when(
      Domain == 'Work' ~ Negative_office_rating < Negative_family_rating,
      Domain == 'Family' ~ Negative_family_rating < Negative_office_rating
    ),
    Passed_all = Passed_Pos & Passed_Neg & Passed_Pos_Domain & Passed_Neg_Domain
  )

gossip <- 
  gossip %>% 
  rowwise() %>%
  mutate(
    WorkPos = pos_mean(c_across(c(Q00:Q09))),
    WorkNeg = neg_mean(c_across(c(Q00:Q09))),
    FamilyPos = pos_mean(c_across(c(Q10:Q13,Q17:Q19))),
    FamilyNeg = neg_mean(c_across(c(Q10:Q13,Q17:Q19))),
    TastePos = pos_mean(Q16),
    TasteNeg = neg_mean(Q16)
  ) %>%
  ungroup()

# Conditions
#
#    Scenario  Promotions    Reward Coalition
#
# 0. Office    one person,   small, One neighborhood friend.
# 1. Office    one person,   large, One neighborhood friend.
# 2. Office    one person,   large, Two neighborhood friends.
# 3. Office    one person,   large, One office friend.
# 4. Office    one person,   large, Two office friends.
# 5. Office    three people, large, One neighborhood friend.
# 6. Office    five people,  large, One neighborhood friend.
# 7. Office    one person,   large, [No friend]
# 8. Family   [one painting] [large] [None]


# Long format ---------------------------------------------------------------

# Only for comparing office vs. family conditions

gossip_long <- 
  gossip %>% 
  dplyr::filter(Condition %in% 7:8) %>% 
  dplyr::select(
    ID,
    Condition,
    Scenario,
    Sex,
    Age,
    WorkPos,
    WorkNeg,
    FamilyPos,
    FamilyNeg
  ) %>%
  mutate(
    across(c(WorkPos, WorkNeg, FamilyPos, FamilyNeg), ~as.numeric(scale(.x)))
  ) %>% 
  pivot_longer(cols = c(WorkPos, WorkNeg, FamilyPos, FamilyNeg), names_to = 'Type') %>% 
  separate(Type, into = c('Domain', 'Valence'), remove = F, sep = -3) %>% 
  mutate(
    Scenario = ifelse(Scenario == 'office', 'Office scenario', 'Family scenario'),
    Domain = ifelse(Domain == 'Work', 'Work gossip', 'Family gossip'),
    Valence = ifelse(Valence == 'Neg', 'Negative', 'Positive'),
    Match = ifelse((Scenario == 'Office scenario' & Domain == 'Family gossip') | (Scenario == 'Family scenario' & Domain == 'Work gossip'), F, T)
  )

# Standardize variables ---------------------------------------------------------

gossip_original <- gossip

gossip <-
  gossip %>% 
  mutate(
    across(c(where(is.numeric), -ID, -Condition, -Promotions, -Allies), ~as.numeric(scale(.x))),
    Condition = factor(Condition),
    Sex = factor(Sex),
    PayRaise = factor(PayRaise, levels = c('small', 'large')),
    Promotions = ordered(Promotions),
    Allies = ordered(Allies)
    )

nina_original <- nina

nina <-
  nina %>% 
  mutate(
    across(-ID, ~as.numeric(scale(.x)))
  )

# Domain specificity -------------------------------------------------

# Conditions:
# 7: office (no friends, one large resource); 8: family (no friends, one large resource)

gossip$SELECT <- gossip$Condition == 7 | gossip$Condition == 8

# Impact of family vs. work situation on positive taste gossip

tastepos_ttest <- inline_ttest(
  t.test(TastePos ~ Condition, data = gossip, subset = SELECT),
  effsize = cohen_d(TastePos ~ Condition, gossip[gossip$SELECT,], sig = 2)
  )

# Impact of family vs. work situation on negative taste gossip

tasteneg_ttest <- inline_ttest(
  t.test(TasteNeg ~ Condition, data = gossip, subset = SELECT),
  effsize = cohen_d(TasteNeg ~ Condition, gossip[gossip$SELECT,], sig = 2)
  )

# Plot distributions

gossip_means <-
  gossip_long %>%
  group_by(Type, Scenario) %>% 
  summarise(mean = mean(value, na.rm = T))

plot_domains <-
  ggplot(gossip_long, aes(value, colour = Scenario)) + 
  geom_density(key_glyph='path') + 
  geom_rug(data=subset(gossip_long, Scenario == 'Family scenario'), aes(value), sides = 'b') +
  geom_rug(data=subset(gossip_long, Scenario == 'Office scenario'), aes(value), sides = 't') +
  geom_vline(data = gossip_means, aes(xintercept = mean, colour = Scenario), linetype = '11', size = 1, key_glyph='path') +
  scale_color_binary() +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(x = "Likelihood of transmitting gossip (Z-score)", y = "Density\n") +
  facet_wrap(~Type) +
  theme_minimal(15) +
  theme(legend.position = "right", legend.title=element_blank())
plot_domains

# Domain differences
m_domain <- lmer(value ~ Type*Scenario + (1 + Valence + Match|ID), data = gossip_long)
anova(m_domain)
em_domain <- emmeans(m_domain, specs = c('Type', 'Scenario'))
em_domain_df <- as.data.frame(em_domain)

plot_domain_diffs <-
  ggplot(em_domain_df, aes(Type, emmean)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=3.5, alpha = 0.5, position = position_dodge(width = 0.5)) + 
  geom_point() + 
  labs(x = '', y = 'Estimated marginal mean\n') +
  facet_wrap(~Scenario) + 
  theme_bw(15)
plot_domain_diffs


# Effect of gossip Valence

m_domain2 <- lmer(value ~ Domain*Valence*Scenario + (1|ID), data = gossip_long)
anova(m_domain2)
em_domain2 <- emmeans(m_domain2, specs = c('Domain', 'Valence', 'Scenario'))
em_domain2_df <- as.data.frame(em_domain2)
em_domain2b <- emmeans(m_domain2, specs = c('Domain', 'Scenario')) # Valence interaction terms small and not sig., so average over levels
effsizes <- as.data.frame(eff_size(em_domain2b, sigma = sigma(m_domain2), edf = df.residual(m_domain2)))
effsize_fam <- round(effsizes[2, c(2,5,6)], 2)
effsize_fam_str <- glue('d = {effsize_fam[1]} ({effsize_fam[2]}-{effsize_fam[3]})')
effsize_wrk <- round(-effsizes[5, c(2,5,6)], 2)
effsize_wrk_str <- glue('d = {effsize_wrk[1]} ({effsize_wrk[2]}-{effsize_wrk[3]})')

plot_domain_diffs2 <-
  ggplot(em_domain2_df, aes(Scenario, emmean, color = Valence)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=3.5, alpha = 0.5, position = position_dodge(width = 0.25)) + 
  geom_point(position = position_dodge(width = 0.25)) + 
  scale_color_binary() +
  labs(x = '', y = 'Likelihood of transmitting gossip (Z-score)\n') +
  facet_wrap(~Domain) + 
  theme_bw(15)
plot_domain_diffs2

m_domain3 <- lmer(value ~ Valence*Match + (1 + Match + Valence|ID), data = gossip_long)
anova(m_domain3)

# Explore sex differences
m_sex <- lmer(value ~ Sex*Scenario*Type + (1|ID), data = gossip_long)
anova(m_sex)
em_sex <- emmeans(m_sex, specs = c('Type', 'Sex', 'Scenario'))
em_sex_df <- as.data.frame(em_sex)

plot_sex_diffs <-
  ggplot(em_sex_df, aes(Scenario, emmean, colour=Sex)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=2.5, alpha = 0.7, position = position_dodge(width = 0.25)) + 
  geom_point(position = position_dodge(width = 0.25)) + 
  labs(x = '', y = 'Likelihood of transmitting gossip (Z-score)\n') +
  facet_wrap(~Type) + 
  theme_bw()
plot_sex_diffs

m_sex2 <- lmer(value ~ Sex*Domain*Valence + (1|ID), data = gossip_long)
anova(m_sex)
em_sex2 <- emmeans(m_sex2, specs = c('Sex', 'Valence'))

# Explore Age differences

m_age <- lmer(value ~ Age*Valence + (1|ID), data=gossip_long)
m_age60 <- lmer(value ~ Age*Valence + (1|ID), data=gossip_long, subset = Age <= 60, control = lmerControl(optimizer = 'Nelder_Mead'))

# Size of contestable resource (salary increase) vs gossip --------------------------

# small salary: 0; large salary: 1 (other variables are constant)

gossip$SELECT <- gossip$Condition == 0 | gossip$Condition == 1

# Comparing impact of small vs large salary increase on positive and negative gossip at work
m1 <- lm(WorkNeg ~ PayRaise + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m1)
plot(allEffects(m1))

m1b <- lm(WorkNeg ~ PayRaise, data = gossip, subset = SELECT)
summary(m1b)

m2 <- lm(WorkPos ~ PayRaise + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m2)
plot(allEffects(m2))

m2b <- lm(WorkPos ~ PayRaise, data = gossip, subset = SELECT)
summary(m2b)

# There is a significant interaction with sex for positive gossip
m2sex <- lm(WorkPos ~ PayRaise * Sex + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m2)
plot(allEffects(m2))

m2bsex <- lm(WorkPos ~ PayRaise * Sex, data = gossip, subset = SELECT)
summary(m2b)

# Scarcity of resource (# of promotions) vs gossip ----------------------------

# Number of promotions offered
# 1: one; 5: two; 6: five (other variables constant)

gossip$SELECT <- gossip$Condition == 1 | gossip$Condition == 5 | gossip$Condition == 6

# Scarcity vs. positive and negative gossip
m3 <- lm(WorkNeg ~ Promotions + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m3)
plot(allEffects(m3))

m3b <- lm(WorkNeg ~ Promotions, data = gossip, subset = SELECT)
summary(m3b)

m4 <- lm(WorkPos ~ Promotions + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m4)
plot(allEffects(m4))

m4b <- lm(WorkPos ~ Promotions, data = gossip, subset = SELECT)
summary(m4b)

# Work vs. neighborhood friends--------------------------------------------

# 1: one neighborhood friend; 2: two neighborhood friends
# 3: one office friend; 4: two office friends
# (other variables constant)

gossip$SELECT <- gossip$Condition %in% 1:4
gossip_original$SELECT <- gossip_original$Condition %in% 1:4

# Impact of work friend vs. neighborhood friend on negative and positive gossip

m5 <- lm(WorkNeg ~ Ally_location + Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m5)
plot(allEffects(m5))

m5b <- lm(WorkNeg ~ Ally_location + PhysicalAttack, data = gossip, subset = SELECT)
summary(m5b)

m6 <- lm(WorkPos ~ Ally_location +  Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m6)
plot(allEffects(m6))

m6b <- lm(WorkPos ~ Ally_location + PhysicalAttack, data = gossip, subset = SELECT)
summary(m6b)

# Comparing likelihood of reputational attack for work vs neighborhood friend
m <- lm(ReptuationAttack ~  Ally_location + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m)

# Comparing likelihood of physical attack for work vs neighborhood friend
m <- lm(PhysicalAttack ~ Ally_location + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m)

friendly_ttest <- inline_ttest(
  t.test(Friendliness ~ Ally_location, data = gossip_original, subset = SELECT),
  effsize = cohen_d(Friendliness ~ Ally_location, gossip_original[gossip_original$SELECT,], sig = 2)
)

aggressive_ttest <- inline_ttest(
  t.test(Aggressiveness ~ Ally_location, data = gossip_original, subset = SELECT),
  effsize = cohen_d(Aggressiveness ~ Ally_location, gossip_original[gossip_original$SELECT,], sig = 2)
)

physical_ttest <- inline_ttest(
  t.test(PhysicalAttack ~ Ally_location, data = gossip_original, subset = SELECT),
  effsize = cohen_d(PhysicalAttack ~ Ally_location, gossip_original[gossip_original$SELECT,], sig = 2)
)

# Number of friends -------------------------------------------------------

gossip$SELECT <- gossip$Condition %in% 3:4
gossip_original$SELECT <- gossip_original$Condition %in% 3:4

m7 <- lm(WorkNeg ~ Allies + Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m7)
plot(allEffects(m7))

m7b <- lm(WorkNeg ~ Allies + PhysicalAttack, data = gossip, subset = SELECT)
summary(m7b)

m8 <- lm(WorkPos ~ Allies +  Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m8)
plot(allEffects(m8))

m8b <- lm(WorkPos ~ Allies + PhysicalAttack, data = gossip, subset = SELECT)
summary(m8b)

# Friend vs. no friend ----------------------------------------------------

# Condition 7: No friend; 3: One office friend. (other variables constant)

gossip$SELECT <- gossip$Condition == 7 | gossip$Condition == 3

# Comparing Friendliness
m <- lm(Friendliness ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)

# Comparing becoming friends
m <- lm(BecomeFriends ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)

# Comparing likelihood of reputational attack
m <- lm(ReptuationAttack ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)

# Comparing likelihood of physical attack
m <- lm(PhysicalAttack ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)

# Comparing likelihood of Aggressiveness
m <- lm(Aggressiveness ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)

# Study 2: Sorority study -------------------------------------------------

m9 <- lm(reputational_harm ~ selfclose + friendsclose, nina)
summary(m9)
plot(allEffects(m9))

m9b <- lm(reputational_harm ~ friendsclose, nina)

# Regression table --------------------------------------------

models <- list(
  "Model 1: Resource size (negative office gossip)" = m1,
  "Model 2: Resource size (positive office gossip)" = m2,
  "Model 3: Scarcity (negative office gossip)" = m3,
  "Model 4: Scarcity (positive office gossip)" = m4,
  "Model 5: Ally location (negative office gossip)" = m5,
  "Model 6: Ally location (positive office gossip)" = m6,
  "Model 7: Number of allies (negative office gossip)" = m7,
  "Model 8: Number of allies (positive office gossip)" = m8,
  "Model 9: Social network vs reputational harm to antagonist" = m9
)

model_names <- names(models)

models2 <- list(m1=m1, m2=m2, m3=m3, m4=m4, m5=m5, m6=m6, m7=m7, m8=m8, m9=m9)
stats <- fmt_terms(models2) # For inline use in rmarkdown

# Table of conditions -----------------------------------------------------

condition_tbl <-
  gossip %>% 
  group_by(Condition) %>% 
  summarise(
    N = n(),
    Scenario = Scenario[1],
    Promotions = Promotions[1],
    Resource = PayRaise[1],
    Ally_location = Ally_location[1],
    Allies = Allies[1]
  ) %>% 
  mutate(
    Hypotheses = case_when(
      Condition == 0 ~ 'Resource size',
      Condition == 1 ~ 'Resource size, Scarcity, Allies',
      Condition == 2 ~ 'Allies',
      Condition == 3 ~ 'Allies, Ally vs. no ally',
      Condition == 4 ~ 'Allies',
      Condition == 5 ~ 'Scarcity',
      Condition == 6 ~ 'Scarcity',
      Condition == 7 ~ 'Domain specificity, Ally vs. no ally',
      Condition == 8 ~ 'Domain specificity'
    )
  )

condition_tbl$Resource[condition_tbl$Condition == 8] <- 'large' # Valuable painting
condition_tbl$Allies[condition_tbl$Condition == 8] <- 0 # No allies in family scenario

# Comparison -------------------------------------------------------------

model_statsA <-
  map_dfr(models, ~tidy(., conf.int=T), .id = 'Model') %>% 
  dplyr::filter(
    Model != "Model 7: Social network vs reputational harm to antagonist",
    ! term %in% c('(Intercept)', 'SexMale', 'Friendliness', 'Aggressiveness', 'PhysicalAttack', 'Promotions.Q')
  ) %>% 
  dplyr::select(Model, term, estimate, conf.low, conf.high) %>% 
  mutate(`Controls included` = T)

modelsB <- list(
  "Model 1: Resource size (negative office gossip)" = m1b,
  "Model 2: Resource size (positive office gossip)" = m2b,
  "Model 3: Scarcity (negative office gossip)" = m3b,
  "Model 4: Scarcity (positive office gossip)" = m4b,
  "Model 5: Ally location (negative office gossip)" = m5b,
  "Model 6: Ally location (positive office gossip)" = m6b,
  "Model 7: Number of allies (negative office gossip)" = m7b,
  "Model 8: Number of allies (positive office gossip)" = m8b
)

model_statsB <-
  map_dfr(modelsB, ~tidy(., conf.int=T), .id = 'Model')%>% 
  dplyr::filter(
    ! term %in% c('(Intercept)', 'SexMale', 'PhysicalAttack', 'Promotions.Q')
  ) %>% 
  dplyr::select(Model, term, estimate, conf.low, conf.high) %>% 
  mutate(`Controls included` = F)

model_stats <- 
  bind_rows(model_statsA, model_statsB) %>% 
  separate(Model, into = c('Model', 'Condition'), sep = ': ') %>% 
  mutate(
    Gossip = ifelse(str_detect(Condition, 'positive'), 'positive', 'negative'),
    Model = str_glue('{Model} ({Gossip} gossip)')
  ) %>% 
  dplyr::filter(Model != 'Model 9 (negative gossip)')

plot_covariate_compare <-
  ggplot(model_stats, aes(estimate, term, xmin = conf.low, xmax = conf.high, colour = `Controls included`)) +
  geom_errorbarh(height = 0, lwd = 2.5, alpha = 0.5, position = position_dodge(width = 0.7)) +
  geom_point(position = position_dodge(width = 0.7)) + 
  geom_vline(xintercept = 0, linetype = 'dotted') +
  facet_wrap(~Model) +
  guides(colour = guide_legend(reverse=T)) +
  ylab('') +
  theme_bw(15)
plot_covariate_compare

# Effect size plots --------------------------------------------------------

ggemmeans2 <- function (em, title, negpos, xaxis = T, supported = T, by = NULL, reorder = F) {
  emsum <- summary(em)
  estName <- attr(emsum, "estName")
  clNames <- attr(emsum, "clNames")
  var <- attr(emsum, "pri.vars")[1]
  if (reorder & is.factor(emsum[[var]])) {
    emsum[var] <- forcats::fct_reorder(emsum[[var]], emsum[[estName]])
  }
  if (negpos == 'Negative gossip') clr = 'red' else clr = 'darkgreen'
  if (is.null(by)) {
    p <- ggplot(emsum, aes_string(estName, var, xmin = clNames[1], xmax = clNames[2]))
  }
  else {
    p <- ggplot(emsum, aes_string(estName, var, xmin = clNames[1], xmax = clNames[2], colour = by))
  }
  p <- p + 
    geom_pointrange(colour = clr, lwd = 2.5, alpha = 0.7, position = position_dodge(width = 0.25), fatten = 1) + 
    geom_vline(xintercept = 0, linetype='dotted') +
    xlim(-0.5, 0.6) +
    labs(title = title, x = '') +
    guides(colour = guide_legend(override.aes = list(size = 1))) + 
    theme_minimal() +
    theme(axis.text = element_text(size = 12), axis.title.y = element_blank())
  if (!xaxis) p <- p + theme(axis.text.x = element_blank())
  return(p)
}

em_m1 <- emmeans(m1, specs = 'PayRaise')
em_m2 <- emmeans(m2, specs = 'PayRaise')
em_m3 <- emmeans(m3, specs = 'Promotions')
em_m4 <- emmeans(m4, specs = 'Promotions')
em_m5 <- emmeans(m5, specs = 'Ally_location')
em_m6 <- emmeans(m6, specs = 'Ally_location')
em_m7 <- emmeans(m7, specs = 'Allies')
em_m8 <- emmeans(m8, specs = 'Allies')
# em_m9 <- emmeans(m9, specs = c('friendsclose', 'selfclose'))

pem1 <- ggemmeans2(em_m1, 'Model 1: Pay raise*', 'Negative gossip', xaxis = F)
pem2 <- ggemmeans2(em_m2, 'Model 2: Pay raise', 'Positive gossip', xaxis = F)
pem3 <- ggemmeans2(em_m3, 'Model 3: Promotions*', 'Negative gossip', xaxis = F)
pem4 <- ggemmeans2(em_m4, 'Model 4: Promotions', 'Positive gossip', xaxis = F)
pem5 <- ggemmeans2(em_m5, 'Model 5: Ally location*', 'Negative gossip', xaxis = F)
pem6 <- ggemmeans2(em_m6, 'Model 6: Ally location*', 'Positive gossip')
pem7 <- ggemmeans2(em_m7, 'Model 7: Number of office allies', 'Negative gossip')
pem8 <- ggemmeans2(em_m8, 'Model 8: Number of office allies', 'Positive gossip')
pm9 <- visreg(m9, xvar = 'friendsclose', rug = F, partial = F, gg = T) + 
  labs(title = 'Model 9: Reputational harm*') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), axis.title.y = element_blank())

# pem9 <- ggemmeans2(em_m9, 'Model 9 (negative gossip)')

plot_alleffects <- pem1 + pem2 + pem3 + pem4 + pem5 + pem6 + pem7 + pem8 + pm9 + 
  plot_layout(ncol=3) +
  plot_annotation(
    title = "<span style='color:red;'><strong>&#x25cf; Negative gossip</strong></span> <span style='color:darkgreen;'><strong>&#x25cf; Positive gossip</strong></span><br>",
    theme = theme(plot.title = element_markdown(hjust=0.5)))

plot_alleffects

sessionInfo()