
library(tidyverse)
library(lme4)
library(emmeans)
library(effects)
library(broom)
library(hagenutils)
library(gossipdata2008)

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

# Rescale variables ---------------------------------------------------------

gossip <- 
  gossip %>% 
  mutate(
    WorkGossip = as.numeric(scale(WorkPos + WorkNeg)),
    FamilyGossip = as.numeric(scale(FamilyPos + FamilyNeg)),
  )

gossip_original <- gossip

gossip <-
  gossip %>% 
  mutate(
    across(c(where(is.numeric), -ID, -Condition, -Promotions, -Allies), ~as.numeric(scale(.x))),
    Condition = factor(Condition),
    Sex = factor(Sex),
    PayRaise = factor(PayRaise, levels = c('small', 'large'))
    )

# Domain specificity -------------------------------------------------

# Conditions:
# 7: office (no friends, one large resource); 8: family (no friends, one large resource)

gossip$SELECT <- gossip$Condition == 7 | gossip$Condition == 8

# Impact of family vs. work situation on positive taste gossip"

tastepos_ttest <- inline_ttest(
  t.test(TastePos ~ Condition, data = gossip, subset = SELECT),
  effsize = cohen_d(TastePos ~ Condition, gossip[gossip$SELECT,], sig = 2)
  )

# Impact of family vs. work situation on negative taste gossip

tasteneg_ttest <- inline_ttest(
  t.test(TasteNeg ~ Condition, data = gossip, subset = SELECT),
  effsize = cohen_d(TasteNeg ~ Condition, gossip[gossip$SELECT,], sig = 2)
  )

# Create GossipBias
gossip <-
  gossip %>%
  group_by(SELECT) %>% 
  mutate(
    GossipBias = WorkGossip - FamilyGossip
  ) %>% 
  ungroup()


bias_ttest <- inline_ttest(
  t.test(GossipBias ~ Condition, data = gossip, subset = SELECT),
  effsize = cohen_d(GossipBias ~ Condition, data = gossip[gossip$SELECT,], sig = 2)
)

gossip_diff <- 
  gossip %>% 
  dplyr::filter(SELECT) %>% 
  mutate(
    PosDiff = ifelse(Scenario == 'office', WorkPos - FamilyPos, FamilyPos - WorkPos),
    NegDiff = ifelse(Scenario == 'office', WorkNeg - FamilyNeg, FamilyNeg - WorkNeg)
  ) 

diff_ttest <- t.test(gossip_diff$NegDiff, gossip_diff$PosDiff, paired = T)

# Plot distributions

gossip_long <- 
  gossip %>% 
  dplyr::select(
    ID, 
    Condition,
    Scenario,
    Sex,
    Age,
    WorkPos, 
    WorkNeg, 
    FamilyPos, 
    FamilyNeg,
    SELECT
  ) %>% 
  pivot_longer(cols = c(WorkPos, WorkNeg, FamilyPos, FamilyNeg), names_to = 'Type')

gossip_means <-
  gossip_long %>% 
  dplyr::filter(SELECT) %>% 
  group_by(Type, Scenario) %>% 
  summarise(mean = mean(value, na.rm = T))

plot_domains <-
  ggplot(subset(gossip_long, SELECT), aes(value, colour = Scenario)) + 
  geom_density(key_glyph='path') + 
  geom_rug(data=subset(gossip_long, Scenario == 'family' & SELECT), aes(value), sides = 'b') +
  geom_rug(data=subset(gossip_long, Scenario == 'office' & SELECT), aes(value), sides = 't') +
  geom_vline(data = gossip_means, aes(xintercept = mean, colour = Scenario), linetype = 'dotted',key_glyph='path') +
  scale_color_binary() +
  labs(x = "\nLikelihood of transmitting gossip", y = "Density\n") +
  facet_wrap(~Type) +
  theme_minimal()
plot_domains
ggsave(filename = 'Figure1.pdf', plot = plot_domains, width = 8, height = 5)

# Explore sex differences
m <- lmer(value ~ Sex*Scenario*Type + (1|ID), data = gossip_long)
em <- emmeans(m, specs = c('Type', 'Sex', 'Scenario'))
em2 <- as.data.frame(em)

ggplot(em2, aes(Scenario, emmean, colour=Sex)) + 
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0, lwd=2.5, alpha = 0.7, position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  facet_wrap(~Type) + 
  theme_bw(15)

# Size of contestable resource (salary increase) vs gossip --------------------------

# small salary: 0; large salary: 1 (other variables are constant)

gossip$SELECT <- gossip$Condition == 0 | gossip$Condition == 1

# Comparing impact of small vs large salary increase on positive and negative gossip at work
m1 <- lm(WorkNeg ~ PayRaise + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m1)
plot(allEffects(m1))

# There is a significant interaction with sex for positive gossip
m2 <- lm(WorkPos ~ PayRaise * Sex + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m2)
plot(allEffects(m2))

# Contestability of resource (# of promotions) vs gossip ----------------------------

# Number of promotions offered
# 1: one; 5: two; 6: five

gossip$SELECT <- gossip$Condition == 1 | gossip$Condition == 5 | gossip$Condition == 6

# Contestability vs. positive and negative gossip
m3 <- lm(WorkNeg ~ Promotions + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m3)
plot(allEffects(m3))

m4 <- lm(WorkPos ~ Promotions + Friendliness + Aggressiveness, data = gossip, subset = SELECT)
summary(m4)
plot(allEffects(m4))

# Work vs. neighborhood friends--------------------------------------------

# 1: one neighborhood friend; 2: two neighborhood friends
# 3: one office friend; 4: two office friends

gossip$SELECT <- gossip$Condition %in% 1:4
gossip_original$SELECT <- gossip_original$Condition %in% 1:4

# Impact of work friend vs. neighborhood friend on negative and positive gossip

m5 <- lm(WorkNeg ~ Allies * Ally_location +  Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m5)
plot(allEffects(m5))

m6 <- lm(WorkPos ~ Allies * Ally_location +  Friendliness + Aggressiveness + PhysicalAttack, data = gossip, subset = SELECT)
summary(m6)
plot(allEffects(m6))

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

# Friend vs. no friend ----------------------------------------------------

# Condition 7: No friend; 3: One office friend.

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

# Comparing likelihood of Dominance
m <- lm(Dominance ~ Sex * Condition, data = gossip, subset = SELECT)
summary(m)


# Study 2: Sorority study -------------------------------------------------

m7 <- lm(reputational_harm ~ selfclose + friendsclose, nina)
summary(m)
plot(allEffects(m7))

# Regression table --------------------------------------------

models <- list(
  "Model 1: Resource size (negative office gossip)" = m1,
  "Model 2: Resource size (positive office gossip)" = m2,
  "Model 3: Contestability (negative office gossip)" = m3,
  "Model 4: Contestability (positive office gossip)" = m4,
  "Model 5: Office vs neighbor ally (negative office gossip)" = m5,
  "Model 6: Office vs neighbor ally (positive office gossip)" = m6,
  "Model 7: Social network vs reputational harm to antagonist" = m7
)

model_stats <-
  map_dfr(models, ~tidy(., conf.int=T), .id = 'Model')

model_summary <- 
  map_dfr(models, ~glance(.)) %>%
  mutate(across(r.squared:p.value, ~signif(.x, 2))) %>% 
  str_glue_data('N={nobs}; Rsq={r.squared}; Adj.Rsq={adj.r.squared}; F({df},{df.residual})={statistic}; p={p.value}')

sessionInfo()