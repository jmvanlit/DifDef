##### SET UP ---------------------------------------------------------------------------------------

### packages
library(DeclareDesign)
library(ggplot2)
library(dplyr)

##### POWER ANALYSIS ------------------------------------------------------------------------------

### Base Parameters (all can be varied later)
N <- 120
b <- 0.1
n_cond <- 9

### Declare Design
design <- 
  
  # Model
  declare_population(N = N,
                     u = rnorm(N)) +
  declare_potential_outcomes(Y ~ b * Z + u) +
  
  # Inquiry
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  
  # Data Strategy
  declare_assignment(Z = complete_ra(N,
                                     prob = 1 / n_cond),
                     Y = reveal_outcomes(Y ~ Z)) +
  
  # Answer Strategy
  declare_estimator(Y ~ Z,
                    inquiry = "ATE",
                    .method = lm)

### Incorporate multiple designs
multiple_designs <- 
  design |> 
  redesign(b = c(0.1, 0.15, 0.2, 0.3), # vary effect size
           N = seq(from = 3000, # vary number of respondents
                   to = 4000,
                   by = 250),
           n_cond = 9) # vary number of conditions to be compared

### Diagnose
power <-
  diagnose_design(multiple_designs,
                  sims = 1000) |>
  tidy() |> 
  filter(diagnosand == "power")

### Plot
ggplot(data = power,
       aes(x = N,
           y = estimate)) +
  
  # Criteria
  geom_hline(yintercept = 0.8,
             linetype = "dotdash",
             color = "darkgrey") +
  geom_vline(xintercept = 3750,
             linetype = "dotdash",
             color = "darkgrey") +
  
  # CIs
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  group = factor(b)),
              alpha = 0.2) +
  
  # Power lines
  geom_point(aes(shape = factor(b),
                 color = factor(b)),
             size = 2) +
  geom_line(aes(color = factor(b),
                group = factor(b)),
            linewidth = 0.8) +
  
  # Annotations
  labs(x = "Number of Respondents",
       y = "Power (One-Tailed)",
       color = "Effect size",
       shape = "Effect size") +
  
  # Theme
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = c("#16607a", "#09bb9f", "#ffb55f", "#a63716"))

ggsave(filename = "figures/power_analysis.png",
       dpi = 1200,
       width = 7,
       height = 6)

# END OF CODE /./
