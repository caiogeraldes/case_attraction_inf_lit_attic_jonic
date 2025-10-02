library(rethinking)
library(tidyverse)
library(kableExtra)
require(gtools)
require(ggcheck)
require(vcd)
require(FactoMineR)
require(factoextra)
require(ca)
library(kableExtra)
library(ggridges)
library(bayesplot)
source("./fix_pairs_rethinking.R")

# Loading data
dados <- read_csv("./data.csv") %>%
  mutate(
    pos_pred_a = factor(pred_a_pos, levels = c("P", "A", "N")),
    OBJ_TH = factor(OBJ_TH, levels = c("Recipient", "Experiencer", "Agent"))
  )

dados %>% ggplot(aes(fill = Attr, x = autor_legivel)) +
  geom_bar(position = "dodge2") +
  xlab("Autor") +
  ylab("Passagens") +
  guides(fill = guide_legend(title = "Atração")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

dados %>% ggplot(aes(x = Attr, y = DIST_OBJ_PRED, fill = Attr)) +
  geom_boxplot() +
  xlab("Atração") +
  ylab("Distância (n de palavras)") +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  guides(fill = "none") +
  theme(
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

attach(dados)
mca <- MCA(
  dados[, c("Attr", "Vinf_Cop", "VM_MOD", "GENRE")],
  method = "Burt", graph = FALSE
)

# Eigen-values

fviz_screeplot(
  mca,
  addlabels = TRUE,
  ylim = c(0, 60),
  barfill = "#1B9E77",
  barcolor = "#1B9E77",
  main = "Eigenvalues",
  ylab = "% explained variance"
) +
  theme_bw()

# Dim1 and Dim2

plot(mca,
  invisible = "ind", graph.type = "ggplot",
  col.var = c(
    "#1B9E77", "#1B9E77",
    "#D95F02", "#D95F02",
    "#7570B3", "#7570B3",
    rep("#E7298A", 5),
    repel = TRUE
  )
) +
  theme_bw()

# Loading fitted model
source("./glm.R")
pairs(model)
# Sampling
samples <- extract.samples(model)

# PoS
precis(model, pars = c("b_pos", "delta_pos"), depth = 2)

precis(model, pars = c("d_part", "d_adj", "d_noun"), depth = 2)

pairs(
  model,
  pars = c("b_pos", "delta_pos"), labels = c("Participle", "Adjective", "Noun")
)

# Theta
precis(model, pars = c("b_th", "delta_th"), depth = 2)

precis(model, pars = c("d_recip", "d_exp", "d_agent"), depth = 2)

pairs(
  model,
  pars = c("b_th", "delta_th"), labels = c("Recipient", "Experiencer", "Agent")
)

# Copula and Distance
precis(model, pars = c("b_c", "b_dist"), depth = 2)

density_b_c <- density(samples$b_c, bw = 0.036)
density_b_c <- tibble(x = density_b_c$x, y = density_b_c$y)
boundaries <- HPDI(samples$b_c)
density_b_c %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_c,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_cop") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

density_b_dist <- density(samples$b_dist, bw = 0.0035)
density_b_dist <- tibble(x = density_b_dist$x, y = density_b_dist$y)
boundaries <- HPDI(samples$b_dist)
density_b_dist %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_dist,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_dist") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

# Dialect
precis(model, pars = c("b_dia_bar", "s_dia", "b_dia"), depth = 2)

density_b_attic <- density(samples$b_dia[, 1], bw = 0.06)
density_b_attic <- tibble(x = density_b_attic$x, y = density_b_attic$y)
boundaries <- HPDI(samples$b_dia[, 1])
density_b_attic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_attic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_attic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

density_b_jonic <- density(samples$b_dia[, 2], bw = 0.06)
density_b_jonic <- tibble(x = density_b_jonic$x, y = density_b_jonic$y)
boundaries <- HPDI(samples$b_dia[, 2])
density_b_jonic %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_b_jonic,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

density_diff_dia <- density(samples$b_dia[, 1] - samples$b_dia[, 2], bw = 0.06)
density_diff_dia <- tibble(x = density_diff_dia$x, y = density_diff_dia$y)
boundaries <- HPDI(samples$b_dia[, 1] - samples$b_dia[, 2])
density_diff_dia %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_diff_dia,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  geom_vline(aes(xintercept = 0)) +
  xlab("b_attic - b_jonic") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

# Author
precis(model, pars = c("s_auth", "b_auth"), depth = 2)

plot(precis(model, pars = "b_auth", depth = 2),
  labels = c(
    "Aeschines",
    "Aeschylus",
    "Andocides",
    "Antiphon",
    "Demosthenes",
    "Euripides",
    "Herodotus",
    "Isaeus",
    "Isocrates",
    "Lycurgus",
    "Lysias",
    "Plato",
    "Sophocles",
    "Thucydides",
    "Xenophon"
  )
)

hpdi_wrapper <- function(x, probs) {
  HPDI(x, prob = probs[2])
}
as_tibble(samples$b_auth) %>%
  rename(
    "Aeschines" = V1,
    "Aeschylus" = V2,
    "Andocides" = V3,
    "Antiphon" = V4,
    "Demosthenes" = V5,
    "Euripides" = V6,
    "Herodotus" = V7,
    "Isaeus" = V8,
    "Isocrates" = V9,
    "Lycurgus" = V10,
    "Lysias" = V11,
    "Plato" = V12,
    "Sophocles" = V13,
    "Thucydides" = V14,
    "Xenophon" = V15
  ) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(
    name,
    level =
      sort(
        c(
          "Aeschines", "Aeschylus", "Andocides", "Antiphon", "Demosthenes", "Euripides", "Herodotus",
          "Isaeus", "Isocrates", "Lycurgus", "Lysias", "Plato", "Sophocles", "Thucydides", "Xenophon"
        ),
        decreasing = TRUE
      )
  )) %>%
  ggplot(aes(x = value, y = name, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.11, 0.89),
    quantile_fun = hpdi_wrapper,
    scale = 2, bandwidth = 0.0500, show.legend = FALSE
  ) +
  scale_fill_manual(
    values = c("0", fill_alpha("purple", 0.75), "0"),
  ) +
  xlim(-2, 2) +
  xlab("b_auth") +
  ylab("Autor") +
  scale_y_discrete(expand = expansion(mult = c(0.01, .13))) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  ) +
  theme_ridges()

as_tibble(samples$b_auth) %>%
  rename(
    "Aeschines" = V1,
    "Aeschylus" = V2,
    "Andocides" = V3,
    "Antiphon" = V4,
    "Demosthenes" = V5,
    "Euripides" = V6,
    "Herodotus" = V7,
    "Isaeus" = V8,
    "Isocrates" = V9,
    "Lycurgus" = V10,
    "Lysias" = V11,
    "Plato" = V12,
    "Sophocles" = V13,
    "Thucydides" = V14,
    "Xenophon" = V15
  ) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(
    name,
    level =
      sort(
        c(
          "Aeschines", "Aeschylus", "Andocides", "Antiphon", "Demosthenes", "Euripides", "Herodotus",
          "Isaeus", "Isocrates", "Lycurgus", "Lysias", "Plato", "Sophocles", "Thucydides", "Xenophon"
        ),
        decreasing = TRUE
      )
  )) %>%
  ggplot(aes(x = name, y = value, width = after_stat(density), fill = name)) +
  geom_vridgeline(
    stat = "ydensity",
    trim = FALSE,
    alpha = 0.4,
    scale = 2,
    show.legend = FALSE,
    lwd = 0.1
  ) +
  xlab("auth") +
  ylab("b_auth") +
  theme_ridges() +
  theme(axis.text.x = element_text(angle = -90, vjust = 0.5, hjust = 1))

# Average effect
precis(model, pars = c("a_bar", "s_sent"))
density_a <- density(samples$a_bar, bw = 0.086)
density_a <- tibble(x = density_a$x, y = density_a$y)
boundaries <- HPDI(samples$a_bar)
density_a %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(
    data = subset(
      density_a,
      x > boundaries[1] & x < boundaries[2]
    ),
    mapping = aes(x = x, y = y),
    fill = "purple",
    alpha = 0.5
  ) +
  xlab("a") +
  ylab("density") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

color_scheme_set("teal")
mcmc_scatter(
  model@cstanfit$draws(c("a_bar", "b_dia_bar"))
)

as_tibble(samples$a_sent) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = factor(name)) %>%
  ggplot(aes(x = name, y = value, width = after_stat(density), fill = name)) +
  geom_vridgeline(
    stat = "ydensity",
    trim = FALSE,
    alpha = 0.1,
    scale = 40,
    show.legend = FALSE,
    lwd = 0.01
  ) +
  geom_hline(yintercept = mean(samples$a_bar), alpha = 0.2) +
  xlab("sentence") +
  ylab("a_sent") +
  theme_ridges() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

tibble(b_dia_bar = samples$b_dia_bar, a_bar = samples$a_bar) %>%
  ggplot(aes(x = a_bar, y = b_dia_bar)) +
  geom_density2d_filled()
