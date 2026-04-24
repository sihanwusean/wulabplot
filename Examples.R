library(dplyr)
library(ggplot2)
library(wulabplot)

# Example 1
mtcars$cyl_fact <- as.factor(mtcars$cyl)
p_qual <- ggplot(mtcars, aes(x = cyl_fact, y = mpg, fill = cyl_fact)) +
  geom_boxplot(color = "black", linewidth = 0.2, outlier.shape = NA) +
  scale_fill_wulab(type = "qualitative-light") +
  labs(title = "Example 1", x = "Number of Cylinders", y = "Miles Per Gallon") +
  theme_wulab() +
  theme(legend.position = "none")
save_wulab(p_qual, type = "2x2", filename = "Example 1.pdf")

# Example 2
df_line <- mtcars %>%
  group_by(cyl) %>%
  slice(1:7) %>%
  mutate(obs_id = row_number()) %>%
  ungroup() %>%
  mutate(cyl_fact = as.factor(cyl))
p_line_standard <- ggplot(df_line, aes(x = obs_id, y = mpg, color = cyl_fact)) +
  geom_line() +
  geom_point(stroke = NA) +
  scale_color_wulab(type = "qualitative-deep") +
  labs(title = "Example 2", x = "Observation Index", y = "Miles Per Gallon", color = "Cylinders") +
  theme_wulab()
save_wulab(p_line_standard, type = "2x2", filename = "Example 2.pdf")

# Example 3
df_5bar <- mtcars
df_5bar$hp_group <- ggplot2::cut_number(df_5bar$hp, n = 5)
p_5bar <- ggplot(df_5bar, aes(x = hp_group, y = mpg, fill = hp_group)) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  scale_fill_wulab(type = "sequential") +
  labs(title = "Example 3", x = "Horsepower Quintiles", y = "Mean MPG") +
  theme_wulab() +
  theme(axis.text.x = element_blank())
p_5bar
save_wulab(p_5bar, type = "2.58x2", filename = "Example 3.pdf")

# Example 4
mtcars$mpg_z <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
mtcars$car_name <- rownames(mtcars)
p_div <- ggplot(mtcars[1:10, ], aes(x = reorder(car_name, mpg_z), y = mpg_z, fill = mpg_z)) +
  geom_col(color = "black", linewidth = 0.2) +
  coord_flip() +
  scale_fill_wulab(type = "diverging", discrete = FALSE) +
  labs(title = "Example 4", x = NULL, y = "Z-score") +
  theme_wulab() +
  theme(legend.position = "none")
save_wulab(p_div, type = "2x4.9", filename = "Example 4.pdf")

# Example 5
set.seed(42)
num_points_per_cluster <- 500
centroids <- expand.grid(x = seq(-3, 3, length.out = 4), y = seq(-3, 3, length.out = 5))
centroids <- centroids[sample(nrow(centroids), 20), ]
df_umap <- data.frame(
  UMAP1 = unlist(lapply(centroids$x, function(x) rnorm(num_points_per_cluster, x, 0.5))),
  UMAP2 = unlist(lapply(centroids$y, function(y) rnorm(num_points_per_cluster, y, 0.5))),
  Cluster = factor(rep(1:20, each = num_points_per_cluster))
)
p_umap <- ggplot(df_umap, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
  geom_point(size = 0.5, stroke = NA) +
  scale_color_wulab(type = "umap") +
  theme_wulab() +
  labs(title = "Example 5",
       x = "UMAP 1", y = "UMAP 2") +
  theme(legend.position = "none")
save_wulab(p_umap, type = "4.9x4.9", filename = "Example 5.pdf")

# Example 6
mtcars$cyl_fact <- as.factor(mtcars$cyl)
p_facet <- ggplot(mtcars, aes(x = wt, y = mpg, color = cyl_fact, fill = cyl_fact)) +
  geom_smooth(method = "lm") +
  geom_point(stroke = NA, color = "black") +
  facet_wrap(~cyl_fact, nrow = 1) +
  scale_color_wulab(type = "qualitative-deep") +
  scale_fill_wulab(type = "qualitative-light") +
  labs(title = "Example 6", x = "Weight (1000 lbs)", y = "MPG") +
  theme_wulab() +
  theme(legend.position = "none")
save_wulab(p_facet, type = "2x2", filename = "Example 6.pdf")

