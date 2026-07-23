library(tidyverse)
library(scales)  # for colorRampPalette
library(grid)    # for unit()
library(INBOtheme)

conflicted::conflicts_prefer(dplyr::filter)

df <- read_delim("./data/tblRLCEurope20102025_short.csv",
								 delim = ";") %>%
	filter(Year != "y1999") %>%
	filter(nYears >= 2)
	#filter(GlobalRange != "Range extends outside Palearctic and Holarctic")
head(df)

unique(df$Speciesname)

table(df$RLC, df$Year)

df_wide <- df %>%
	pivot_wider(
		id_cols = Speciesname,
		names_from = Year,
		values_from = RLC,
		names_prefix = "RLC_"
	)
head(df_wide)
nrow(df_wide)

df_wide <- df_wide %>%
	#filter(!is.na(RLC_y2025)) %>%
	#filter(!is.na(RLC_y2010)) %>%
	select(Speciesname,
				 RLC_y2010,
				 RLC_y2025)
head(df_wide)
nrow(df_wide)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_delim(df_wide,
						"./output/tables/specieslistanalysis.csv",
						delim = ";")

#score <- c(LC = 0, NT = 1, VU = 2, EN = 4, CR = 8, EX = 16, RE = 16) # what about DD? doubling steps
score <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5) # what about DD? doubling steps

df_wide$sc2010 <- score[df_wide$RLC_y2010]
df_wide$sc2025 <- score[df_wide$RLC_y2025]
head(df_wide)
nrow(df_wide)

N <- nrow(df_wide)
N

S2010 <- sum(df_wide$sc2010,
						 na.rm = TRUE)
S2010

S2025 <- sum(df_wide$sc2025,
						 na.rm = TRUE)
S2025

RLI2010 <- 1 - S2010 / (N * 5)
#RLI2010 <- 1 - S2010 / (N * 16)
RLI2010

RLI2025 <- 1 - S2025 / (N * 5)
#RLI2025 <- 1 - S2025 / (N * 16)
RLI2025

RLIchange20102025 <- 100 * (RLI2025 - RLI2010)/RLI2010
RLIchange20102025

cat("RLI 2010:", RLI2010, "\n")
cat("RLI 2025:", RLI2025, "\n")
cat("Annual rate:", (RLI2025 - RLI2010)/16, "\n")

# bootstrap for CI
set.seed(0)
nboot <- 10000
diffs <- numeric(nboot)
for (i in 1:nboot) {
	samp <- df_wide[sample(1:N,
												 N,
												 replace = TRUE), ]
	r2 <- 1 - sum(samp$sc2010, na.rm = TRUE) / (N * 5)
	r3 <- 1 - sum(samp$sc2025, na.rm = TRUE) / (N * 5)
	#r2 <- 1 - sum(samp$sc2010, na.rm = TRUE) / (N * 16)
	#r3 <- 1 - sum(samp$sc2025, na.rm = TRUE) / (N * 16)
	diffs[i] <- r3 - r2
}

mean_diff <- mean(diffs)
mean_diff

ci_diff <- quantile(diffs, c(0.025, 0.975))
ci_diff

rli_2010 <- numeric(nboot)
rli_2025 <- numeric(nboot)

for (i in 1:nboot) {
	samp <- df_wide[sample(1:N, N, replace = TRUE), ]
	rli_2010[i] <- 1 - sum(samp$sc2010, na.rm = TRUE) / (N * 5)
	rli_2025[i] <- 1 - sum(samp$sc2025, na.rm = TRUE) / (N * 5)
	#rli_2010[i] <- 1 - sum(samp$sc2010, na.rm = TRUE) / (N * 16)
	#rli_2025[i] <- 1 - sum(samp$sc2025, na.rm = TRUE) / (N * 16)
}

rli_summary_Overall <- data.frame(
	year = c(2010, 2025),
	mean_difference = c(mean(rli_2010), mean(rli_2025)),
	lower_ci = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	upper_ci = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
) %>%
	# https://b-cubed-eu.github.io/dubicube/articles/effect-classification.html
	dubicube::add_effect_classification(
		c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0
	)
rli_summary_Overall

# ??Werkt nog niet!

# Without 1999
pd <- position_dodge(width = 0.5)

p <- ggplot(rli_summary_Overall,
						aes(x = year,
								y = mean_difference)) +
	geom_point(size = 3,
						 position = pd) +
	geom_line(linewidth = 1.5,
						linetype = 1,
						position = pd) +
	geom_errorbar(aes(ymin = lower_ci,
										ymax = upper_ci),
								width = 1,
								linewidth = 1,
								position = pd) +
	ylim(0.55, 1) +
	ylab("Red List Index") +
	xlab("Year") +
	labs(x = "Year",
			 y = "Red List Index") +
	theme(axis.text.x = element_text(angle = 0,
																	 hjust = 0.5),
				axis.text = element_text(size = 20),
				axis.title = element_text(size = 20,
																	face = "bold"),
				legend.position = "bottom",
				legend.text = element_text(size = 20),
				legend.title = element_blank(),
				#legend.title = element_text(size = 20),
				panel.background = element_rect(fill = "white",
																				colour = "grey",
																				linewidth = 1,
																				linetype = "solid"),
				panel.grid.major = element_line(linewidth = 0.1,
																				linetype = 1,
																				colour = "grey"),
				panel.grid.major.x = element_line(color = "grey",
																					linewidth = 0.5,
																					linetype = 1),
				axis.title.y = element_text(angle = 90,
																		vjust = 0.5)) +
	scale_x_continuous(breaks = c(2010, 2025)) +
	ggtitle("Overall")
p

dir.create("output/figures/rli", recursive = TRUE, showWarnings = FALSE)
ggsave("./output/figures/rli/RLI_EuropeanButterflies2010_2025_all.jpg",
			 width = 10,
			 height = 7,
			 dpi = 150)

rli_summary_Overall <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
)
rli_summary_Overall$label <- "All"

rli_summary_Overall <- rli_summary_Overall %>%
	mutate(
		n_specs = mean_difference * n * 5,
		label = paste0(label, " (n = ", n, ")")
	) %>%
	dubicube::add_effect_classification(
		cl_columns = c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0,
		coarse = FALSE
	)
rli_summary_Overall

p <- ggplot(
	rli_summary_Overall,
	aes(y = mean_difference,
			x = reorder(label, -upper_ci),
			ymin = lower_ci,
			ymax = upper_ci)
) +
	geom_errorbar(aes(colour = effect),
								linewidth = 2) +

	# Add a vertical dashed line at 0 for reference
	geom_hline(yintercept = 0,
						 linetype = "dashed",
						 linewidth = 1,
						 colour = "black",
						 alpha = 1) +
	geom_hline(yintercept = -0.02,
						 linetype = "dotdash",
						 linewidth = 1,
						 colour = "black",
						 alpha = 1) +
	geom_hline(yintercept = 0.02,
						 linetype = "dotdash",
						 linewidth = 1,
						 colour = "black",
						 alpha = 1) +
	# Add the horizontal error bars
	effectclass::stat_effect(
		aes(ymin = lower_ci,
				ymax = upper_ci),
		reference = 0,
		threshold = 0.02,
		size = 10
	) +
	coord_flip() +
	# Clean up labels and theme
	labs(y = "Mean Difference",
			 x = "Overall"
	) +
	theme(axis.text.x = element_text(angle = 0,
																	 hjust = 0.5),
				axis.text = element_text(size = 20),
				axis.title = element_text(size = 20,
																	face = "bold"),
				legend.position = "right",
				legend.text = element_text(size = 20),
				legend.title = element_blank(),
				panel.background = element_rect(fill = "white",
																				colour = "grey",
																				linewidth = 1,
																				linetype = "solid"),
				panel.grid.major = element_line(linewidth = 0.1,
																				linetype = 1,
																				colour = "grey"),
				panel.grid.major.y = element_line(color = "grey",
																					linewidth = 0.5,
																					linetype = 1),
				axis.title.y = element_text(angle = 90,
																		vjust = 0.5)) +
	scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
	scale_y_continuous(limits = c(-0.3, 0.05),
										 breaks = seq(-0.3, 0.05, by = 0.05),
										 labels = label_number(accuracy = 0.01)) +
	labs(color = "Trend")
p

dir.create("output/figures/diff_rli", recursive = TRUE, showWarnings = FALSE)
ggsave("./output/figures/diff_rli/diffRLI_EuropeanButterflies2010_2025_all.jpg",
			 width = 12,
			 height = 5,
			 dpi = 150)

