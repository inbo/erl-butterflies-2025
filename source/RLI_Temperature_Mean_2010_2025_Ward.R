library(tidyverse)
library(scales)  # for colorRampPalette
library(grid)    # for unit()
library(INBOtheme)

conflicted::conflicts_prefer(dplyr::filter)

#############
# Read data #
#############
dfTraits <- read_delim("./data/tblTraitSpeciesTemperatureIndex.csv",
											 delim = ";") %>%
	filter(Trait == "SpeciesTemperatureIndex") %>%
	filter(nYears >= 2) %>%
	select(SpeciesnameFull,
				 Trait,
				 TraitValue) %>%
	rename(Speciesname = SpeciesnameFull)
head(dfTraits)
nrow(dfTraits)

dfRLC <- read_delim("./data/tblRLCEurope20102025_short.csv",
										delim = ";") %>%
	filter(Year != "y1999") %>%
	filter(!is.na(RLC))
head(dfRLC)
nrow(dfRLC)

# Join data
dfTraitRLC <- left_join(dfTraits,
												dfRLC,
												by = "Speciesname")
head(dfTraitRLC)
nrow(dfTraitRLC)

unique(dfTraitRLC$Speciesname)

#############
# Very cold #
#############
df <- dfTraitRLC %>%
	filter(TraitValue == "VeryCold") %>%
	filter(TraitValue != "Range extends outside Palearctic and Holarctic") %>%
	filter(!is.na(RLC))
head(df)
nrow(df)

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

#score <- c(LC = 0, NT = 1, VU = 2, EN = 4, CR = 8, EX = 16, RE = 16) # what about DD? doubling steps
score <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5) # what about DD? doubling steps

df_wide$sc2010 <- score[df_wide$RLC_y2010]
df_wide$sc2025 <- score[df_wide$RLC_y2025]
head(df_wide)
nrow(df_wide)

N <- nrow(df_wide)
N

S2010 <- sum(df_wide$sc2010, na.rm = TRUE)
S2010

S2025 <- sum(df_wide$sc2025, na.rm = TRUE)
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
cat("Change:", RLI2025 - RLI2010, "\n")
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

rli_summary_VeryCold <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_VeryCold$SpeciesTemperatureIndex <- "VeryCold"
head(rli_summary_VeryCold)

df_summary_VeryCold <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(SpeciesTemperatureIndex = "Very cold") %>%
	select(SpeciesTemperatureIndex, n, lower_ci, mean_difference, upper_ci)
df_summary_VeryCold

################
# Cold species #
################
df <- dfTraitRLC %>%
	filter(TraitValue == "Cold") %>%
	filter(TraitValue != "Range extends outside Palearctic and Holarctic") %>%
	filter(!is.na(RLC))
head(df)
nrow(df)

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

#score <- c(LC = 0, NT = 1, VU = 2, EN = 4, CR = 8, EX = 16, RE = 16) # what about DD? doubling steps
score <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5) # what about DD? doubling steps

df_wide$sc2010 <- score[df_wide$RLC_y2010]
df_wide$sc2025 <- score[df_wide$RLC_y2025]
head(df_wide)
nrow(df_wide)

N <- nrow(df_wide)
N

S2010 <- sum(df_wide$sc2010, na.rm = TRUE)
S2010

S2025 <- sum(df_wide$sc2025, na.rm = TRUE)
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
cat("Change:", RLI2025 - RLI2010, "\n")
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

rli_summary_Cold <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)
head(rli_summary_Cold)

rli_summary_Cold$SpeciesTemperatureIndex <- "Cold"
head(rli_summary_Cold)

df_summary_Cold <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(SpeciesTemperatureIndex = "Cold") %>%
	select(SpeciesTemperatureIndex, n, lower_ci, mean_difference, upper_ci)
df_summary_Cold

################
# Warm species #
################
df <- dfTraitRLC %>%
	filter(TraitValue == "Warm") %>%
	filter(TraitValue != "Range extends outside Palearctic and Holarctic") %>%
	filter(!is.na(RLC))
head(df)
nrow(df)

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

#score <- c(LC = 0, NT = 1, VU = 2, EN = 4, CR = 8, EX = 16, RE = 16) # what about DD? doubling steps
score <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5) # what about DD? doubling steps

df_wide$sc2010 <- score[df_wide$RLC_y2010]
df_wide$sc2025 <- score[df_wide$RLC_y2025]
head(df_wide)
nrow(df_wide)

N <- nrow(df_wide)
N

S2010 <- sum(df_wide$sc2010, na.rm = TRUE)
S2010

S2025 <- sum(df_wide$sc2025, na.rm = TRUE)
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
cat("Change:", RLI2025 - RLI2010, "\n")
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

rli_summary_Warm <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_Warm$SpeciesTemperatureIndex <- "Warm"
head(rli_summary_Warm)

df_summary_Warm <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(SpeciesTemperatureIndex = "Warm") %>%
	select(SpeciesTemperatureIndex, n, lower_ci, mean_difference, upper_ci)
df_summary_Warm

#####################
# Very warm species #
#####################
df <- dfTraitRLC %>%
	filter(TraitValue == "VeryWarm") %>%
	filter(TraitValue != "Range extends outside Palearctic and Holarctic") %>%
	filter(!is.na(RLC))
head(df)
nrow(df)

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

#score <- c(LC = 0, NT = 1, VU = 2, EN = 4, CR = 8, EX = 16, RE = 16) # what about DD? doubling steps
score <- c(LC = 0, NT = 1, VU = 2, EN = 3, CR = 4, EX = 5, RE = 5) # what about DD? doubling steps

df_wide$sc2010 <- score[df_wide$RLC_y2010]
df_wide$sc2025 <- score[df_wide$RLC_y2025]
head(df_wide)
nrow(df_wide)

N <- nrow(df_wide)
N

S2010 <- sum(df_wide$sc2010, na.rm = TRUE)
S2010

S2025 <- sum(df_wide$sc2025, na.rm = TRUE)
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
cat("Change:", RLI2025 - RLI2010, "\n")
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

rli_summary_VeryWarm <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_VeryWarm$SpeciesTemperatureIndex <- "VeryWarm"
head(rli_summary_VeryWarm)

df_summary_VeryWarm <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(SpeciesTemperatureIndex = "Very warm") %>%
	select(SpeciesTemperatureIndex, n, lower_ci, mean_difference, upper_ci)
df_summary_VeryWarm

##########################
# Put all RLI's together #
##########################
mean_differences <- rbind(df_summary_VeryCold,
													df_summary_Cold,
													df_summary_Warm,
													df_summary_VeryWarm) %>%
	# https://b-cubed-eu.github.io/dubicube/articles/effect-classification.html
	dubicube::add_effect_classification(
		c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0
	)

rli_summary_all <- rbind(rli_summary_VeryCold,
												 rli_summary_Cold,
												 rli_summary_Warm,
												 rli_summary_VeryWarm)
rli_summary_all

# Data for the final year only
final_labels <- rli_summary_all %>%
	group_by(SpeciesTemperatureIndex) %>%
	filter(year == max(year)) %>%
	ungroup() %>%
	left_join(
		mean_differences %>%
			select(SpeciesTemperatureIndex, effect_code),
		by = c("SpeciesTemperatureIndex" = "SpeciesTemperatureIndex")
	)

rli_summary_all$SpeciesTemperatureIndex <- factor(rli_summary_all$SpeciesTemperatureIndex,
																	 levels = c("VeryCold",
																	 					 "Cold",
																	 					 "Warm",
																	 					 "VeryWarm"))

# Without 1999
pd <- position_dodge(width = 1.5)

p <- ggplot(rli_summary_all,
						aes(x = year,
								y = rli_mean,
								group = SpeciesTemperatureIndex,
								colour = SpeciesTemperatureIndex)) +
	geom_point(size = 3,
						 position = pd) +
	geom_errorbar(aes(ymin = rli_lower,
										ymax = rli_upper,
										colour = SpeciesTemperatureIndex,
										group = SpeciesTemperatureIndex),
								width = 1,
								linewidth = 1,
								position = pd) +
	geom_line(size = 1,
						linetype = 1,
						position = pd) +
	ggrepel::geom_label_repel(
		data = final_labels,
		aes(label = as.character(effect_code), fill = SpeciesTemperatureIndex),
		box.padding = 0,
		label.padding = unit(0.2, "lines"),
		label.r = 0.5,
		colour = "white",
		direction = "y",
		nudge_x = 2,
		hjust = "center",
		vjust = "center",
		segment.color = NA,
		show.legend = FALSE
	) +
	ylim(0.55, 1) +
	ylab("Red List Index") +
	xlab("Year") +
	labs(x = "Year",
			 y = "Red List Index") +
	theme(axis.text.x = element_text(angle = 0,
																	 hjust = 0.5),
				axis.text = element_text(size = 15),
				axis.title = element_text(size = 15,
																	face = "bold"),
				#legend.position = "bottom",
				legend.text = element_text(size = 15),
				legend.title = element_blank(),
				#legend.title = element_text(size = 15),
				panel.background = element_rect(fill = "white",
																				colour = "grey",
																				linewidth = 1,
																				linetype = "solid"),
				panel.grid.major = element_line(linewidth = 0.1,
																				linetype = 1,
																				colour = "grey"),
				panel.grid.major.x = element_line(color = "grey",
																					size = 0.5,
																					linetype = 1),
				axis.title.y = element_text(angle = 90,
																		vjust = 0.5),
				aspect.ratio = 1) +
	scale_x_continuous(breaks = c(2010, 2025), expand = expansion(mult = c(0.05, 0.1))) +
	labs(color = "Species Temperature Index") +
	ggtitle("Species Temperature Index")
p

dir.create("output/figures/rli", recursive = TRUE, showWarnings = FALSE)
ggsave("./output/figures/rli/RLI_EuropeanButterflies2010_2025_SpeciesTemperatureIndex.jpg",
			 width = 6,
			 height = 4,
			 dpi = 150)

###############################
# Put all mean's etc together #
###############################
df_summary_speciestemperatureindex <- rbind(df_summary_VeryCold,
																						df_summary_Cold,
																						df_summary_Warm,
																						df_summary_VeryWarm) %>%
	mutate(
		n_specs = mean_difference * n * 5,
		label = paste0(SpeciesTemperatureIndex, " (n = ", n, ")")
	) %>%
	dubicube::add_effect_classification(
		cl_columns = c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0,
		coarse = FALSE
	)
df_summary_speciestemperatureindex

p <- ggplot(
	df_summary_speciestemperatureindex,
	aes(y = mean_difference,
			x = reorder(label, -mean_difference),
			ymin = lower_ci,
			ymax = upper_ci)
) +
	geom_errorbar(linewidth = 1.5,
								colour = "darkgrey") +

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
			 x = "Species Temperature Index"
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
ggsave("./output/figures/diff_rli/diffRLI_EuropeanButterflies2010_2025_SpeciesTemperatureIndex.jpg",
			 width = 12,
			 height = 5,
			 dpi = 150)
