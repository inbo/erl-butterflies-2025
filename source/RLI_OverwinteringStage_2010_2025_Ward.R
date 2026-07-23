library(tidyverse)
library(scales)  # for colorRampPalette
library(grid)    # for unit()
library(INBOtheme)

conflicted::conflicts_prefer(dplyr::filter)

#############
# Read data #
#############
dfTraits <- read_delim("./data/tblTraitOverwinteringStage.csv",
											 delim = ";") %>%
	filter(Trait == "OverwinteringStage") %>%
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

#######
# Egg #
#######
df <- dfTraitRLC %>%
	filter(TraitValue == "Egg") %>%
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

rli_summary_Egg <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)
head(rli_summary_Egg)

rli_summary_Egg$OverwinteringStage <- "Egg"
head(rli_summary_Egg)

df_summary_Egg <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(overwinteringstage = "Egg") %>%
	select(overwinteringstage, n, lower_ci, mean_difference, upper_ci)
df_summary_Egg

###############
# Caterpillar #
###############
df <- dfTraitRLC %>%
	filter(TraitValue == "Caterpillar") %>%
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

rli_summary_Caterpillar <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_Caterpillar$OverwinteringStage <- "Caterpillar"
head(rli_summary_Caterpillar)

df_summary_Caterpillar <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(overwinteringstage = "Caterpillar") %>%
	select(overwinteringstage, n, lower_ci, mean_difference, upper_ci)
df_summary_Caterpillar

########
# Pupa #
########
df <- dfTraitRLC %>%
	filter(TraitValue == "Pupa") %>%
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

rli_summary_Pupa <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_Pupa$OverwinteringStage <- "Pupa"
head(rli_summary_Pupa)

df_summary_Pupa <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(overwinteringstage = "Pupa") %>%
	select(overwinteringstage, n, lower_ci, mean_difference, upper_ci)
df_summary_Pupa

#########
# Adult #
#########
df <- dfTraitRLC %>%
	filter(TraitValue == "Adult") %>%
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

rli_summary_Adult <- data.frame(
	year = c(2010, 2025),
	rli_mean = c(mean(rli_2010), mean(rli_2025)),
	rli_lower = c(quantile(rli_2010, 0.025), quantile(rli_2025, 0.025)),
	rli_upper = c(quantile(rli_2010, 0.975), quantile(rli_2025, 0.975))
)

rli_summary_Adult$OverwinteringStage <- "Adult"
head(rli_summary_Adult)

df_summary_Adult <- tibble(
	mean_difference = mean_diff,
	lower_ci = ci_diff[1],
	upper_ci = ci_diff[2],
	n = nrow(df_wide)
) %>%
	mutate(overwinteringstage = "Adult") %>%
	select(overwinteringstage, n, lower_ci, mean_difference, upper_ci)
df_summary_Adult

##########################
# Put all RLI's together #
##########################
mean_differences <- rbind(df_summary_Egg,
													df_summary_Caterpillar,
													df_summary_Pupa,
													df_summary_Adult) %>%
	# https://b-cubed-eu.github.io/dubicube/articles/effect-classification.html
	dubicube::add_effect_classification(
		c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0
	)
mean_differences

rli_summary_all <- rbind(rli_summary_Egg,
												 rli_summary_Caterpillar,
												 rli_summary_Pupa,
												 rli_summary_Adult)
rli_summary_all

# Data for the final year only
final_labels <- rli_summary_all %>%
	group_by(OverwinteringStage) %>%
	filter(year == max(year)) %>%
	ungroup() %>%
	left_join(
		mean_differences %>%
			select(overwinteringstage, effect_code),
		by = c("OverwinteringStage" = "overwinteringstage")
	)
final_labels

rli_summary_all$OverwinteringStage <- factor(rli_summary_all$OverwinteringStage,
																		levels = c("Egg",
																							 "Caterpillar",
																							 "Pupa",
																							 "Adult"))
# Without 1999
pd <- position_dodge(width = 1.5)

p <- ggplot(rli_summary_all,
						aes(x = year,
								y = rli_mean,
								group = OverwinteringStage,
								colour = OverwinteringStage)) +
	geom_point(size = 3,
						 position = pd) +
	geom_errorbar(aes(ymin = rli_lower,
										ymax = rli_upper,
										colour = OverwinteringStage,
										group = OverwinteringStage),
								width = 1,
								linewidth = 1,
								position = pd) +
	geom_line(size = 1,
						linetype = 1,
						position = pd) +
	ggrepel::geom_label_repel(
		data = final_labels,
		aes(label = as.character(effect_code), fill = OverwinteringStage),
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
	labs(color = "Overwintering stage") +
	ggtitle("Overwintering stage")
p

dir.create("output/figures/rli", recursive = TRUE, showWarnings = FALSE)
ggsave("./output/figures/rli/RLI_EuropeanButterflies2010_2025_OverwinteringStage.jpg",
			 width = 6,
			 height = 4,
			 dpi = 150)

###############################
# Put all mean's etc together #
###############################
df_summary_overwinteringstage <- rbind(df_summary_Egg,
																			 df_summary_Caterpillar,
																			 df_summary_Pupa,
																			 df_summary_Adult) %>%
	mutate(
		n_specs = mean_difference * n * 5,
		label = paste0(overwinteringstage, " (n = ", n, ")")
	) %>%
	dubicube::add_effect_classification(
		cl_columns = c("lower_ci", "upper_ci"),
		threshold = 0.02,
		reference = 0,
		coarse = FALSE
	)
df_summary_overwinteringstage

df_summary_overwinteringstage$overwinteringstage <- factor(df_summary_overwinteringstage$overwinteringstage,
																														levels = c("Adult",
																																			 "Pupa",
																																			 "Caterpillar",
																																			 "Egg"))

p <- ggplot(
	df_summary_overwinteringstage,
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
			 x = "Overwintering stage"
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
ggsave("./output/figures/diff_rli/diffRLI_EuropeanButterflies2010_2025_Overwinteringstage.jpg",
			 width = 12,
			 height = 5,
			 dpi = 150)
