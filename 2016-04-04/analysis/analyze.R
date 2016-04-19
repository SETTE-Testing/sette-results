#
# Commands used in analyzing the output from SETTE's experiment
# NOTE: this is not a script, this should be used interactively
#

#
# NOTE: SETTE assings a status for the executions from the following set:
#			N/A, EX, TM, NC, C
#		do not confuse N/A with NA (missing value in R)
#

# ------------ Load packages ---------------------------------------------------
library(ggplot2)

library(dplyr) 
# NOTE: plyr and dplyr are note the same! dplyr is a specialized version of plyr for data frames
# type vignette("introduction", package = "dplyr") to get a good introduction

library(tidyr)

# --------- Globals  -----------------------------------------------------------

TOOLS = c("CATG", "EvoSuite", "IntelliTest", "jPET", "Randoop", "SPF")
TOP_CATEGORIES = c("Basic", "Structures", "Objects", "Generics", "Library", "Oth.")
MAIN_CATEGORIES = c("B1", "B2", "B3", "B4", "B5", "B6", "S1", "S2", "S3", "S4", "O1", "O2", "O3", "O4", "G1", "G2", "L1", "L2", "L3", "L4", "LO", "Ot")
STATUSES = c("N/A", "EX", "T/M", "NC", "C")

# --------- Load manual CSV ----------------------------------------------------

# NOTE: CSV should not be Unicode
manual <- read.csv("data/snippetinputchecker-manual.csv", sep=",")


# --------- Load experiment CSVs -----------------------------------------------

r <- read.csv("data/sette-snippets_catg_evosuite_jpet_randoop_spf_run-1-10_30sec.csv", sep=",")

# change order of factor levels in Status
r$Status <- factor(r$Status, levels = STATUSES)

str(r)
# CHECK whether Snippet is a factor with 300 levels
# CHECK whether Size column is numeric and not factor
# CHECK whether Status levels are in the correct order

# --------- Load IntelliTest (Pex) -----------------------------------------------------------

intellitest = read.csv(file = "data/sette-snippets_intellitest_run1-10_30sec.csv", sep=",")

r <- rbind(r, intellitest)

# add missing two snippets for IntelliTest with N/A values for all 10 run
for(i in 1:10) {
	run <- ifelse(i<10, paste("run-0", i, "-30sec", sep=""), paste("run-", i, "-30sec", sep=""))
	r <- rbind(r, data.frame(Category="G1", Snippet="G1_guessTypeWithExtends", Tool="IntelliTest", Coverage=NA, Status="N/A", Size=NA, Run=run, Duration=NA))
	r <- rbind(r, data.frame(Category="G1", Snippet="G1_guessTypeWithExtendsAndUse", Tool="IntelliTest", Coverage=NA, Status="N/A", Size=NA, Run=run, Duration=NA))
} 
rm(i, run, intellitest)

# --------- Format data --------------------------------------------------------

# create new column with main category (e.g. B1a, B1b.. is B1)
r$MainCategory <- factor(substr(r$Category, 1, 2))
manual$MainCategory <- factor(substr(manual$Category, 1, 2))

# add top category (Basic, Structures...)
# FIXME: as Ot (Other) and OX (Objects) both begin with O need to hardcode
r$TopCategory[ r$MainCategory %in%c("B1", "B2", "B3", "B4", "B5", "B6") ] <- "Basic"
r$TopCategory[ r$MainCategory %in%c("S1", "S2", "S3", "S4") ] <- "Structures"
r$TopCategory[ r$MainCategory %in%c("O1", "O2", "O3", "O4") ] <- "Objects"
r$TopCategory[ r$MainCategory %in%c("G1", "G2") ] <- "Generics"
r$TopCategory[ r$MainCategory %in%c("L1", "L2", "L3", "L4", "LO") ] <- "Library"
r$TopCategory[ r$MainCategory == "Ot"] <- "Oth."

r$TopCategory <- factor(r$TopCategory, levels = TOP_CATEGORIES)

# reorder main categories according to order defined in paper
r$MainCategory <- factor(r$MainCategory, levels = MAIN_CATEGORIES)

# reorder tools
r$Tool <- factor(r$Tool, levels = TOOLS)

# convert Duration from ms to s
r$Duration <- r$Duration / 1000.0

# CHECK results
str(r)

# results with only C or NC status
r_nc_c <- filter(r, Status == "NC" | Status == "C")


# -------------------- Analyze: Overview----------------------------------------

# bird-eye view of the data, check values
summary(r)

# CHECK that every tool has 3000 observations (300 snippet * 10 run)

# CHECK where are NA's and why
#	executions with N/A, TM, EX hava NA for Coverage, Size
#		for Duration CATG it is NA (the tool did not start), for jPET and SPF it is a small value (tool started, then exited)



# --------------------- Analyze: Variability -----------------------------------

# check variability between different runs along the same snippet per each tool
for (t in TOOLS) {
	v_status <- (r %>% filter(Tool == t) %>% group_by(Snippet) %>% summarise(distinct = n_distinct(Status)) %>% filter(distinct > 1) %>% count())[[1]]
	v_size   <- (r %>% filter(Tool == t) %>% group_by(Snippet) %>% summarise(distinct = n_distinct(Size)) %>% filter(distinct > 1) %>% count())[[1]]
	v_cov    <- (r %>% filter(Tool == t) %>% group_by(Snippet) %>% summarise(mean(Coverage), sd = sd(Coverage)) %>% filter(sd > 0) %>% count())[[1]]
	
	print(paste("Variability / Nr. of different Status /", t, ":", v_status)) 
	print(paste("Variability / Nr. of different Size   /", t, ":", v_size)) 
	print(paste("Variability / Nr. of sd(Coverage) > 0 /", t, ":", v_cov)) 
}
rm(t, v_status, v_size, v_cov)


# ------------------------ Analyze: Status -------------------------------------

# See the status for each tool by runs
status <- r %>% select(Tool, TopCategory, MainCategory, Snippet, Run, Status) %>% spread(Run, Status)
status$Distinct <- apply(status[,5:14], 1, function(x) length(unique(x)))

# CHECK the ones with different statuses in runs
View(filter(status, Distinct > 1))

# TODO move status calculation to a function

# Assign the most frequent status to account for runs with different outcomes
#  C should only be assigned if more than half of the runs resulted in C
#  however as there were 10 runs, there can be ties
# FIXME: abuses that different outcomes only contain C and NC
#         rev() is used to reverse C,NC to NC,C and then which.max select NC when there is a tie
status$Status <- apply(status[,3:12], 1, function(x) names(which.max(rev(table(x)))))
status$Status <- factor(status$Status, levels = STATUSES)

# See the status counts for each tool
status %>% group_by(Tool, Status) %>% summarise(number=n())

# plot stacked bar charts of status
status %>% ggplot(data=., aes(x=Tool, y=..count..)) + geom_bar(aes(fill=Status)) + ylab("Number of snippets") + theme_bw()
# (Note: point plot with scale: not good, scale is too big in counts: 2--253)

# ----- tool-specific queries
# see NA for CATG
View(filter(status, Tool == "CATG", Status == "N/A"))

# see for EvoSuite the snippets that were not covered in any runs
View(filter(status, Tool == "EvoSuite", Distinct == 1, Status == "NC"))



# ------------------------ Analyze: Status Table and Visualization---------------

# calculate snippet numbers in categories
snippet_numbers <- manual %>% group_by(MainCategory) %>% summarise(totalNumber=n())
snippet_numbers$MainCategory <- factor(snippet_numbers$MainCategory, levels = MAIN_CATEGORIES)
snippet_numbers <- arrange(snippet_numbers, MainCategory)

statusByCategory <- status %>% group_by(Tool, TopCategory, MainCategory, Status) %>% summarize(number=n())

# normalize status counts to [0,1]
statusByCategory <- inner_join(statusByCategory, snippet_numbers, by = "MainCategory")
statusByCategory$Ratio <- statusByCategory$number / statusByCategory$totalNumber

#TODO add further old image tweakings 
# create stacked bar chart
statusByCategory %>% ggplot(data=.) + 
  geom_bar(aes(x = MainCategory, y = Ratio, fill = Status), stat = "identity") +
  facet_grid(Tool ~ TopCategory, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "RdYlGn", name = "Legend" ) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())

# create latex input for detailed status table
statusTable <- data.frame(Tool=statusByCategory$Tool, MainCategory=statusByCategory$MainCategory, Status=statusByCategory$Status, Number=statusByCategory$number)
statusTable <- spread(statusTable, MainCategory, Number)
apply( statusTable, 1, paste, collapse=" & " )


rm(statusByCategory, statusTable, snippet_numbers)

# ------------------------- Analyze: Coverage ----------------------------------

# use mean coverage per snippets across runs (to take into account variability in runs)

# see coverage per snippet by all tools -> see what is problematic for tools
# this is without N/A, EX, T/M
mean_cov_cat <- r_nc_c %>% group_by(Tool, Snippet, MainCategory) %>% summarize(c=mean(Coverage)) %>%
  group_by(MainCategory, Snippet) %>% summarise(MeanCoverage = mean(c))
manual_cov_cat <- manual %>% group_by(MainCategory) %>% summarise(MeanCoverage = mean(Coverage, na.rm=TRUE))

# the mean in the boxplot should be compared to the red dot, as the manual is also a mean value
qplot(data=mean_cov_cat, MainCategory, MeanCoverage, geom="boxplot") + geom_point(data=manual_cov_cat ,aes(y=MeanCoverage), color="red") + theme_bw() + xlab("Snippet categories") + ylab("Mean coverage [%]")


# plot mean coverage per tool
# this is with N/A, EX, T/M counted as 0%
cov <- r
cov$Coverage <- ifelse( (cov$Status == "N/A" | cov$Status == "T/M" | cov$Status == "EX"), 0, cov$Coverage)
mean_cov <- cov %>% group_by(Snippet, Tool) %>% summarise(MeanCoverage = mean(Coverage))  
qplot(data=mean_cov, x=MeanCoverage, geom="histogram", breaks=seq(0,110, by = 10)) + 
  facet_wrap(~Tool) + scale_x_continuous(breaks=seq(0,100, by = 10)) + 
  xlab("Mean coverage [%]") + ylab("Number of snippets") + theme_bw()

# descriptive statistics for coverage by tool
mean_cov %>% group_by(Tool) %>% summarise(min(MeanCoverage), mean(MeanCoverage), median(MeanCoverage), max(MeanCoverage), sd(MeanCoverage))
manual %>% group_by(Tool) %>% summarise(min(Coverage), mean(Coverage), median(Coverage), max(Coverage), sd(Coverage))

rm(mean_cov, mean_cov_cat, cov, manual_cov_cat)


#--------------------------- Analyze: Duration ---------------------------------

# this is without N/A,EX,T/M
#		use mean of duration per snippet accross 10 runs

# Pay attention that EvoSuite has values way higher 
#	SE tools 0-10sec, Randoop 30-60, EvoSuite: 30-200

# see variability of duration data, i.e. what is lost with using mean of 10 runs
mean_dur <- filter(r_nc_c, ! is.na(Duration)) %>% group_by(Snippet, Tool) %>% summarise(D=mean(Duration), sd=sd(Duration))

# histogram of durations per tool
mean_dur %>% ggplot(., aes(x=D)) + geom_histogram() + facet_wrap(~Tool) +
    geom_vline(xintercept = 30, linetype = "dashed") +
    xlab("Mean duration [s]") + ylab("Number of snippets") + theme_bw()

# sum of duration (averaged from sum of each run)
filter(r, ! is.na(Duration)) %>% group_by(Tool, Run) %>% summarise(d=sum(Duration)) %>% group_by(Tool) %>% summarise(md=mean(d))

# --- tool-specific analysis

# see which category is long for EvoSuite
evo_dur <- filter(r, Tool == "EvoSuite") %>% group_by(Snippet) %>% summarise(MeanDuration=mean(Duration))
evo_dur$MainCategory <- factor(substr(evo_dur$Snippet, 1, 2), levels = MAIN_CATEGORIES)
qplot(data=evo_dur, x=MainCategory, y=MeanDuration) + 
  theme_bw() + ylab("Mean duration [s]") + xlab("Snippet category")

# see number of snippets reaching timeout for Randoop
filter(mean_dur, Tool == "Randoop", D >= 30)

rm(mean_dur, evo_dur)

#--------------------------- Analyze: Size --------------------------------

# this is without N/A,EX,T/M

# descriptive statistics
mean_size <- r_nc_c %>% group_by(Snippet, Tool) %>% summarize(S=mean(Size), sd=sd(Size))
mean_size %>% group_by(Tool) %>% summarize(min(S), mean(S), median(S), max(S), sd(S), sum(S))
manual %>% group_by(Tool) %>% summarize(min(Size), mean(Size), median(Size), max(Size), sd(Size), sum(Size))

# compare manual sample input size and tool generated size, but only for C
r_c <- filter(r, Status == "C")
s_diff <- r_c %>% group_by(Snippet, Tool) %>% summarize(S=mean(Size))
s_diff <- left_join(s_diff, select(manual, Snippet, Size), by="Snippet")
s_diff$Diff <- s_diff$S - s_diff$Size

ggplot(s_diff, aes(x = s_diff$Tool, y = s_diff$Diff)) + geom_boxplot() +
  theme_bw() + ylim(-1,30) + xlab("Tool") + ylab("Difference vs. sample inputs")
    
# --- tool-specific analysis

# plot size vs. duration for Randoop
randoop_d_s <- filter(r, Tool == "Randoop") %>% group_by(Snippet, TopCategory) %>% summarise(d=mean(Duration), s=mean(Size))
ggplot(data = randoop_d_s, aes(x = d, y = s, color = TopCategory)) + geom_point(size = 1.5) + 
  theme_bw() + xlab("Duration [s]") + ylab("Size") + scale_color_discrete(name="Category")

rm(r_c, s_diff, mean_size, randoop_d_s)


#--------------------------- Analyze: Performance/time --------------------------------

p <- read.csv("data/performance-time.csv")
p$Duration <- p$Duration / 1000.0
p$MainCategory <- factor(substr(p$Category, 1, 2))

# add the results for these 129 snippets from the 30sec experiment
run30 <-  filter(r, Snippet %in% unique(p$Snippet) ) 
run30$TopCategory <- NULL
p <- rbind(p, run30)

# create timelimit column
p <- data.frame(p, do.call(rbind, strsplit(as.character(p$Run),'-'))[,3] )
names(p)[10] <- "TimeLimit"
p$TimeLimit <- do.call(rbind, strsplit(as.character(p$TimeLimit),'sec'))[,1]
p$TimeLimit <- as.numeric(p$TimeLimit)
# strip time from end of run id to have the same run ids for each timelimit
p$Run <- substr(p$Run, 1, 6)

# CHECK whether different runs resulted in different statuses
View(p %>% group_by(Tool, Snippet) %>% summarise(distinct = n_distinct(Status)) %>% filter(distinct > 1))
# CHECK where is it affecting C status
View( filter(p, Status=="C") %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(C=n()) %>% filter(C < 10) )

# summary views
status_by_time <- p %>% group_by(TimeLimit, Tool, Run, Status) %>% summarise(n=n()) %>% group_by(TimeLimit, Tool, Status) %>% summarise(m=mean(n), sd=sd(n))
status_by_time_wide <- status_by_time %>% select(TimeLimit, Tool, Status, m)  %>% spread(TimeLimit, m)

# set missing coverage to 0%
p$Coverage <- ifelse( (p$Status == "N/A" | p$Status == "T/M" | p$Status == "EX"), 0, p$Coverage)

# calculate mean coverage, size, number of C status along the runs with different time limits
p_cov  <- p %>% filter(! is.na(Coverage)) %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(c=mean(Coverage)) %>% group_by(Tool, TimeLimit) %>% summarise(Coverage=mean(c))
p_size <- p %>% filter(! is.na(Coverage)) %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(s=mean(Size)) %>% group_by(Tool, TimeLimit) %>% summarise(Size=mean(s))

# proper C status calculation (more than 5 out of 10)
p_status <- p %>% select(Tool, Snippet, TimeLimit, Run, Status) %>% spread(Run, Status)
p_status$Distinct <- apply(p_status[,4:13], 1, function(x) length(unique(x)))
p_status$Status <- apply(p_status[,4:13], 1, function(x) names(which.max(rev(table(x)))))
p_status$Status <- factor(p_status$Status, levels = STATUSES)

p_c_status <- filter(p_status, Status=="C") %>% select(Tool, Snippet, TimeLimit, Status) %>% group_by(Tool, TimeLimit) %>% summarise(C=n())

# create input for latex table
p_cov$Coverage <- round(p_cov$Coverage, digits = 2)
apply( spread(p_cov, TimeLimit, Coverage), 1, paste, collapse=" & " )
p_size$Size <- round(p_size$Size, digits = 2)
apply( spread(p_size, TimeLimit, Size), 1, paste, collapse=" & " )
apply( spread(p_c_status, TimeLimit, C), 1, paste, collapse=" & " )

p1 <- p_status %>% select(Tool, Snippet, TimeLimit, Status)
p2 <- p %>% select(Tool, Snippet, TimeLimit, Duration, Run) %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(d=mean(Duration))
p_dur <- inner_join(p1, p2)

# see durations with max time limit
#  30 < duration < 300 are those, which we miss in the 30sec experiments
#  (except for EvoSuite and Randoop, which mostly use the allocated maximum time)
p_dur %>% filter(TimeLimit == "300") %>%
  qplot(data=., x=d, geom="histogram", fill=Status) + facet_wrap(~Tool) +
  geom_vline(xintercept = 30, linetype = "dashed") +
  theme_bw() + xlab("Duration of test generation [s]") + ylab("Number of snippets")

# --- tool-specific analysis

# EvoSuite: search for snippet that changed status
evo_p_s <- filter(p_status, Tool=="EvoSuite") %>% select(Tool, Snippet, TimeLimit, Status) %>% spread(TimeLimit, Status)
View(evo_p_s[evo_p_s$`15` != evo_p_s$`300`,])

rm(evo_p_s, p_dur, status_by_time, status_by_time_wide, p_status, p1, p2, p, p_cov, p_size, p_c_status)


# -------------------------------- Extra snippets -------------------

extra <- read.csv("data/snippets-extra-catg-evosuite-jpet-randoop-spf.csv")
extra$Status <- factor(extra$Status, levels = STATUSES)
extra$Tool <- factor(extra$Tool, levels = TOOLS)

status_extra <- extra %>% select(Tool, Snippet, Run, Status) %>% spread(Run, Status)
status_extra$Status <- apply(status_extra[,3:12], 1, function(x) names(which.max(rev(table(x)))))
status_extra$Status <- factor(status_extra$Status, levels = STATUSES)
status_extra %>% group_by(Tool, Status) %>% summarise(n=n())

