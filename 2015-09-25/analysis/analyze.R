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

# --------- Globals  -----------------------------------------------------------

TOOLS = c("CATG", "EvoSuite", "jPET", "Pex", "Randoop", "SPF")


# --------- Load manual CSV ----------------------------------------------------

# NOTE: CSV should not be Unicode
manual <- read.csv("data/sample-manual-inputs.csv", sep=",")


# --------- Load experiment CSVs -----------------------------------------------

r <- read.csv("data/10runs-30sec-catg_evosuite_jpet_randoop_spf.csv", sep=",")

# change order of factor levels in Status
r$Status <- factor(r$Status, levels = c("N/A", "EX", "T/M", "NC", "C"))

str(r)
# CHECK whether Snippet is a factor with 300 levels
# CHECK whether Size column is numeric and not factor
# CHECK whether Status levels are in the correct order

# --------- Load Pex -----------------------------------------------------------

pex = read.csv(file = "data/10runs-30sec-pex.csv", sep=",")

r <- rbind(r, pex)

# add missing two snippets for Pex with N/A values for all 10 run
for(i in 1:10) {
	run <- ifelse(i<10, paste("run-0", i, "-30sec", sep=""), paste("run-", i, "-30sec", sep=""))
	r <- rbind(r, data.frame(Category="G1", Snippet="G1_guessTypeWithExtends", Tool="Pex", Coverage=NA, Status="N/A", Size=NA, Run=run, Duration=NA))
	r <- rbind(r, data.frame(Category="G1", Snippet="G1_guessTypeWithExtendsAndUse", Tool="Pex", Coverage=NA, Status="N/A", Size=NA, Run=run, Duration=NA))
} 
rm(i, run, pex)

# --------- Format data --------------------------------------------------------

# create new column with main category (e.g. B1a, B1b.. is B1)
r$MainCategory <- factor(substr(r$Category, 1, 2))
manual$MainCategory <- factor(substr(manual$Category, 1, 2))

# reorder main categories according to order defined in paper
r$MainCategory <- factor(r$MainCategory, levels = c("B1", "B2", "B3", "B4", "B5", "B6", "S1", "S2", "S3", "S4", "O1", "O2", "O3", "O4", "G1", "G2", "L1", "L2", "L3", "L4", "LO", "Ot"))

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

# Variability along the 10 runs (30sec)
#				    status		size	coverage
#	CATG: 		0			    0     0
#	EvoSuite: 42			  109   49
#	jPET:     0			    0     0
#	Randoop:  0         16    0
#	PEX:      0         1     1
#	SPF:      0         0     0

# analyize differences in EvoSuite
es_status <- r %>% filter(Tool == "EvoSuite") %>% group_by(Snippet) %>% summarise(distinct = n_distinct(Status)) %>% filter(distinct > 1)

for (i in 1:10) {
	run <- ifelse(i<10, paste("run-0", i, "-30sec", sep=""), paste("run-", i, "-30sec", sep=""))
	run_i <- select(filter(r, Tool=="EvoSuite", Run==run), Snippet, Status)
	names(run_i) <- c("Snippet", paste("Status_", i, sep=""))
	
	es_status <- left_join(es_status, run_i, by="Snippet")
}
rm(i, run, run_i)

# count how many C-s were for each snippet (each row)
es_status$Nr_C_status <- apply((es_status == "C")[,3:12], 1, FUN = sum)

# CHECK es_status
View(es_status)

# ------------------------ Analyze: Status -------------------------------------

# work with run-01-30sec for status, except EvoSuite it is the same, just fewer points in the plots
run1 <- filter(r, Run == "run-01-30sec")

# See the status counts by category for each tool
for (tool in TOOLS){
	t <- tally(group_by(filter(run1, Tool == tool), MainCategory, Status))
	write.csv( t, file=paste("output/", tool, "_status.csv", sep="") )
}

# plot stacked bar charts of status
run1 %>% ggplot(data=., aes(x=Tool, y=..count..)) + geom_bar(aes(fill=Status)) + ylab("Number of snippets") + theme_bw()
# (Note: point plot with scale: not good, scale is too big in counts: 2--253)

rm(run1, tool, t)

# ------------------------- Analyze: Coverage ----------------------------------

# use mean coverage per snippets across runs (to take into account EvoSuite runs)

# plot coverage per snippet by all tools -> see what is problematic for tools
# this plot is without N/A, EX, T/M
mean_cov_cat <- r_nc_c %>% group_by(MainCategory, Snippet) %>% summarise(MeanCoverage = mean(Coverage, na.rm=TRUE))
manual_cov_cat <- manual %>% group_by(MainCategory) %>% summarise(MeanCoverage = mean(Coverage, na.rm=TRUE))
qplot(data=mean_cov_cat, MainCategory, MeanCoverage, geom="boxplot") + geom_point(data=manual_cov_cat ,aes(y=MeanCoverage), color="red") + theme_bw() + xlab("Snippet categories") + ylab("Mean coverage [%]")

# plot average coverage per tool
# this is with N/A, EX, T/M counted as 0%
m <- r
m$Coverage <- ifelse( (m$Status == "N/A" | m$Status == "T/M" | m$Status == "EX"), 0, m$Coverage)
mean_cov <- m %>% group_by(Snippet, Tool) %>% summarise(MeanCoverage = mean(Coverage))  
qplot(data=mean_cov, x=MeanCoverage, geom="histogram", breaks=seq(0,110, by = 10)) + facet_wrap(~Tool) + xlab("Mean coverage [%]") + ylab("Count") + scale_x_continuous(breaks=seq(0,100, by = 10)) + theme_bw()

# descriptive statistics for coverage by tool
mean_cov %>% group_by(Tool) %>% summarise(min(MeanCoverage), mean(MeanCoverage), median(MeanCoverage), max(MeanCoverage), sd(MeanCoverage))

rm(mean_cov, mean_cov_cat, m, manual_cov_cat)


#--------------------------- Analyze: Duration ---------------------------------

# plots are without N/A,EX,T/M
#		use mean of duration per snippet accross 10 runs

# Pay attention that EvoSuite has values way higher 
#	SE tools 0-10sec, Randoop 30-40, EvoSuite: 100-600

mean_dur <- r_nc_c %>% group_by(MainCategory, Snippet, Tool) %>% summarise(MeanDuration=mean(Duration))

# plot duration per tool
mean_dur %>% ggplot(., aes(Tool, MeanDuration)) + geom_boxplot() + ylab("Mean duration [s]") + ylim(0, 60) + theme_bw()

rm(mean_dur)


#--------------------------- Analyze: Size --------------------------------

# plots are only for runs with C

r_c <- filter(r, Status == "C")

# descriptive statistics
r_c %>% group_by(Snippet, Tool) %>% summarize(S=mean(Size)) %>% group_by(Tool) %>% summarize(min(S), mean(S), median(S), max(S), sd(S))

# plot between sample input size and tool size
s_diff <- r_c %>% group_by(Snippet, Tool) %>% summarize(S=mean(Size))
s_diff <- left_join(s_diff, select(manual, Snippet, Size), by="Snippet")
s_diff$Diff <- s_diff$S - s_diff$Size

ggplot(s_diff, aes(x = s_diff$Tool, y = s_diff$Diff)) + ylim(-1,30) + geom_boxplot() + xlab("Tool") + ylab("Difference vs. sample inputs") + geom_abline(intercept=0, slope=0, color="red", linetype="dashed") + theme_bw()

rm(r_c, s_diff)

#--------------------------- Analyze: Performance/time --------------------------------

p <- read.csv("data/performance-time-evosuite_randoop.csv")

# add the results for these 129 snippets from the 30sec experiment
run30 <-  filter(r, Tool=="Randoop" | Tool=="EvoSuite", Snippet %in% unique(p$Snippet) ) 
run30$MainCategory <- NULL
p <- rbind(p, run30)

# create timelimit column
p <- data.frame(p, do.call(rbind, strsplit(as.character(p$Run),'-'))[,3] )
names(p)[9] <- "TimeLimit"
p$TimeLimit <- do.call(rbind, strsplit(as.character(p$TimeLimit),'sec'))[,1]
p$TimeLimit <- as.numeric(p$TimeLimit)
p <- filter(p, TimeLimit != "120", TimeLimit != "180")

# calculate mean coverage, size, number of C status along the runs with different time limits
p_cov <- p %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(c=mean(Coverage, na.rm=TRUE)) %>% group_by(Tool, TimeLimit) %>% summarise(Coverage=mean(c))
p_size <- p %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(s=mean(Size, na.rm=TRUE)) %>% group_by(Tool, TimeLimit) %>% summarise(Size=mean(s))
p_c_status <- filter(p, Status=="C") %>% group_by(Tool, Snippet, TimeLimit) %>% summarise(C=n()) %>% group_by(Tool, TimeLimit) %>% summarise(C=n())

# use the 30sec experiment as a baseline, normalize the other runs
# HACK refactor this
p_sum <- p_cov
p_sum$Size <- p_size$Size
p_sum$C <- p_c_status$C

p_sum2 <- data.frame(Tool=factor(), TimeLimit=integer(), Coverage=double(), Size=double(), C=integer())

for(t in c("EvoSuite", "Randoop")){
  baseline <- filter(p_sum, Tool==t, TimeLimit==30)
  p_t <- filter(p_sum, Tool==t)
  p_t$Coverage <- p_t$Coverage / baseline$Coverage
  p_t$Size <- p_t$Size / baseline$Size
  p_t$C <- p_t$C / baseline$C
  p_sum2 <- rbind(p_sum2, p_t)
}

ggplot(data=p_sum2) + geom_point(aes(x=TimeLimit,y=Coverage,colour="Coverage")) +
  geom_point(aes(x=TimeLimit,y=Size, colour="Size")) +
  geom_point(aes(x=TimeLimit,y=C, colour="Status (C)")) +
  geom_abline(intercept=1, slope=0, color="green", linetype="dashed") +
  theme_bw() +facet_grid(~Tool) + ylim(0.9, 1.1) + ylab("Ratio") + xlab("Time limit [s]")


rm(run30, p_cov, p_size, p_c_status, p_sum, p_sum2, t, p_t)
