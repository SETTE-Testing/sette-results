# Analysis of the experimental data

Data analysis was performed in the R statistical framework (https://www.r-project.org/).

## Data (CSV files)

The CSV files contain the extracted data from the experiments with the following columns:

* `Category`: the snippets were groupped into 6 major categories (B - Basic, S - Structures, O - Objects, G - Generics, L - Library, O - Other) and several subcategories (e.g. B2a IfElse in B2 - Conditionals). This is the identifier of the subcategory.
* `Snippet`: the name of the snippet method prefixed by the category, should be unique. There are 300 snippets.
* `Tool`: the name of the tool, possible values: CATG, EvoSuite, jPET, Pex, Randoop, SPF.
* `Coverage`: the statement coverage achieved by the tests generated by the tool.
* `Status`: for each execution one of the following statuses were assigned:
 * `N/A`: The tool was not able to perform test generation since the tool�s input could not have been specified for the execution or the tool signaled that it cannot deal with the certain code snippet.
 * `EX`: Test input generation was terminated by an exception, which was thrown by the code of the tool or the tool did not caught an exception thrown from the code snippet and stopped.
 * `T/M`: The tool reached the specified external time-out and it was stopped by force without result or the execution was terminated by an out of memory error. Note that if a tool stopped the execution itself, the result is categorized as NC or C instead.
 * `NC`: The tool has finished test input generation before time-out, however, the generated inputs have not reached the maximal possible coverage.
 * `C`: The tool has finished test input generation before time-out and the generated inputs have reached the maximal possible coverage. If an execution is classified into this category it means that the tool has generated appropriate inputs for the code snippet.
* `Size`: the number of tests (test inputs) generated by the tool for the given snippet (basically the number of JUnit test methods).
* `Run`: the identifier of the run in the format of `run-XX-30sec` where XX is a number from 01 to 10.
* `Duration`: time spent by the tool generating the tests, in miliseconds.
