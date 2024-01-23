This folder contains code for Jenia Yudytska's doctoral dissertation project:
The influence of device affordances on linguistic variation: A three-part computational sociolinguistic study (2024, Universität Hamburg).
R was used for data collection, coding for microlinguistic features, and statistical analysis of results.

TBC: Neat, annotated script files which can be used by other linguists interested in these features.

Currently up (January 2024): "File dump" - all files used for the project, many of which have extraneous code as different methods of analysis were tried
(e.g. linear regression vs. Poisson regression for message length).

What is and will remain redacted/missing is the anonymisation code (C - Anonymise corpus.R) - this was simply done with gsub(), e.g.:
corpus$Content <- gsub("[actual username]", "Jonathan", corpus$Content, ignore.case = TRUE)

The name of the Discord bookblog community server is also redacted.
