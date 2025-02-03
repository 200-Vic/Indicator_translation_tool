#!/usr/bin/env r

#read all the lines of a file into  a character vector
x <- readLines("global.R")

#create regular expression with named matching groups
# discardA will match library(
# package will match somepackagename when it follows library(
# discardB will match ) when it follows library(somepackagename
package.rex <- "(?<discardA>library\\()(?<package>.+)(?<discardB>\\))"

# check each line for a reqular expression match
matches <- regexpr(package.rex, x, perl = TRUE)

#filter lines to just those with library(some_package)
x <-  x[matches != -1]

# starting position of package names
start <- attr(matches, "capture.start")[matches != -1,"package"]
#length of apckage names
l <- attr(matches, "capture.length")[matches != -1,"package"]

#arithmetic to get length of each package name
stop <-  start + l - 1

packages_used <- substr(x, start, stop)


#exclude any base packages
packages_to_install <- packages_used[!packages_used %in% installed.packages(priority = "high")[,1]]


install.packages(packages_to_install)


