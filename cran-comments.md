# Version 0.3.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

No reverse dependencies.

# Version 0.2.0

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

No reverse dependencies.

# Version 0.1.1, resubmission of version 0.1.0 

In this resubmission, I have adressed the following comments:

1. Some code lines in examples are commented out.
   * I changed the examples by wrapping them in tryCatch
2. Too many spaces in your description field. Probably because linebreaks count 
  as spaces too.
   * Lines of the description field of the DESCRIPTION file no longer go over 80
    characters. Also removed sets of consecutive space/linefeed.

It was also suggested that if there are references describing the methods in 
the package, to add these in the description field of the DESCRIPTION file.
There are no references related to this package, so none were added to the 
DESCRIPTION file.

# Version 0.1.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
