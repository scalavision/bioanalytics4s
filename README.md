# vcfprocessor

The most convenient vcf parser, filter and transformer, using scala 3

Useful info about sets, union, intersection, exclusion, symmetric difference and Jaccard coefficient.

* https://www.youtube.com/watch?v=aTwRpqUnQX8

## Status

We now have the ability to parse most kinds of `List[String]` and `String` into tuples of any arity
and types. The next steps would be:

* Read more specifics about each column of the vcf line and implement new parsers for these
* Implement `map`, `filter`, `mapWithFilter`, `intersect`, `insert`, `select` as generic constructs
* Make a minimal math typeclass to calculate mean, variance, stddev, jaccard index etc.
* Make a minimal plot support, to plot values of events (ggplot2 (R) vs matplotlib (Python))

http://fernandoi.cl/blog/posts/altair/