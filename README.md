# vcfprocessor

## resources

* [mirror blog, typeclass derivation](https://blog.philipp-martini.de/blog/magic-mirror-scala3/)
* [enhancing dynamo db api, metaprogramming](https://melgenek.github.io/scala-3-dynamodb)
* [info about tupels, typelevel programming](https://www.scala-lang.org/2021/02/26/tuples-bring-generic-programming-to-scala-3.html)

The most convenient vcf parser, filter and transformer, using scala 3

Useful info about sets, union, intersection, exclusion, symmetric difference and Jaccard coefficient.

* https://www.youtube.com/watch?v=aTwRpqUnQX8

## Performance

We parse 15 MB by approx 2.6 seconds, without parallelizing. Extrapolated, we are able to parse approx 1 GB within
3 -> 4 min.

## Status

We now have the ability to parse most kinds of `List[String]` and `String` into tuples of any arity
and types. The next steps would be:

* Make a dump header feature, to be able to inspect the data formats
* Make it easy to generate a flat csv column database of the vcf, to simplify data analysis / querying
* Read more specifics about each column of the vcf line and implement new parsers for these
* Implement `map`, `filter`, `mapWithFilter`, `intersect`, `insert`, `select` as generic constructs
* Make a minimal math typeclass to calculate mean, variance, stddev, jaccard index etc.
* Make a minimal plot support, to plot values of events (ggplot2 (R) vs matplotlib (Python))

http://fernandoi.cl/blog/posts/altair/

Make a window function, that is able to merge variants, that are split up by the tool. Compare with
surrounding events.
