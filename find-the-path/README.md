# HackerRank __Find the Path__ problem

This problem is appropriately listed as difficult.  It took awhile to get the
logic to work in the first place.  Although the code worked, it was very slow.
I have taken several steps to speed the code up significantly. but it seems I
still need to optimize by several factors.


## What I Have Tried So Far

Originally, I was building paths as a list of locations.  This led me to
building the paths from back to front since list naturally support prepending.
Soon, I realized that a sequence was better than a list due to frequent updates
to the data.  This helped, but I potentially overused sequences in some
sub-steps.

I realized that doubling back (continuing a path directly ajacent to where the
path has already gone) couldn't possibly result in a lower weight.  As such, I
added logic to prevent that from happening.

When this improvement wasn't enough, I decided to keep a record of minimum
weights.  The record contains a minimum weight for each location on the entire
rectangular grid.  A minimum weight is weight of the most efficient path to that
location found so far.  Since the paths are built from back to front, these
paths started with the location of interest, and end with the destination
location.

Then, as new paths were generated, their weights could be compared to the
minWeights record.  If the weight of the path was better, it was kept (so it
continued to be included in path building), and the record was updated.
Otherwise, the the path would be disgarded.

I started doing some profiling around this point, and I realized that my
attempts to precache all of the calculations of the next locations had failed.
I figured out that it was basically a scope issue.  I fixed the issue, and got
the most significant improvement yet.

The `minWeights` record is a sequence of Ints where the index is a calculation
based on the location.  When the speed up (which was quite significant) was not
enough, I tried using an IntMap instead, but that proved to be slower, so I
reverted to the sequence.

I found that since adding in the `minWeights`, filtering out doubling back
routes (which was done before the paths are filtered based on weight) was no
longer necessary.  Rather, it was slowing things done, so I removed it.

Every path weight calculation was done over the entire path.  I sped things up
by a factor of 10 by updating the Path type to include the weight so the total
weight is calculated incrementally.


## The Numbers

Test case 1 is a 7 row by 100 column grid

The benchmark I have been using is the first test case that I failed (due to
timeout).  Intially, the time was unmesurablely long (I think I left it running
over night, and I never saw and answer pop out).  Once I did some of the basic
optimizations listed above, I was solving a single query in ~180s.  With the
rest of the optimizations listed above, I was able to get that down to ~40s.

By calculating weights incrementally, I was able to get the ~40s down to ~4s,
and this reduced to ~2.5s once the profiling was turned off.  However, this is
for a single query, and test case 1 has 1000 queries.  When run together, the
total time was ~3365s.

This is far away from my target of ~6s which I think is the HackerRank time
limit.


## Next Plan

Since the most efficient path contains the most efficient path between any two
locations on the path, this could be a basis for speeding up later queries.
However, this alone does not seem to be enough.

