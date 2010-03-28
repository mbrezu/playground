
# Timetable - experiments with timetabling algorithms in Clojure

## Dependencies

 * Beak-Check for unit tests.

## TODO

 * Sanitizer - checks if the allocation of the activities in the
   schedule is actually possible (e.g. a resource doesn't have less
   timeslots than unscheduled activities);
 * More constraints;
 * Parallelize `allocate-first`.

## Description

This should evolve into a small timetabling program.

The algorithm is fairly simple, but the data structures involved in a
functional implementation are quite complicated. This should be a good
way to learn more about how Clojure data structures work and if their
performance is good enough.