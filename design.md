Design for Fold V 2.0

1. Read a single word from the stream. Remove punctuation and caps. Double newlines and periods are marked as text breaks.
2. Make the initial state object
    1. Center map - RHS center to boxes
        1. RHS center is all but the left column
    2. next map - map from word to set of following word
    3. previous map - map from word to set of previous word
    4. boxes - all previously found boxes mapped from dimensions to set of box.
        1. boxes are compound structures holding an array of arrays with the text of the overlaps contained, the LHS center and diagonals.
            1. diagonals is a list of set. Each set contains the words at index distance from top left corner.
            2. LHS center is all but the right column.
    5. phrases - a suffix tree with every phrase seen so far.
    5. raw - all words since the last break. Start is a break. Ideally this is a queue for fast addition to the end.
    6. increment - the new boxes that have not been sifted yet. Set of boxes.
3. Fold that word into a list of new “atoms” (2x2 boxes) and update indices.
    1. Pass the new word and previous state. empty out raw if there has been a break, and seed it with the first word post-break. Empty out increment. A concept of a safe driver could help here.
    2. The word will be in the bottom left of any new 2x2 box created
    3. search for a new box looks like: d <- c <- a -> b -> d’
        1. <- means find LHS in back map and return all elements of set
        2. -> means find LHS in forward map and return all elements of set
        3. “Searching” for these things means finding the cross product of all of these sets. 
        4. Filter cross products such that b != c and d == d’
        5. reorder successes to be a, b, c, d and pack into result type.
            1. Result type will contain the raw nested data and
            2. RHS centers and
            3. current diagonal buckets (list of set indexed by distance from top left. This is hard coded at this dim to [{“a”}, {“b”, “c”} {“d”}]
        6. Return list of results
    5. Add word to next and prev maps
    6. Add word to the end of raw.
    6. Add raw to phrase suffix tree.
    8. Update center map with result boxes.
4. Ask the scheduler for required next shape. Feed one box in to this. Each box in the list will hit the scheduler in a loop until failure. Specifically, each time either a base 2x2 is made, and each time anything is joined, zero or multiple results may occur. If there are zero, return state back. If there are multiple, do a depth first search. Process the first thing as an addition until it comes back and then the second.
    1. If the new shape is up a dimension (all twos) then trigger an up dimension transform
        1. Up dimension transforms will always act upon all-two boxes. (base dimension boxes)
        2. Start with new input box and check to make sure that each member of the box is a member of the forward map set. (andmap forward box contains other)
        3. Of remaining boxes, and together the set disjoint operation of each diagonal bucket in list with every other box. filter by this.
        4. Remaining boxes may be combined into a result type. RHS centers and diagonal buckets can be computed from previous RHS centers and diagonals.
            1. Computing diagonal buckets. drop first of LHS, drop last of RHS. set union at each list position, put dropped things back.
            2. Put the centers into a bigger array in order.
            3. Put data into a bigger array in order.
        5. Once these are combined update state to reflect. There will be entries missing from centers and boxes. Increment should be overwritten with these results. Specifically, the caller or driver of this combine needs to take boxes and frame that as an increment while updating centers and boxes.
    2. If the new shape is to expand a dimension then trigger an extend transform
        1. Dig around in the previously found shapes to find one that is off by one. 
        2. Move the digit that is off by one to be on the most minor axis. This is the trickiest part with the most performance concern. There are also severe space complexity concerns here as this could duplicate data n-fold where n=number of dimensions. These building blocks are not canonical and so can’t be strongly cached. If they are to be cached you would need to always compare against canonical and update. hash the dimension map and if it has not changed, don’t do this. If it has changed, only add the ones that are new.
            1. swap update axis and last axis.
            2. Fix metadata
                1. diagonal buckets should be orientation independent. Leave that alone.
                2. Fix the centers. Not sure how to do this. This looks like a full recompute.
            3. look up the center of the candidate against existing centers. Mapping should be from center to set of boxes with that center.
            4. filter based upon diagonal buckets 
            5. Perform the next words check
                1. If this search exceeds trie depth, load this new depth from raw - slow but amortized
                2. Check in the phrase trie to see if each phrase along connection axis + new word on RHS is in trie. Filter on this.
            6. Any boxes that pass checks should be packed into result and returned in a list
            7. Once these are combined update state to reflect. There will be entries missing from centers and boxes. Increment should be overwritten with these results. Specifically, the caller or driver of this combine needs to take boxes and frame that as an increment while updating centers and boxes.

Idea: Use symbols instead of strings

Tricky bit: state will change during scheduling. Assume we are doing a depth first search.
Scenario:
1. Take in a single new word. This is the chosen one that will lead to a cascade of actions.
2. Combine the new word into 2x2s. Two of them result (spoiler, there will never be one due to rotations.) Increment = [first2x2, second2x2]
3. Take the first box from the increment and hit the scheduler with it. Scheduler will be in a "new" state and will attempt to find boundaries in the first dimension. Therefore it will try for 2x3. (In this system solving is right to left for convenience as opposed to left to right as in rosette for ease of understanding)
4. first2x2 enters solver for 2x3. This finds two results. [first2x3, second2x3].
5. first2x3 will hit the scheduler and the scheduler is still in searching first dimension mode. Therefore it will pass the first box to 2x4.
6. first2x3 fails to make a 2x4. It asks the scheduler what to do instead and the scheduler tells it to search 3x3 (first dimension is locked at 3 for this execution).
7. first2x3 will be transformed into first3x2 and will go for 3x3. For this use case it fails.
8. first2x3 is up against the ropes and tries for 2x2x2. It fails. first2x3 is dead and can grow no further. Control returns to step 4 to try second2x3.
9. second2x3 hits the scheduler. When we left off with this scheduler it was in a state of trying to find the size of the first dimension. 2x4 comes to mind. This makes it clear that the scheduler is recursively aware of the past but not the future.
10. second2x3 succeeds at 2x4. It will go for 2x5.

Takeaway: the current scheduler as implemented in rosette fold will work so long as search order is reversed to search from right to left instead of left to right. Scheduler holds state and that state comes along for each box.
When one box succeeds to the next level or fails for the next level that reflects only on its own level of recursion. The takeaway is that a new interface for the scheduler is needed. At a top level it needs some ability to take in a success or a failure for its current step and return the next step.
Right now the scheduler takes nothing in at the top level but passes around a plan and a history. It needs to be rewritten such that it takes in a success or failure flag and returns the next step. It will also need to take in scheduler state which is a plan and a history and perhaps a stage.

