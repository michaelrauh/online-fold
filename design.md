Design for Fold V 2.0

1. Read a single word from the stream. Remove punctuation and caps. Double newlines and periods are marked as text breaks.
2. Make the initial state object
    1. Center map - LHS center to boxes
        1. LHS center is all but the right column
    2. next map - map from word to set of following word
    3. previous map - map from word to set of previous word
    4. boxes - all previously found boxes mapped from dimensions to set of box.
        1. boxes are compound structures holding an array of arrays with the text of the overlaps contained, the RHS center (local center as the "new" box is considered to be joining from the left) and diagonals.
            1. diagonals is a list of set. Each set contains the words at index distance from top left corner.
            2. RHS center is all but the left column.
    5. phrases - a suffix tree with every phrase seen so far.
    5. raw - all words since the last break. Start is a break. Ideally this is a queue for fast addition to the end.
    6. increment - the new boxes that have not been sifted yet. Set of boxes.
3. Fold that word into a list of new “atoms” (2x2 boxes) and update indices. This will result in zero or an even number. They occur in pairs to account for rotation.
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
4. Hold an empty stack of pairs. Half of the pair is the ortho that has been found and the other is the current state of the scheduler. When a new box is found push it on and push scheduler state. Proceed by popping the top, asking the scheduler what is next and attempting that join. With this approach, the first 2x2 will beget higher shapes that can be returned to, and the scheduler will resume but not assume that higher blockers are there as they may move. Note: this scheduler does not yet exist. It will be based off of the rosette recursion scheme but must be refactored to take in a state and a result and return a next step.
    1. If the new shape is up a dimension (all twos) then trigger an up dimension transform
        1. Up dimension transforms will always act upon all-two boxes. (base dimension boxes)
        2. Start with new input box and check to make sure that each member of the box is a member of the forward map set. (andmap forward box contains other)
        3. Of remaining boxes, and together the set disjoint operation of each diagonal bucket in list with every other box. filter by this.
        4. Remaining boxes may be combined into a result type. RHS centers and diagonal buckets can be computed from previous RHS centers and diagonals.
            1. Computing diagonal buckets. drop first of LHS, drop last of RHS. set union at each list position, put dropped things back.
            2. Put the centers into a bigger array in order.
            3. Put data into a bigger array in order.
        5. Once these are combined update state to reflect. There will be entries missing from centers and boxes. Increment should be overwritten with these results. Specifically, the caller or driver of this combine needs to take boxes and frame that as an increment while updating centers and boxes.
        6. Update state to include all rotations of the new centers and boxes. All rotations mean all top left corner (or all diagonal) preserving rotations. In particular, swap the minor axis with every other one.
    2. If the new shape is to expand a dimension then trigger an extend transform
        1. The building blocks of the new thing will be something of the same dims but the minor dim will be one less. The transform increments the most minor dimension. 
            3. Combine the current with the other in the most minor axis.
                1. look up the center of the candidate against existing centers.
                2. Perform the next words check
                    1. Check in the phrase tree to see if each phrase along connection axis + new word on RHS is in the tree. Filter on this. Phrases can be found by reshaping the data array to be nbym where m is the size of the most minor axis and n is whatever it needs to be to preserve volume (volume over m)
                3. Any boxes that pass checks should be packed into result and returned in a list
                    1. Diagonals can be created from old diagonals. Shift the outers out and set union the inners.
            4. Once these are combined update state to reflect. There will be entries missing from centers and boxes. Increment should be overwritten with these results. Specifically, the caller or driver of this combine needs to take boxes and frame that as an increment while updating centers and boxes.
            5. Add in all rotations of boxes and centers.