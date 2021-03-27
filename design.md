Design for Fold V 2.0

1. Read a single word from the stream. Remove punctuation and caps. Mark any new paragraphs as a break and ensure there is only one break in a row.
2. Make the initial state object
    1. Center map - LHS center to boxes
        1. LHS center is all but the right column
    2. next map - map from word to set of following word
    3. previous map - map from word to set of previous word
    4. boxes - all previously found boxes mapped from dimensions to set of box.
        1. boxes are compound structures holding an array of arrays with the text of the overlaps contained, the RHS center and diagonals.
            1. diagonals is a list of set. Each set contains the words at index distance from top left corner.
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
4. Ask the scheduler for required next shape. Feed one box in to this. Each box in the list will hit the scheduler in a loop until failure.
    1. If the new shape is up a dimension (all twos) then trigger an up dimension transform
        1. Up dimension transforms will always act upon all-two boxes. (base dimension boxes) 
        2. Start with new input box. and together the set disjoint operation of each diagonal bucket in list with every other box. filter by this.
        3. Of remaining boxes, check to make sure that each member of the box is a member of the forward map set. (andmap forward box contains other)
        4. Remaining boxes may be combined into a result type. RHS centers and diagonal buckets can be computed from previous RHS centers and diagonals.
            1. Computing diagonal buckets. drop first of LHS, drop last of RHS. set union at each list position, put dropped things back.
            2. I can’t remember how to do this. Look up center tracking.
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

Idea: Use symbols instead of strings

