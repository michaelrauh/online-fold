Design for Fold V 2.0

1. Read a single word from the stream. It should not have whitespace, punctuation, or caps. Breaks in text should be marked.
2. Hold a map from LHS centers to their boxes. This is the center map.
    1. LHS centers is the whole box except for the right hand colum
    2. RHS centers is the whole box except for the left hand column
3. Fold that word into a list of new “atoms” (2x2 boxes) and update indices.
    1. Pass the new word and the previous word. If this tuple has been passed or contains a text break, skip.
    2. Also pass a map from word to set of words that come after. Also pass the opposite of this map. 
    3. The word will be in the bottom left of any new 2x2 box created
    4. search for a new box looks like: d <- c <- a -> b -> d’
        1. <- means find LHS in back map and return all elements of set
        2. -> means find LHS in forward map and return all elements of set
        3. “Searching” for these things means finding the cross product of all of these sets. 
        4. Filter cross products such that b != c and d == d’
        5. reorder successes to be a, b, c, d and pack into result type.
            1. Result type will contain the raw nested data and
            2. RHS centers and
            3. current diagonal buckets (list of set indexed by distance from top left. This is hard coded at this dim to [{“a”}, {“b”, “c”} {“d”}]
        6. Return list of results
    5. Add word to forward and back edges
    6. Add word to phrase trie to depth = max discovered depth. Don’t add phrases with breaks.
    7. Add word to raw, a list of strings containing breaks to be read into the phrase trie later.
    8. Update center map with result boxes. (half of the centers will be in a map and the other half will be in metadata for fast asymmetric lookup)
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

Idea: Use convert each word to a symbol when it is read in

