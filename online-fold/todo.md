1. Add a dynamo table to SAM that has orthos with a secondary index of dimensions
1. Create a fetcher that takes dimensions and gives back all ortho-data fields associated
1. Create a handler calling this fetcher with the dims of the current thing being combined 