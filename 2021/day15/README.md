Pretty inefficient implementation of dijkstra's algorithm doing none of the things you'd usually do to make it faster. Was sufficient for the task though (takes about 25s for both parts in sequence).

Easiest improvement would likely be to filter set of nodes to visit such that any node is discarded that
- already is more expensive than the current path, and
- has a higher manhattan-distance from the target (as lower bound for distance).

If I remember correctly that would basically turn this into A^*-Search.