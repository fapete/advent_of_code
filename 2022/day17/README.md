# Day 17

Part 1 only. I think solving part 2 would involve finding a height from which it cycles (that is, dropped tiles repeat). I do not have time to do that.

How I probably would try to:
1. Generate an infinite list of (shape, position) where position is the point the shape was settled
2. Maybe use a tortoise/hare approach to find a cycle in that list (would need some modification as we need to find the start index of the cycle, not just existence)
3. Compute height at the beginning of the cycle and the height of one period