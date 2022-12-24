# Day 24

This is a *horribly* inefficient solution, but whatever works, works.

How to improve:
- Cache the grid after *n* steps instead of storing it in the state
- An A^* search might improve things by preferring steps moving in the right direction, though it might also waste a lot of time in local optima