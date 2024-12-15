# Quicksort

This algorithm have been copied directly from the Haskell wiki[^1], as seen on the wiki this can be changed into a one-liner function without much loss of expressiveness. A few simple changes can also be introduced to optimize the function[^2], but i will not dwell on this, as a true quicksort is not meaningful in a functional setting, as the partition cannot happen in-place.

Length of code comparison (including spaces, excluding comments)

| CLRS Pseudocode | Haskell implementation | Python implementation[^1] |
|-----------------|------------------------|---------------------------|
| 249 chars       | 178 chars              | 460 chars                 |

[^1]: https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
[^2]: https://literateprograms.org/quicksort__haskell_.html
[^3]: https://www.geeksforgeeks.org/python-program-for-quicksort/
