# Functional-CLRS

In this project i take a functional and declarative approach to some of the algorithms in the CLRS Textbook. I will implement them in a purely functional language, namely Haskell. 

The motivation to do this, is to explore the functional programming design, and to see how short and concise and expressive these algorithms really can be made. 

I will copy the algorithms in terms of input and output whenever possible. I will also to the best of my ability mimic, the same pedogogical intuition in the implementation as CLRS, for example quickSort can be written short and sweet as ```qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]```[^1], but this does not clearly portray the approach intuitively, so i would not accept it.

For the *in-place* algorithms in the textbook, the output cannot be in-place as functional programming allow for no side-effects, such that the memory ineffeciency is not matched, and that sometimes the essence of am algoritm is not quite possible to recreate in a functional setting because of this, an example of this is the ```qsort``` function above.

[^1]: https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
