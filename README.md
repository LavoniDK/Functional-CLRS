# Functional-CLRS

In this project i take a functional and declarativ approach to some of the algorithms in the CLRS Textbook. I will implement them in a purely functional language, namely Haskell. 

The motivation to do this, is to explore the functional programming design, and to see how short and concise these algorithms really can be made. 

I will copy the algorithms in terms of input and output whenever possible. I will also to the best of my ability mimic, the same pedogogical intuition in the implementation as CLRS, for example quickSort can be written short and sweet as ```qsortOneLine s = case s of{[]->[];(x:xs)->qsortOneLine [y | y<-xs, y<x] ++ x : qsortOneLine [y | y<-xs, y>=x]}```[^1], but this does not clearly portray the approach intuitively, so i would not accept it.

For the *in-place* algorithms in the textbook, the output cannot be in-place as functional programming allow for no side-effects, such that the memory ineffeciency is not matched, but maybe the aesthetics of the code will make up for it.

[^1]: https://stackoverflow.com/a/11501643