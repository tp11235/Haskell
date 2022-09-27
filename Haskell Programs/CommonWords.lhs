11:58 19/01/201711:59 19/01/201711:59 19/01/201712:01 19/01/201712:01 19/01/2017The Common Words program to find and count the words in a text file down to length n

>import Data.List
>import Data.Function
>import Data.Ord

>commonWords :: String -> [(Int,String)]
>sortRuns :: [(Int,String)] -> [(Int,String)]
>countRuns :: [String] -> [(Int,String)]
>makeSortLC :: String -> [String]

>makeSortLC x = sort (words x)
>countRuns [] = []
>countRuns (word:listOfRemainingWords) = (1+length listOfSameWord,word):countRuns restOfList
>               where (listOfSameWord,restOfList) = span (==word) listOfRemainingWords

The span function splits a list into two parts: the first element and any adjacent elements that are the same and the rest of the list.
It obviously needs the list to be sorted!
The syntax (word:listOfWords) is equivalent to [word] but defines the list in terms of the first element and the rest of the list which we need later
It outputs a list created from elements (x,y) where x is the length of a list of the same word plus 1 (because counting starts at 0)
and y is the word itself. The structure (1+length listOfSameWord,word):countRuns restOfList builds a list recursively
by creating the (1+length listOfSameWord,word) element from the span function below then appending the result of calling the function again
on the remainder of the list after span has pulled out the first run of words.

Now sort the list of tuples

>sortRuns x = (reverse.sortBy (comparing fst)) x
>commonWords = sortRuns.countRuns.makeSortLC