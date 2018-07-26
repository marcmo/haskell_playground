Abstract Absurdities

Monday, February 19, 2007
Sussman, Robustness, & Quickcheck
Today on reddit there was a link to a paper by Gerald Sussman about how to construct robust systems.
If I understood correctly, his thesis is that one can only gain the robustness of biological systems by emulating their development: evolution and exploratory development. Not only that, but an emphasis on runtime tests and checks can help catch problems and further improve the strength of the system.

The idea makes a lot of sense when I think about it, but I have a feeling a lot of people who read it are going to take it as an argument for dynamically typed tools. It may have even been intended as such, but I do not presume to know the mind of Prof. Sussman.

On the other hand, I think a truly powerful type system may in fact help with exploratory programming and test-driven programming. When reading the paper I couldn't help but think of the beautiful Haskell system, QuickCheck. QuickCheck actually comes with GHC, so if you're using that compiler then you don't have to worry about installing anything else.

I actually think it'd be nice to have another example than the intro to the wiki, so I'm including my own stupid one: attempting to verify that a breadth-first search doesn't suck given that I already have some search that I know works.


>import Test.QuickCheck

>data Tree a = Node a | Branch (Tree a) a (Tree a)
>       deriving Show


So here we've defined a very simple binary tree, yay!


>dfs :: Eq a => a -> Tree a -> Bool
>dfs a (Node x) = a==x
>dfs a (Branch t x t') | x==a = True
>                      | otherwise = dfs a t || (dfs a t')

>bfs :: Eq a => a -> Tree a -> Bool
>bfs a tree = bfh tree []
>    where bfh (Node x) [] = x==a
>          bfh (Node x) (l:ls) = if x==a then True else bfh l ls
>          bfh (Branch t x t') l = if x==a 
>                                   then True 
>                                   else bfh (head l') (tail l')
>            where l' = l++[t,t']


Now we have two very simple forms of search. Let's now assume that God himself has descended and told us that our depth first search is The Awesome but we are worried that the breadth first search doesn't work. QuickCheck to the rescue!


>instance (Arbitrary a) => Arbitrary (Tree a) where
>    arbitrary = oneof [do
>                         x <- arbitrary                   
>                         return (Node x),
>                       do
>                         t <- arbitrary   
>                         t' <- arbitrary   
>                         x <- arbitrary   
>                         return (Branch t x t')]

>prop_Search t x = dfs x t ==> bfs x t


So here we've defined an instance of the Arbitrary type for our binary tree, and the property we check is that if the depth first search for a random element in a randomly generated tree is true then the breadth first search for that same element in that same tree must be true.

Of course I think we can go a lot further than this when it comes to exploratory programming and testing. I personally believe that a very strong static type system can help create a very tidy embedded language for genetic programming, but all I have are some very old toy examples on that front. Who knows how well we can do if we invoke Conor's Law?
Posted by Creighton Hogg at 7:38 PM
Labels: haskell
3 comments:

Tom said...

    Tom Moertel (http://blog.moertel.com/) here, posting from who-knows-what Blogger account. Anyway...

    As your QC property is currently defined,

    prop_Search t x = dfs x t ==> bfs x t

    you're only testing that (bfs x t) returns True when (dfs x t) does, which probably isn't what you want. For example, you can replace (bfs x t) with the constant True, which certainly is a correct breadth-first-search implementation, and the property checks:

    prop_Search t x = dfs x t ==> True

    *Test> quickCheck prop_Search
    OK, passed 100 tests.

    Try using a straight equality test instead:

    prop_Search' t x = dfs x t == bfs x t

    *Test> quickCheck prop_Search'
    OK, passed 100 tests.


    Also, you might want to monitor the sizes of the trees tested, just to make sure you're actually testing non-trivial trees. Something like this ought to do it:

    size :: Tree a -> Int
    size t = size' 0 t where
    size' n (Node _) = 1 + n
    size' n (Branch l x r) = size' (size' (1 + n) l) r

    prop_Search' t x =
    label (show (size t)) $
    dfs x t == bfs x t

    *Test> quickCheck prop_Search'
    OK, passed 100 tests.
    54% 1.
    11% 3.
    6% 5.
    4% 7.
    4% 15.
    3% 11.
    2% 9.
    2% 13.
    1% 83.
    1% 53.
    1% 511.
    1% 47.
    1% 43467.
    1% 43.
    1% 415.
    1% 35.
    1% 299.
    1% 25.
    1% 21.
    1% 199.
    1% 17.
    1% 105.


    Cheers,
    Tom
    February 21, 2007 10:27 AM 
Tom said...

    Whoops. Typo in my previous post. I meant to write this: "For example, you can replace (bfs x t) with the constant True, which certainly is *not* a correct breadth-first-search implementation ..."
    February 21, 2007 10:31 AM 
Creighton Hogg said...

    Thanks Tom, you're right. I thought that doing
    dfs x t ==> bfs x t
    would be a cute way to show that feature of QuickCheck but you're quite correct.
    February 21, 2007 2:36 PM 

Post a Comment
Newer Post Home
Subscribe to: Post Comments (Atom)

