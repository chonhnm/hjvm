module Test.Lazy where

import Prelude hiding (init)

reqs = client init resps

resps = server reqs

-- must have "~" to delay pattern match of resps
client init ~(resp : resps) = init : client (next resp) resps

server (req : reqs) = process req : server reqs

-- init
init = 0

next resp = resp

process req = req + 1

-- run
fetchreq = take 10 reqs

fetchresp = take 11 resps

-- lazy fib
-- the entire left-hand side is a pattern, which is called pattern binding
-- pattern binding is implicitly lazy.
fibL :: [Integer]
fibL@(1 : tfib) = 1 : 1 : [a + b | (a, b) <- zip fibL tfib]

ll :: [(Integer, Integer)]
ll = [(x, y) | x <- [1, 2, 3], y <- [1, 2, 3], x /= y]

ll2 :: [(Integer, Integer)]
ll2 = do
  x <- [1, 2, 3]
  y <- [1, 2, 3]
  True <- return (x /= y)
  return (x, y)

ll3 =
  [1, 2, 3]
    >>= ( \x ->
            [1, 2, 3]
              >>= ( \y ->
                      return (x /= y)
                        >>= ( \r -> case r of
                                True -> return (x, y)
                                _ -> fail "failed!"
                            )
                  )
        )