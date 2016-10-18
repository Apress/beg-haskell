module Chapter7.MoreThanOneValue where

brokenJump :: Integer -> [Integer]
brokenJump y = [y-1, y+3, y+5]

brokenThreeJumps :: Integer -> [Integer]
brokenThreeJumps y = do firstJ  <- brokenJump y
                        secondJ <- brokenJump firstJ
                        brokenJump secondJ

brokenJumps :: Integer -> Integer -> [Integer]
brokenJumps n year = brokenJumps' n [year]
  where brokenJumps' 0 years = years
        brokenJumps' i years = years >>= brokenJumps' (i-1) . brokenJump
