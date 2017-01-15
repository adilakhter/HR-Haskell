lj = 2
h = 3

lj2 = 3
h2 = 14
-- expect 6 jumps

lj3 = 1
h3 = 3
-- expect 3 jumps

-- long jump -> height -> jumps
jumps :: Int -> Int -> Int
jumps 0 _ = 0
jumps height longJump
      | height < longJump = height
      | height >= longJump = 1 + jumps (height - longJump) longJump


example = jumps h lj
example2 = jumps h2 lj2

example3 = jumps h3 lj3
