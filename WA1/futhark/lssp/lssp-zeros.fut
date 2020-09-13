-- Parallel Longest Satisfying Segment
--
-- ==
-- compiled input {
-- [2, 42, 19, 54, 40, 56, 24, 0, 0,  0, 0, 38, 72, -7,  8, 25, 66, 23, 30, 50, 10, 65, 28, 36, 20, 46, 43, 62, 16, 71,  0, -9, 59, 0, 0, 0,  0, 0, 0, 14, 49, 21, -1,  3, 32, 0, 0, 79, -3, 55, 26, 52, 78, 17,  6, 64, 61, 41, 44,  7, 51, 70, 60,  9, 76, 22, 63, 31, 34, 58, 69, 48, 75, 73, -6, 29, 77, 74, 67, 27, 68, 15, 80, 45, 53, 18, 13,  4, 11, 35, 39]
-- }
-- output {
--    6
-- }


-- compiled input {
--    [1i32, -2, -2, 0, 0, 0, 0, 0, 3, 4, -6, 1]
-- }
-- output {
--    5
-- }

import "lssp-seq"
import "lssp"

type int = i32

let main (xs: []int) : int =
  let pred1 x   = (x == 0)
  let pred2 x y = (x == 0) && (y == 0)
--  in  lssp_seq pred1 pred2 xs
  in  lssp pred1 pred2 xs

