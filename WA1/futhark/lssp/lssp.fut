-----------------------------------------
-- Parallel Longest Satisfying Segment --
-- Longest Satisfying Segment          --
-- ASSIGNMENT 1: fill in the blanks    --
--       See lecture notes             --
-- pred1 x   = p [x]                   --
-- pred2 x y = p [x,y]                 --
-----------------------------------------

type int = i32
let max (x:int, y:int) = i32.max x y

-- the task is to implement this operator by filling in the blanks
let redOp (pred2 : int -> int -> bool)
          (x: (int,int,int,int,int,int))
          (y: (int,int,int,int,int,int))
        : (int,int,int,int,int,int) =
  let (lssx, lisx, lcsx, tlx, firstx, lastx) = x
  let (lssy, lisy, lcsy, tly, firsty, lasty) = y

  let connect= if tlx == 0 || tly==0 then true else pred2 lastx firsty --connection between lastx and fisrsty [True, False]
  let newlss = if connect then max(max((lcsx+lisy), lssx), lssy) else max(lssx, lssy)--new longest satisfying segment
  let newlis = if connect && tlx == lisx then tlx+lisy else lisx --new longest initial segment
  let newlcs = if connect && tly == lcsy then tly+lcsx else lcsy --new longest concluding segment
  let newtl  = tlx+tly --new total list length
  let first  = if tlx == 0 then firsty else firstx
  let last   = if tly == 0 then lastx else lasty in
  (newlss, newlis, newlcs, newtl, first, last)

let mapOp (pred1 : int -> bool) (x: int) : (int,int,int,int,int,int) =
  let xmatch = if pred1 x then 1 else 0 in
  (xmatch, xmatch, xmatch, 1, x, x)

let lssp (pred1 : int -> bool)
         (pred2 : int -> int -> bool)
         (xs    : []int ) : int =
  let (x,_,_,_,_,_) =
        reduce (redOp pred2) (0,0,0,0,0,0) <|
        map (mapOp pred1) xs
  in  x

