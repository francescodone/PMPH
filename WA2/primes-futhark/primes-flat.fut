-- Primes: Flat-Parallel Version
-- ==
-- compiled input { 30 } output { [2,3,5,7,11,13,17,19,23,29] }
-- compiled input { 10000000i32 } auto output



let segmented_scan [n] 't (op: t -> t -> t) (ne: t) (flags : [n]i32) (arr : [n]t): [n]t =
  let flags_vals =
    scan (\ (f1, x1) (f2, x2) ->
            let f = f1 | f2 in
            if f2 > 0 then (f, x2)
            else (f, x1 `op` x2))
         (0, ne) (zip flags arr)
  let (_, vals) = unzip flags_vals
  in vals



let mkFlagArray [m] (aoa_shp : [m]i32) (zero : i32) (aoa_val : [m]i32) : []i32 =
        let shp_rot = map (\i->if i==0 then 0
                                        else aoa_shp[i-1]
                        ) (iota m)
        let shp_scn = scan (+) 0 shp_rot
        let aoa_len = shp_scn[m-1] + aoa_shp[m-1]
        let shp_ind = map2 (\ shp ind ->
                                if shp==0 then -1
                                        else ind
                        ) aoa_shp shp_scn
        let rpl = replicate aoa_len zero
        in scatter (rpl) shp_ind aoa_val


let primesFlat (n : i32) : []i32 =
  let sq_primes   = [2,3,5,7]
  let len  = 8
  let (sq_primes, _) =
    loop (sq_primes, len) while len < n do
      -- this is "len = min n (len*len)"
      -- but without running out of i32 bounds
      let len = if n / len < len then n else len*len

      let mult_lens = map (\ p -> (len / p) - 1 ) sq_primes
      let flat_size = reduce (+) 0 mult_lens

      --------------------------------------------------------------
      -- The current iteration knowns the primes <= 'len',
      --  based on which it will compute the primes <= 'len*len'
      -- ToDo: replace the dummy code below with the flat-parallel
      --       code that is equivalent with the nested-parallel one:
      --   let composite = map (\ p -> let mm1 = (len / p) - 1
      --                               in  map (\ j -> j * p ) (map (+2) (iota mm1))
      --                       ) sq_primes
      --   let not_primes = reduce (++) [] composite
      --
      -- Your code should compute the right `not_primes`
      -- Please look at the lecture slides L2-Flattening.pdf to find
      --  the normalized nested-parallel version.
      -- Note that the scalar computation has already been distributed
      --  and the result is in "mult_lens", but it stores `len / p - 1`,
      --  where `p \in sq_primes`.
      --
      let ms = map (\ p->len/p) sq_primes
      let mm1s = map (\ m->m-1) ms
      --let iots = map (\ mm->(iota mm)) mm1s
      let len1 = length mm1s
      let flag1 = mkFlagArray mm1s 0 mm1s
      let vals1 = map (\ f-> if f != 0
                                then 0
                                else 1
                       ) flag1
      let iots = segmented_scan (+) 0i32 flag1 vals1

      -- let arrs = map (\ i->(map (+2) i) iots)
      let arrs = map (+2) iots

      --let pss = map (\ n p ->(replicate n p)) mm1s sq_primes
      --flatten version
      --let (flag_n, flag_p) =
      --                  unzip <|
      --                  mkFlagArray mm1s (0,0) <|
      --                  zip mm1s sq_primes
      --or
      let flag_p = mkFlagArray mm1s 0 sq_primes
      --
      let pss = segmented_scan (+) 0i32 flag1 (flag_p :> []i32)


      let composite = map (*) pss arrs
      let not_primes = flatten composite
      --let not_primes = replicate flat_size 0

      -- If not_primes is correctly computed, then the remaining
      -- code is correct and will do the job of computing the prime
      -- numbers up to n!
      --------------------------------------------------------------
      --------------------------------------------------------------

       let zero_array = replicate flat_size 0
       let mostly_ones= map (\ x -> if x > 1 then 1 else 0) (iota (len+1))
       let prime_flags= scatter mostly_ones not_primes zero_array
       let sq_primes = filter (\i-> i>1 && i<=n && prime_flags[i]>0i32) (0...len)

       in  (sq_primes, len)

  in sq_primes

-- RUN a big test with:
-- $ futhark opencl primes-flat.fut
-- $ echo "10000000" | ./primes-flat -t /dev/stderr -r 10 > /dev/null
let main (n : i32) : []i32 = primesFlat n

                                                      