countup = (\next start end -> if start == end then end else next (start + 1) end)

{-
> fix (\next start end -> if start == end then end else next (start + 1) end) 0 10
> (let rec = (\next start end -> if start == end then end else next (start + 1) end) rec in rec) 0 10

-- This is wrong
> (let rec = (\start end -> if start == end then end else rec (start + 1) end) in rec) 0 10
> let rec = (\start end -> if start == end then end else rec (start + 1) end) in (rec 0 10)

-- This is correct
> let rec start end = if start == end then end else rec (start + 1) end in (rec 0 10)
-}