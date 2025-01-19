countup = (\next start end -> if start == end then end else next (start + 1) end)

{-
> fix (\next start end -> if start == end then end else next (start + 1) end) 0 10
> (let rec = (\next start end -> if start == end then end else next (start + 1) end) rec in rec) 0 10

-- This is wrong
> (let rec = (\start end -> if start == end then end else rec (start + 1) end) in rec) 0 10
> let rec = (\start end -> if start == end then end else rec (start + 1) end) in (rec 0 10)

-- This is correct
> let rec start end = if start == end then end else rec (start + 1) end in (rec 0 10)

-- But the above jumps many steps, what we actually have is
Let func = (\next start end -> if start == end then end else next (start + 1) end)
(fix func) 0 10
> func (fix func) 0 10
> (\next start end -> if start == end then end else next (start + 1) end) (fix func) 0 10
> if 0 == 10 then 10 else fix func 1 10
> fix func  1 10
> ... and so on ...
-}