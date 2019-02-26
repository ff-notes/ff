```haskell
data TaskMode
    = Overdue (Down Natural)  -- ^ end in past, with days
    | EndToday                -- ^ end today
    | EndSoon Natural         -- ^ started, end in future, with days
    | Actual                  -- ^ started, no end
    | Starting Natural        -- ^ starting in future, with days

taskMode today Note{note_start = start, note_end} = case note_end of
    Nothing
        | start <= today     -> Actual
        | otherwise          -> Starting $ start - today
    Just end -> case compare end today of
        LT                   -> Overdue  $ today - end
        EQ                   -> EndToday
        GT  | start <= today -> EndSoon  $ end   - today
            | otherwise      -> Starting $ start - today
```
