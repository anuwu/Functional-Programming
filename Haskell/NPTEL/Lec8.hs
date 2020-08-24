wordCount :: String -> Int
wordCount "" = 0
wordCount str = do
                let rst = trimSpc str
                (if rst == "" then 0 else 1) + (wordCount (elimWrd rst))

trimSpc :: String -> String
trimSpc "" = ""
trimSpc (ch:str)
    | (ch == ' ') = trimSpc (str)
    | otherwise   = (ch:str)

elimWrd :: String -> String
elimWrd "" = ""
elimWrd (ch:str)
    | (ch /= ' ') = elimWrd (str)
    | otherwise   = (ch:str)