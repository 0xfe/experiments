-- PCRE-based Regex Substitution
-- Mohit Muthanna Cheppudira <mohit@muthanna.com>
--
-- Based off code from Chris Kuklewicz's regex-compat library.
--
-- Requires Text.Regex.PCRE from regex-pcre.

module PCRESub(
  (=~$),
  reSub
) where

import Data.Array((!))
import Text.Regex.PCRE

-- subRegex copied (almost) in its entirety from regex-compat.
subRegex :: Regex                          -- ^ Search pattern
         -> String                         -- ^ Input string
         -> String                         -- ^ Replacement text
         -> String                         -- ^ Output string
subRegex _ "" _ = ""
subRegex regexp inp repl =
  let compile _i str [] = \ _m ->  (str++)
      compile i str (("\\",(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then \ _m -> (pre ++) . ('\\':)
             else \  m -> (pre ++) . ('\\' :) . compile i' str' rest m
      compile i str ((xstr,(off,len)):rest) =
        let i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
            x = read xstr
        in if null str' then \ m -> (pre++) . ((fst (m!x))++)
             else \ m -> (pre++) . ((fst (m!x))++) . compile i' str' rest m
      compiled :: MatchText String -> String -> String
      compiled = compile 0 repl findrefs where
        bre = makeRegexOpts defaultCompOpt execBlank "\\\\(\\\\|[0-9]+)"
        findrefs = map (\m -> (fst (m!1),snd (m!0))) (matchAllText bre repl)
      go _i str [] = str
      go i str (m:ms) =
        let (_,(off,len)) = m!0
            i' = off+len
            pre = take (off-i) str
            str' = drop (i'-i) str
        in if null str' then pre ++ (compiled m "")
             else pre ++ (compiled m (go i' str' ms))
  in go 0 inp (matchAllText regexp inp)

-- Substitue re with sub in str using options copts and eopts.
reSub :: String -> String -> String -> CompOption -> ExecOption -> String
reSub str re sub copts eopts = subRegex (makeRegexOpts copts eopts re) str sub

-- Substitute re with sub in str, e.g.,
--
-- The perl expression:
--
--   $text = "me boo";
--   $text =~ s/(me) boo/he $1/;
--
-- can be written as:
--
--   text = "me boo" =~$ ("(me) boo", "he \\1")
--
(=~$) :: String -> (String, String) -> String
(=~$) str (re, sub) = reSub str re sub defaultCompOpt defaultExecOpt
