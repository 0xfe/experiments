import Text.Regex
import Text.Regex.PCRE

main = print $ subRegex (mkRegex "(me) boo") "me boo" "he \\1"
