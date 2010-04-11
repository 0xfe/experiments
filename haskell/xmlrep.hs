data H_XML = Tag { name::String, children::[H_XML]} 
             | Value { name::String, value::String } 

instance Show H_XML where 
  show x = xml_show x 

instance Read H_XML where 
  read x = xml_read x 

class XML_Rep a where 
  toXML :: a -> H_XML 
  fromXML :: H_XML -> a 

xml_show x = "String"
xml_read x = Tag "Nothing" [Value "Int" "One"]

main = do
  print $ show $ read "A"
