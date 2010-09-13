import PCRESub

main = do
  let text = "me boo" =~$ ("(me) boo", "he \\1")
  print text
