Correct inputs

  $ ChoiceTest --choice-path "2 3 ."
  2 3 .

  $ ChoiceTest --choice-path "2 3 _"
  2 3 _

  $ ChoiceTest --choice-path "2 3 fail"
  2 3 fail

  $ ChoiceTest --choice-path "(2 _) 3 ."
  (2 _) 3 .

Incorrect inputs

  $ ChoiceTest --choice-path "2"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"2\"")
  [2]

  $ ChoiceTest --choice-path "2 3 . _"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"2 3 . _\"")
  [2]

  $ ChoiceTest --choice-path "2 . ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"2 . .\"")
  [2]

  $ ChoiceTest --choice-path "2 _ ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"2 _ .\"")
  [2]

  $ ChoiceTest --choice-path "2 fail ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"2 fail .\"")
  [2]

  $ ChoiceTest --choice-path "(2) fail"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"(2) fail\"")
  [2]
