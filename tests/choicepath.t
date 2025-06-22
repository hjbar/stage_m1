Correct inputs

  $ ChoiceTest --choice-path "s2 s3 ."
  s2 s3 .

  $ ChoiceTest --choice-path "s2 s3 _"
  s2 s3 _

  $ ChoiceTest --choice-path "s2 s3 fail"
  s2 s3 fail

  $ ChoiceTest --choice-path "s2 i3"
  s2 i3

  $ ChoiceTest --choice-path "bind (s2 _) s3 ."
  bind(s2 _) s3 .

Incorrect inputs

  $ ChoiceTest --choice-path "s2"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"s2\"")
  [2]

  $ ChoiceTest --choice-path "s2 i3 _"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"s2 i3 _\"")
  [2]

  $ ChoiceTest --choice-path "s2 . ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"s2 . .\"")
  [2]

  $ ChoiceTest --choice-path "s2 _ ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"s2 _ .\"")
  [2]

  $ ChoiceTest --choice-path "s2 fail ."
  Fatal error: exception Failure("ChoiceTest: parse error on input \"s2 fail .\"")
  [2]

  $ ChoiceTest --choice-path "bind i3 fail"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"bind i3 fail\"")
  [2]

  $ ChoiceTest --choice-path "bind (s2) fail"
  Fatal error: exception Failure("ChoiceTest: parse error on input \"bind (s2) fail\"")
  [2]
