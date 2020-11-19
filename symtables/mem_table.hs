[(String * [[(String * String * Type)]])]

insertMemTable :: (String )-> [(String * [[(String * String * Type)]])] -> [(String * [[(String * String * Type)]])]




proc foo(){
  Int x := 10; // insira a variavel (x, "foo", 10) em "foo"
  if(x == 10){
    Int z := -1; // insira a variavel (z, "foo:if", -1) em "foo"
  }
}



memory = [
  ("", [
    ("", "x", 0)
  ]),
  ("faa", [[
    ("faa:", "y", 0)
    ("faa:if", "z", 0)
  ],[
    ("faa", "y", 0)
  ],
    ...,
  ]),
]
