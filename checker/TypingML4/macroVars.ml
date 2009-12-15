open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("t", fun s -> MVTYPE s);
    ("env", fun s -> MVTENV s);
  ]
