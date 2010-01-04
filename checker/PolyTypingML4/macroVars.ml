open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("t", fun s -> MVTYPE s);
    ("s", fun s -> MVTYSC s);
    ("env", fun s -> MVTENV s);
  ]
