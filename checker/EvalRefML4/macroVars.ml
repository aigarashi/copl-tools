open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("v", fun s -> MVVALUE s);
    ("env", fun s -> MVENV s);
    ("s", fun s -> MVSTORE s);
    ("S", fun s -> MVSTORE s);
  ]
