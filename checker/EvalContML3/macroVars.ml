open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("v", fun s -> MVVALUE s);
    ("env", fun s -> MVENV s);
    ("k", fun s -> MVCONT s);
  ]
