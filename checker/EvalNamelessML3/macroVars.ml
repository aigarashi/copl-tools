open Parser

let v = [
    ("d", fun s -> MVDEXP s);
    ("v", fun s -> MVVALUE s);
    ("env", fun s -> MVENV s);
]


