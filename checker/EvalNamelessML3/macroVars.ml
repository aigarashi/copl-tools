open Parser

let v = [
    ("d", fun s -> MVDEXP s);
    ("w", fun s -> MVVALUE s);
    ("env", fun s -> MVENV s);
]


