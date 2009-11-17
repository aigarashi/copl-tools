open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("d", fun s -> MVDBEXP s);
    ("env", fun s -> MVENV s);
]


