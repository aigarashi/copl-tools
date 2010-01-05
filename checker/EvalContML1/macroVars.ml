open Parser

let v = [
    ("e", fun s -> MVEXP s);
    ("v", fun s -> MVVALUE s);
    ("k", fun s -> MVCONT s);
]


