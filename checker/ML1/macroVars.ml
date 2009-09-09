open Parser

let v = [
    ("exp", fun s -> MVEXP s);
    ("value", fun s -> MVVALUE s);
]


