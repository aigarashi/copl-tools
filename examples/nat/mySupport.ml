module Error =
struct
  type loc = {
    fn : string; (* file name *)
    l1 : int;
    c1 : int;
    l2 : int;
    c2 : int
  }

  let merge_loc loc1 loc2 =
    {loc1 with l2 = loc2.l2; c2 = loc2.c2}

end
