let rec list_drop n lst =
  match n with 0 -> lst | n -> list_drop (n - 1) (List.tl lst)
