module Resource = struct
  let fetch_filename is_sys name =
    ignore is_sys;
    name (* todo: add resource dict *)
end
