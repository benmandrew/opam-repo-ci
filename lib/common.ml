let parse_revdeps pkg output =
  String.split_on_char '\n' output |>
  List.fold_left (fun acc -> function
      | "" -> acc
      | revdep ->
          let revdep = OpamPackage.of_string revdep in
          if OpamPackage.equal pkg revdep then
            acc (* NOTE: opam list --recursive --depends-on <pkg> also returns <pkg> itself *)
          else
            OpamPackage.Set.add revdep acc
    ) OpamPackage.Set.empty
