type t = {
  results : (unit, [ `Msg of string ]) result Lwt_stream.t;
  push_result : (unit, [ `Msg of string ]) result option -> unit;
}
