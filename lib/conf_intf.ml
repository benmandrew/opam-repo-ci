module type S = sig
  val profile : [> `Dev | `Production | `Staging ]
  val build_timeout : int64
  val host_os : string
  val host_arch : Ocaml_version.arch
end
