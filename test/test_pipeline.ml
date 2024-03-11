module Lint = Opam_repo_ci.Lint
module Analyse = Opam_repo_ci.Analyse

open Lwt.Infix

open Util

let with_temp_repo f =
  Lwt_io.create_temp_dir ~prefix:"dummy-opam-repository-" () >>=
  fun repo_dir ->
  let repo_dir = Fpath.v repo_dir in
  init repo_dir >>= fun () ->
  apply_patches ~cwd:repo_dir "a-1" [ "a-1.patch" ] >>= fun () ->
  Cmd.git ~cwd:repo_dir [ "checkout"; "-qb"; "new-branch" ] >>= fun () ->
  f repo_dir >>= fun () ->
  Cmd.rm ~cwd:repo_dir [ "-rf" ] [ repo_dir ]

let test_correct _switch () =
  with_temp_repo @@ fun repo_dir ->
  apply_patches ~cwd:repo_dir "b-correct" [ "b-correct.patch" ] >>= fun () ->
  let cmd = [
    "opam-repo-ci-local";
    Printf.sprintf "--repo=%s" @@ Fpath.to_string repo_dir;
    "--branch=new-branch";
    "--lint-only";
  ] in
  Cmd.exec_or_fail_stdout cmd >|= fun s ->
  Alcotest.(check string) ("Correct") "Ok ()\n" s

(** Tests the following:
    - [b.0.0.1] is missing the [author] field
    - [b.0.0.2] has an extra unknown field
    - [b.0.0.3] is correct *)
let test_incorrect_opam _switch () =
  with_temp_repo @@ fun repo_dir ->
  apply_patches ~cwd:repo_dir "b-incorrect-opam"
    [ "b-incorrect-opam.patch" ] >>= fun () ->
  let cmd = [
    "opam-repo-ci-local";
    Printf.sprintf "--repo=%s" @@ Fpath.to_string repo_dir;
    "--branch=new-branch";
    "--lint-only";
  ] in
  Cmd.exec_or_fail_stdout cmd >|= fun s ->
  Alcotest.(check string) ("Incorrect opam") "Error \"2 errors\"\n" s

(** Tests the package name collision detection by adding four versions
    of a package [a_1] that conflicts with the existing [a-1] package *)
let test_name_collision _switch () =
  with_temp_repo @@ fun repo_dir ->
  apply_patches ~cwd:repo_dir "a_1-name-collision"
    [ "a_1-name-collision.patch" ] >>= fun () ->
  let cmd = [
    "opam-repo-ci-local";
    Printf.sprintf "--repo=%s" @@ Fpath.to_string repo_dir;
    "--branch=new-branch";
    "--lint-only";
  ] in
  Cmd.exec_or_fail_stdout cmd >|= fun s ->
  Alcotest.(check string) ("Name collision") "Error \"4 errors\"\n" s

let tests = [
    Alcotest_lwt.test_case "lint-correct" `Slow test_correct;
    Alcotest_lwt.test_case "lint-incorrect-opam" `Slow test_incorrect_opam;
    Alcotest_lwt.test_case "lint-name-collision" `Slow test_name_collision;
  ]
