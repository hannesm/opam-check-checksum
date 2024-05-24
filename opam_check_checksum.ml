
let map_directory f dir =
  let go dir =
    let dirfd = Unix.opendir dir in
    let rec one (files, dirs) =
      try
        let entry = Unix.readdir dirfd in
        if entry = "." || entry = ".." || entry = ".git" then
          one (files, dirs)
        else
          let name = dir ^ "/" ^ entry in
          match Unix.(stat name).st_kind with
          | Unix.S_REG -> one ((dir, entry) :: files, dirs)
          | Unix.S_DIR -> one (files, name :: dirs)
          | _ -> Logs.err (fun m -> m "unkown file type for %s" name); exit 3
      with
        End_of_file -> (files, dirs)
    in
    let files, dirs = one ([], []) in
    Unix.closedir dirfd;
    (files, dirs)
  in
  let rec get_all cnt files dirs =
    match dirs with
    | dir :: dirs ->
      if cnt mod 1000 = 0 && cnt > 0 then
        Logs.app (fun m -> m "%u directories traversed, going through %s" cnt dir);
      let new_files, new_dirs = go dir in
      get_all (cnt + 1) (files @ new_files) (dirs @ new_dirs)
    | [] -> files
  in
  let files = get_all 0 [] [ dir ] in
  List.filter_map f files

module M = Map.Make(String)

let jump () old_dir new_dir =
  (* first we collect a map of opam files from [old_dir] which have md5 hashes *)
  let f (dir, filename) =
    if String.equal "opam" filename then
      let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.create (OpamFilename.Dir.of_string dir) (OpamFilename.Base.of_string filename))) in
      let url_has_only_md5 url =
        List.for_all (fun hash -> OpamHash.kind hash = `MD5) (OpamFile.URL.checksum url)
      in
      let has_extra_file =
        match OpamFile.OPAM.extra_files opam with None -> false | Some _ -> true
      in
      if
        List.exists (fun (_, url) -> url_has_only_md5 url) (OpamFile.OPAM.extra_sources opam)
        || (match OpamFile.OPAM.url opam with None -> false | Some url -> url_has_only_md5 url)
        || has_extra_file
      then begin
        let pkgdir = String.sub dir (String.length old_dir) (String.length dir - String.length old_dir) in
        Logs.debug (fun m -> m "opam file %s/%s looks interesting to us" pkgdir filename);
        Some (pkgdir, opam)
      end else
        None
    else
      None
  in
  let interesting_opam_files = map_directory f old_dir in
  let of_interest =
    List.fold_left (fun m (dir, opam) -> M.add dir opam m) M.empty interesting_opam_files
  in
  Logs.app (fun m -> m "%u opam files of interest" (M.cardinal of_interest));
  let f (dir, filename) =
    let pkgdir = String.sub dir (String.length new_dir) (String.length dir - String.length new_dir) in
    if String.equal filename "opam" && M.mem pkgdir of_interest then
      let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.create (OpamFilename.Dir.of_string dir) (OpamFilename.Base.of_string filename))) in
      Some (pkgdir, opam)
    else
      None
  in
  let new_opam_files = map_directory f new_dir in
  let new_opam_files = List.fold_left (fun m (dir, opam) -> M.add dir opam m) M.empty new_opam_files in
  M.iter (fun dir opam ->
      match M.find_opt dir new_opam_files with
      | None -> Logs.err (fun m -> m "opam file %s not found in new repo" dir);
      | Some new_opam ->
        let print_hashes hs =
          String.concat "\n" (List.map (fun h -> OpamHash.(string_of_kind (kind h) ^ "=" ^ contents h)) hs)
        in
        let present hashes new_hashes =
          List.for_all (fun h ->
              let kind = OpamHash.kind h
              and content = OpamHash.contents h
              in
              List.exists (fun h' ->
                  OpamHash.kind h' = kind && OpamHash.contents h' = content)
                new_hashes)
            hashes
        in
        let check_url () =
          match OpamFile.OPAM.url opam, OpamFile.OPAM.url new_opam with
          | None, None -> ()
          | Some _, None | None, Some _ -> Logs.err (fun m -> m "shouldn't happen, url disappeared for %s" dir)
          | Some url, Some new_url ->
            let hashes = OpamFile.URL.checksum url in
            let new_hashes = OpamFile.URL.checksum new_url in
            if present hashes new_hashes then
              () (* Logs.app (fun m -> m "%s looks good" dir) *)
            else
              Logs.err (fun m -> m "%s doesn't contain the old hashes:@.%s@.new hashes:@.%s@."
                           dir (print_hashes hashes) (print_hashes new_hashes))
        in
        let check_extra_files () =
          match OpamFile.OPAM.extra_files opam with
          | None -> ()
          | Some files ->
            let new_files = OpamFile.OPAM.extra_sources new_opam in
            List.iter (fun (base, hash) ->
                match List.find_opt (fun (base', _) ->
                    String.equal (OpamFilename.Base.to_string base) (OpamFilename.Base.to_string base'))
                    new_files
                with
                | None -> Logs.err (fun m -> m "%s extra-file %s not present in new" dir (OpamFilename.Base.to_string base))
                | Some (_, url) ->
                  let new_hashes = OpamFile.URL.checksum url in
                  if present [hash] new_hashes then
                    () (* Logs.app (fun m -> m "%s looks good" dir) *)
                  else
                    Logs.err (fun m -> m "%s extra-file %s doesn't contain the old hashes:@.%s@.new hashes:@.%s@."
                                 dir (OpamFilename.Base.to_string base)
                                 (print_hashes [hash]) (print_hashes new_hashes)))
              files
        in
        check_url ();
        check_extra_files ()
    ) of_interest;
  Ok ()

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ~dst:Format.std_formatter ())

open Cmdliner

let setup_log =
  Term.(const setup_log
        $ Fmt_cli.style_renderer ()
        $ Logs_cli.level ())

let dir =
  let doc = "Directory of the opam-repository" in
  Arg.(value & opt string "/home/hannes/mirage/opam-repository" & info [ "opam-repo" ] ~doc)

let old_dir =
  let doc = "Directory of the old opam-repository" in
  Arg.(value & opt string "/home/hannes/mirage/or-old" & info [ "old-opam-repo" ] ~doc)

let cmd =
  let info = Cmd.info "opam-check-checksum"
  and term =
    Term.(term_result (const jump $ setup_log $ old_dir $ dir))
  in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
