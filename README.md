# opam-check-checksum

Once upon a time, there was a hash algorithm (MD5) used widely in the
opam-repository. This hash algorithm suffered from cryptographic flaws.

Take a look into the corresponding issue: https://github.com/ocaml/opam-repository/issues/25876

Thanks to the great work in opam itself (esp. `opam admin` -
https://github.com/hannesm/opam/tree/migrate-extra-files), we wrote some
tooling to move over that weak and brittle period in time. But who'd trust a
programmer?

So, opam-check-checksum takes as input two opam-repositories.

Let's clone 6ed19e3 as `old-opam` and 2730ed6 as `new-opam`:
```
$ git clone https://github.com/ocaml/opam-repository.git old-opam
$ cd old-opam ; git checkout 6ed19e3 ; cd ..
$ git clone https://github.com/hannesm/opam-repository.git new-opam
$ cd new-opam ; git checkout 2730ed6 ; cd ..
```

Ok, now we can run
```
$ _build/default/opam_check_checksum.exe --old-opam-repo=`pwd`/old-opam --opam-repo=`pwd`/new-opam
```

And see the results, mainly whether the old hashes are included in the new ones, and all previously known extra-files now have an entry as extra-source.
