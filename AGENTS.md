# AGENTS.md

## Project purpose
OCaml library that parses, manipulates, and serializes VyOS 1.x and EdgeOS configuration files. The data layer that the rest of the VyOS config stack is built on.

## Tech stack
- OCaml; `dune` 2.0 with `menhir` (parser); `ppx_deriving_yojson`, `yojson` for JSON I/O.
- `opam`: `vyos1x-config.opam` declares version `0.3`, MIT license, deps `ocamlfind`, `menhir`, `dune >= 1.4.0`, `ppx_deriving_yojson <= 3.9.1`, `yojson`.
- License: LGPL-2.1 in tree (`LICENSE`); opam declares `MIT` (note the discrepancy if redistributing).

## Build / test / run
- Local: `opam install. --deps-only` then `dune build -p vyos1x-config`.
- Install: `dune install` (or via opam-pin).
- No `dune runtest` suite in tree; coverage comes from `libvyosconfig` and `vyos-1x` smoketests downstream.

## Repository layout
- `src/` — OCaml sources (parser via menhir, AST, serializers) plus `lexical_numeric_compare.c` (small C helper).
- `dune-project`, `vyos1x-config.opam` — build/package metadata.
- `.github/workflows/` — `check-pr-conflicts.yml`, `cla-check.yml`, `pr-mirror-repo-sync.yml` — all delegate to `vyos/.github` reusables.

## Cross-repo context
- Wrapped by `VyOS-Networks/libvyosconfig` (OCaml→C shim producing `libvyosconfig0.so`) which is also vendored inside `vyos/vyos-1x/libvyosconfig/` as the canonical build source for the Debian package.
- Consumed (via `libvyosconfig0`) by `vyos/vyos-1x`'s `python/vyos/configtree.py` (ctypes binding) and by the legacy C++ Vyatta layer.
- `vyos/vyconf` (config-session daemon, OCaml) depends on this library directly.
- Live consumer of the generation-1 mirror pipeline (`pr-mirror-repo-sync.yml@current`).

## Conventions
- Commit / PR title: `component: T12345: description` (Phorge ID mandatory).
- Default branch `current`; backports via `@Mergifyio backport <branch>`.
- Reusable workflows pinned `@current` — changes in `vyos/.github` ship immediately.

## Mirror relationship
Canonical side. Twin at `VyOS-Networks/vyos1x-config` is force-pushed by the mirror pipeline; do not edit it.

## Notes for future contributors
- This is a load-bearing library — every `commit` operation against VyOS goes through it. Add tests at the `libvyosconfig`/`vyos-1x` layers, not just locally.
- Keep parser changes backwards compatible; existing configs and migration scripts depend on round-trip stability.
- LICENSE (LGPL-2.1) vs opam (MIT) mismatch is worth resolving before any non-trivial relicensing scenario.
- `vyconf` (still early-stage per its README) is a fellow OCaml consumer; coordinate API changes with that repo.
