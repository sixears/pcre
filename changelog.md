0.1.6.0 2025-09-24
==================

- PCREScriptError is now an instance of AsParseError (inherited from
  Stdmain.ScriptError)

0.1.5.0 2025-09-20
==================
- Fix capture-name lookup for non-zero-width usage; add error about using zero-width assertions with
  capture names

0.1.4.0 2025-09-16
==================
- Export PCRE.GroupID

0.1.3.0 2025-09-10
==================
- PCREScriptError is also an instance of AsREParseError

0.1.2.0 2025-09-09
==================
- +PCREScriptError

0.1.1.0 2025-09-08
==================
- add (~~), (≈)

0.1.0.0 2024-09-23
==================
- harden PCRE as a type; remove PCRE.OptParse, make PCRE an instance of
  OptParsePlus.OptReader

0.0.6.0 2024-09-16
==================
- add PCRE.OptParse

0.0.5.0 2023-12-15
==================
- upgrade for GHC9

0.0.4.2 2023-07-08
==================
- use stdmain-1.6

0.0.4.1 2022-04-14
==================
- upgrade dependencies (particularly stdmain to 1.5.7.0)
- no longer depend on base1, since we require base1t

0.0.4.0 2022-01-17
==================
- add Printable instance of REPlacement

0.0.3.0 2022-01-15
==================
- add pcre executable
- add Printable instance of ReplText

0.0.2.1 2022-01-07
==================
- fix bug with capture groups in REMatch in the presence of zero-width assertions

0.0.2.0 2021-12-31
==================
- add Hashable instance of REPlacement

0.0.1.0 2021-12-23
==================
- split REPlacement into its own file; export REPlacement data c'tor

0.0.0.0 2021-12-17
==================
- first version (factored out from rename.hs)
