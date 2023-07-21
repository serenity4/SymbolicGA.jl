# Changelog

## v0.2

- ![BREAKING][badge-breaking] `*` no longer aliases the geometric product by default `⟑` (`\wedgedot`).
- ![BREAKING][badge-breaking] `VariableInfo` has been renamed `Bindings` for better clarity.
- ![BREAKING][badge-breaking] The `flattening` symbol parameter to `@ga` has been removed, instead using type annotations to express a concatenated or tuple output.
- ![BREAKING][badge-breaking] The `::KVector{$k}` type annotation is no longer supported; instead, prefer using `::$k` or (if `k` is small enough) one of its aliases, e.g. `Bivector` for `KVector{2}`. The `::Multivector{$k, $l, ...}` syntax is also no longer supported; use the `::($k, $l, ...)` syntax instead.
- ![BREAKING][badge-breaking] Default bindings are no longer merged into provided bindings in `codegen_expression`.
- ![feature][badge-feature] A new macro `@geometric_space` has been defined to ease the definition of user macros. See the documentation for more information.

[badge-breaking]: https://img.shields.io/badge/BREAKING-red.svg
[badge-deprecation]: https://img.shields.io/badge/deprecation-orange.svg
[badge-feature]: https://img.shields.io/badge/feature-green.svg
[badge-enhancement]: https://img.shields.io/badge/enhancement-blue.svg
[badge-bugfix]: https://img.shields.io/badge/bugfix-purple.svg
[badge-security]: https://img.shields.io/badge/security-black.svg
[badge-experimental]: https://img.shields.io/badge/experimental-lightgrey.svg
[badge-maintenance]: https://img.shields.io/badge/maintenance-gray.svg

<!--
# Badges (reused from the CHANGELOG.md of Documenter.jl)

![BREAKING][badge-breaking]
![Deprecation][badge-deprecation]
![Feature][badge-feature]
![Enhancement][badge-enhancement]
![Bugfix][badge-bugfix]
![Security][badge-security]
![Experimental][badge-experimental]
![Maintenance][badge-maintenance]

-->
