## v1.4.1.0 (2024-06-30)

Release v1.4.1.0

- Add support for custom targets
    - `--custom-target` (`-T`)
- Improve logic behind looping mechanism for `--mode=reinstall-atoms`
- Improve output and internal representations of packages/program state
- Test with ghc-9.10

## v1.4.0.0 (2024-05-25)

Release v1.4.0.0

- Add new options for portage invocation

    - Add `--mode=reinstall-atoms` (`-R`)
    - Add `--world` (`-W`)

  This adds new functionality to haskell-updater which utilizes the
  --reinstall-atoms flag for portage. This should bypass issues where
  haskell-updater pulls in masked or unavailable packages and attempts to
  pull them into the dependency graph. This will remain as optional pending
  further testing.

- Add new flags to the command line to help organize functionality:

  - `--target={invalid|all|world}`
  - `--mode={basic|list|reinstall-atoms}`

- Fix bug where some installed package `.conf` files do not get parsed
  properly
