# design
## installation
- needs to be easy to install. Cargo install is ok for the short term
- provide an install script that puts the binary in .local/bin

## new user experience
- should launch help the first time it's opened
- ability to search for normal mode commands for easy keybinding discovery (help should mention this)

## interaction
- selection first, like kakoune
- text objects
  - inner line, like vim-utils/vim-line
- fast navigation, like amp
- fuzzy find for all menus (skim library)
  - easy discoverability
- fuzzy autocompletion
- space for command line? some thing easy to reach
- jump to previous location is very important
- operater pending popups like kakoune?
### keys
- x/X for line select is awesome
- hjkl
- HJKL for "amplified" motion, like in amp - start and end of line, top and bottom of file
- tab to switch buffers
- quick jump modes
- paragraph motion
- dedicated duplicate button?

## configuration and extensibility
- runs as server
- lsp integration is important
- how to do extensibility...? msgpack rpc like nvim?
- maybe plugins are just in haskell?
- configuration needs to be easy to understand - no "highlighters"
- maybe config with dhall?
