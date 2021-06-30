# todo
## In progress

## todo
- [ ] if path provided on command line doesn't exist, create a new buffer with that path
- [ ] add general way of getting input from the user, eg command line, filename input
- [ ] saving to arbitrary location
- [ ] display error message when trying to save a file with no path
  in future maybe just automatically open the save as dialog
- [ ] creating a new buffer
- [ ] add error message for saving when the buffer has no path yet
- [ ] I, a, A, o, O
- [ ] selection
- [ ] line numbers
- [ ] fuzzy finding of files
- [ ] ensure files are terminated with a newline when saving
- [ ] command line
- [ ] investigate where I should put strictness annotations
- [ ] display tabs correctly
- [ ] load dhall settings file - provide expandTab, tabstop, shiftwidth, etc (or think about that design)
- [ ] handle long lines correctly - wrapping, or horizontal scrolling
- [ ] investigate sluggish scrolling, especially with long lines
- [ ] tweak colors
- [ ] map cursor position to buffer position
- [ ] deal correctly with characters of width > 1
- [ ] use a rope for each line (is this a terrible idea? lots of memory overhead. Maybe switch depending on line length)
- [ ] autoclose brackets
