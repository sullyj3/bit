# todo
## In progress

## todo
- [ ] fix bugs around beginning and end of lines
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
- [ ] better line editing for input widget
  - eg visible cursor, arrow key movement at minimum. Will probably want to re-use some insert mode code
