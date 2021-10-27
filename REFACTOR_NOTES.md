
in the middle of refactoring from
    storing buffer directly in the window
    to
    storing buffers in global buffer map, and storing just the buffer ID in the window

I'm up to refactoring the editing functions to operate on buffers instead of just windows.