# Open in Editor

## Description

Provides functionality to open editor based on user preferences.

Features:

* By default compatible with how UNIX-like systems interpret `VISUAL` and
  `EDITOR` environment variables.
  <https://unix.stackexchange.com/questions/4859/visual-vs-editor-what-s-the-difference>
  <https://en.wikibooks.org/wiki/Guide_to_Unix/Environment_Variables#VISUAL>
  <https://unix.stackexchange.com/questions/213367/where-do-editor-pager-browser-environment-variables-come-from>

* Designed in a fully configurable way so that it can be used in applications
  with a consistent UI.

* Possibility to use Dhall configuration file and/or custom environment
  variables to specify defaults or override system/user preferences.


## Similar packages

* [hackage.haskell.org/package/editor-open
  ](http://hackage.haskell.org/package/editor-open) - Open the user's `$VISUAL`
  or `$EDITOR` for text input.

  This package prefers `EDITOR` before `VISUAL`, which is not a correct
  behaviour.

* [hackage.haskell.org/package/editor-open
  ](http://hackage.haskell.org/package/read-editor) - Opens a temporary file on
  the system's EDITOR and returns the resulting edits.

  This package takes into account only `EDITOR` environment variable, and if
  that fails it prompts user for editor command.
