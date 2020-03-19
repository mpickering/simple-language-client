A LSP client where save events are powered by fsnotify and diagnostics
displayed in a minimal terminal interface.

```
Welcome to simple-language-client

Usage: simple-language-client [-s|--server ARG] DIR [--root-dir ARG]
  A language client powered by fsnotify

Available options:
  -s,--server ARG          Path to the language server (default: "ghcide")
  DIR                      The directory containing the source files to load
  --root-dir ARG           Path to root dir
  -h,--help                Show this help text
```

# Usage

Start `simple-language-client` in the root directory of your project. By default it will
try to call `ghcide` in order to start the language server. The argument is
a directory of files which contains the component you want `ghcide` to load or
a single root file which will be loaded on its own.

```
simple-language-client src/
```

You can also specify to use `hie` by changing the server argument to a path to the
`hie` executable.

```
simple-language-client -s hie src/
```

Pointing the client at a single file also works:

```
simple-language-client exe/Main.hs
```
