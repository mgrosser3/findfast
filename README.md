# 🔍 Find Fast

Find Fast (short ff) is a lightweight and efficient command-line tool for
quickly searching files for text patterns—similar to the classic grep.

**It is used with:**

```bash
ff PATTERN [PATH]
```

**Example:**

Search for the word “grep” in the project's readme file.

```bash
ff grep README.md
```

## 🔧 Build

When building with ...

```bash
cabal build
```

... the artifacts are placed under `dist-newstyle/`.
The structure is long by design to separate builds by architecture, GHC version,
and package version:

```
dist-newstyle/
  build/
    x86_64-windows/     ← target architecture
      ghc-9.4.8/        ← GHC version
        findfast-0.0.1/ ← package name + version
          x/            ← executables
            findfast/
              build/
                findfast ← the built binary
```

You usually don’t need the full path — just run the program with:

```bash
cabal run
```

Or install the binary to a simpler folder:

```bash
cabal install --installdir=build --overwrite-policy=always
```

This places the executable neatly in build/.
