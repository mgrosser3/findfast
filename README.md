# 🔍 Find Fast

Find Fast (short ff) is a lightweight and efficient command-line tool for
quickly searching files for text patterns—similar to the classic grep.

> [!IMPORTANT]
> This project is for personal development and is purely a leisure activity.
> **It is not recommended to use this tool in production.**

> [!TIP]
> Other implementations of this in other languages can be found here:
>
> - [Elixir provided by pherklotz](https://github.com/pherklotz/find-fast-elixir/)
> - [Rust provided by ardo314](https://github.com/ardo314/find-fast)

## ✨ Features

- **Hidden files** (starting with `.`) are skipped
- **Inaccessible files** (no read premissions) are skipped with error message
- **Recursive search** through all subdirectories
- **Duration measurement** at the end of execution
- **UTF-8 support** for console output

## 🚀 Usage

**Syntax**

```bash
ff <pattern> <path>
```

**Example:**

Search for the word “grep” in the project's readme file:

```bash
ff grep README.md
```

Search in a directory (recursive):

```bash
ff "TODO|FIXME" ./src
```

## 🔨 Development

### What You Need

**Haskell Toolchain**

You need the Following software:

1. **GHC (Glasgow Haskell Compiler)** - Version 9.6
2. **Cabal** - Version 3.12

> [!TIP]
> It is recommend that you use **GHCup** to install and manage your Haskell
> toolchain. GHCup makes it easy to install GHC, Cabal, and other Haskell tools,
> and allows you to switch between different versions effortlessly.
> [haskell.org/ghcup](https://www.haskell.org/ghcup/)

### Building the Project

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
