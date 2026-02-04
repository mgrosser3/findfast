# find-fast Docker Benchmark Container

This repository provides a **Docker image** that serves as a **test and
benchmark environment** for different **“find-fast” implementations**.

The container bundles the required toolchains (including Elixir/Erlang, Haskell,
and Rust) in a reproducible environment, allowing the various implementations
to be built and executed in a comparable way.

Reference implementations:

- **Elixir implementation by pherklotz**  
  https://github.com/pherklotz/find-fast-elixir.git

- **Rust implementation by ardo314**  
  https://github.com/ardo314/find-fast.git

> [!IMPORTANT]
> The container is not intended for production use.

---

## Included Implementations & Executables

| Language | Implementation           | Executable  |
| -------- | ------------------------ | ----------- |
| Rust     | ardo314                  | `find-fast` |
| Haskell  | (Haskell implementation) | `ff`        |
| Elixir   | pherklotz                | `fifa`      |

---

## 📦 Requirements

- Docker (CLI or Docker Desktop)

---

## 🚀 Usage

### Build the Docker image

```bash
docker build -t findfast-bench .
```

For Windows users, batch script is provided: `build.bat`

### Start the Docker container

```bash
docker run --rm -it findfast-bench bash
```

For Windows users, batch script is provided: `run.bat`

- `--rm` -> the container is automatically removed after it exits
- `-it` -> interactive shell

### Run the executables

Examples (depending on the project paths inside the container):

```bash
./ff 'find' '/home/**/*.md'
./find-fast '/home/**/*.md' 'find'
./fifa '/home/**/*.md' 'find'
```
