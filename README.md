# Glide

A programming language.

Currently, this includes:

-   Static typing
-   Generics, with monomorphization
-   [Type inference on function calls](./examples/identity.gl)

    ```
    func identity<T>(t T) T {
        t
    }

    identity("Foo")
    ```

-   Basic types, including `Int`s, `Bool`s, and `String`s
-   [Functions, usable as values](./examples/functions.gl)

    ```
    func a() {
        print("a")
    }

    func main() {
        let f = a
        f()
    }
    ```

-   Local variables

    ```
    let x = 2
    let y Int = 3
    ```

-   [`if`/`else` expressions](./examples/if.gl)

    ```
    let x = if cond1 {
        1
    } else if cond2 {
        2
    } else {
        3
    }
    ```
-   Single-line comments

    ```
    // This is a comment
    ```

-   Native code generation using [LLVM](https://llvm.org)

## Examples

[Hello world](./examples/hello.gl):

```
func main() {
    print("Hello World!")
}
```

[Fibonacci numbers](./examples/fibonacci.gl):

```
func fibonacci(n Int) Int {
    if eqInt(n, 0) {
        0
    } else if eqInt(n, 1) {
        1
    } else {
        add(fibonacci(sub(n, 1)), fibonacci(sub(n, 2)))
    }
}
```

## Usage

First, install LLVM.

Compile the CLI with

```sh
cargo b -p glide_cli --release
```

Then run the Hello World example with

```
./target/release/glide_cli ./examples/hello.gl > out.ll
clang-14 out.ll -o out
./out
```
