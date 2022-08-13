# Glide

A programming language.

Currently, this includes:

-   Static typing
-   Generics, with monomorphization
-   Type inference on function calls

    ```
    func identity<T>(t T) T {
        t
    }

    identity("Foo")
    ```

-   Basic types, including `Int`s and `String`s
-   Functions as values
-   Local variables
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
