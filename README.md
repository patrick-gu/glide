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
        println("a")
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

-   [Attributes](./examples/attributes.gl)

    ```
    attribute Happy

    [Happy]
    func happy() {
    }
    ```

-   Packages with `pub`lic and private visibility

-   Native code generation using [LLVM](https://llvm.org)

## Examples

[Hello world](./examples/hello.gl):

```
func main() {
    println("Hello World!")
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

Run the Hello World example with

```
cargo r -- compile ./examples/hello.gl -o hello.o
clang-14 -no-pie hello.o -o hello
./hello
```
