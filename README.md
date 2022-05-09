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
-   Native code generation using [LLVM](https://llvm.org)

## Example

[Hello world](./examples/hello.gl):

```
func main() {
    print("Hello World!")
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
./target/release/glide_cli ./examples/hello.gl
```
