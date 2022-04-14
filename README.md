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
-   An interpreter

## Example

[Hello world](./examples/hello.gl):

```
func main() {
    print("Hello World!")
}
```

## Usage

Compile the CLI with

```sh
cargo b -p glide_cli --release
```

Then run with

```
.\target\release\glide_cli ./examples/hello.gl
```
