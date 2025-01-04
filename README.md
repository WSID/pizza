# Pizza Time!

Pizza is a library for vector graphics through graphics hardware.

Currently, this works with Vulkan.

## Packages

- `pizza` : Main library.
- `pizza-preview-window` : A test program to see how `pizza` would do. Not useful.

## Running `pizza-preview-window`

You can clone this repository and run this. You should install Vulkan and GLFW
to build this.

```bash
git clone https://github.com/WSID/pizza   # clone this repository
cd pizza
cabal run pizza-preview-window            # This will build and run pizza-preview-window
```

If you are on wayland session, you can pass additional flags for underlying
libraries.

```bash
cabal run pizza-preview-window -f wayland
```

## Use `pizza`

Using this is tedious for now.

Currently this is not on hackage. Use `source-repository-package` statement in
cabal project file.

`cabal` will clone this, and `pizza` will be available as dependency.

```cabal
source-repository-package
    type:     git
    location: https://github.com/WSID/pizza
    subdir:   pizza
```

## Cooking!

Basic drawing is working, but many works still required for it to be useful.
