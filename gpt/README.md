# Llama.cpp + instructor + pydantic

## Getting the model

Go to [this page](https://huggingface.co/bartowski/Llama-3.2-3B-Instruct-GGUF/blob/main/Llama-3.2-3B-Instruct-Q8_0.gguf) and click download. Put the .gguf file in this directory.

## Running the model.

```bash
$ nix-shell llamashell.nix --run 'python salinegpt_llama.py'
```

# Outlines + pydantic

## No dependencies to install explicitly, nix-shell works out of the box

## No need to explicitly get the model

## Running the model

```bash
$ nix-shell shell.nix --run 'python salinegpt_outlines.py'
```
