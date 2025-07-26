# MyPandoc

## Overview

**MyPandoc** is a simplified version of the popular document converter [Pandoc](https://pandoc.org/). Written in Haskell, this project aims to implement a program capable of converting documents between different formats. The program processes an input document and outputs it in another format, focusing on XML, JSON, and Markdown.

## Features

- **Input Formats**: XML, JSON (Markdown as a bonus).
- **Output Formats**: XML, JSON, Markdown.
- **Document Structure**:
  - **Header**: Includes a title (mandatory), author (optional), and date (optional).
  - **Content**: Composed of text, formatting (italic, bold, code), links, images, structural elements (paragraphs, sections, code blocks), and lists.

## Usage

The program supports the following options:

- `-i`: Path to the input file (mandatory).
- `-f`: Format of the output (mandatory).
- `-o`: Path to the output file (optional).
- `-e`: Format of the input file (optional).

### Example

```bash
./mypandoc -i example/example.xml -f markdown

<document>
  <header title="Simple example"></header>
  <body>
    <paragraph>This is a simple example</paragraph>
  </body>
</document>
```
---
title: Simple example
---

This is a simple example


Commit NORM :

-COMMIT NORM-
<[Gitmoji] : [ELEMENT / MODULE] : [MESSAGE] >
You can find the global list of Gitmojies here: https://gitmoji.dev/

Some useful gitmojies:

    - :sparkles: (✨): Introduce new features
    - :recycle: (♻️): Refactor / update code  code
    - :bug: (🐛): Fix a bug
    - :poop: (💩) : Remove Coding style
    - :rotating_light: (🚨) : Fix Compiling Warning
    - :fire: (🔥): Remove code or files
    - :white_check_mark: (✅): Add, update, or pass tests
    - :see_no_evil: (🙈): Add or update .gitignore files
    - :construction_worker: (👷): Add or update CI build system
    ...
    // WARNING
    - :tada: (🎉): This Gitmoji must be used for each PR created!
    - :lipstick: (💄): This Gitmoji must be used for each PR merged!
    - :rewind: (⏪️): This Gitmoji must be used for each revert done!



GIT CLI important command :

- Changer message de commit, avant qu'il soit push :
    ```bash
    git commit --amend -m "New commit message"
    ```

- Changer le message de commit, si il a deja été push :
    ```bash
    git commit --amend -m "New commit message"
    git push --force
    ```

- Un-add un ficher add par erreur qui est pas encore push:
    ```bash
    git restore --staged <file>
    ```

- Un-add un fichier qui a été commit :
    ```bash
    git reset --soft HEAD~1
    git restore --staged fichier-a-retirer.txt
    git commit -m "Nouveau message de commit (sans le fichier)"
    ```

## Note 
Overall : 87.9% 

Grade A Médaille
