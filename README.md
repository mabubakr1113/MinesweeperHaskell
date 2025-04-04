# Minesweeper in Haskell

## Overview

This project implements a console-based version of the classic Minesweeper game using Haskell. The game features a fixed 10x10 grid with 10 bombs placed randomly. The player can reveal cells and toggle flags, with automatic cascading of reveals for empty cells. The game ends when the player either reveals a bomb (loss) or successfully reveals all non-bomb cells (win).

This game has been created completely my me Mohammad Abubakr, 2410176. I have worked on the code and documentation of the project.

## Features

- **Fixed Board Size:** 10x10 grid.
- **Bomb Placement:** 10 bombs randomly placed on the grid.
- **Gameplay Commands:**
  - `r row col` — Reveal the cell at the given coordinates.
  - `f row col` — Toggle a flag on the cell.
- **Cascading Reveal:** Automatically reveals adjacent cells if a revealed cell has a bomb count of 0.
- **Win/Loss Conditions:** Detects when all non-bomb cells are revealed (win) or when a bomb is revealed (loss).

## Installation and Build Instructions

### Prerequisites

- **GHC (Glasgow Haskell Compiler):** Ensure GHC is installed on your system.
- **Build Tool:** Use either Cabal or Stack.
- **Random Package:** The project requires the `random` library.

### Using Cabal

1. **Set up the dependency:**  
   Edit your `.cabal` file and add `random` to the `build-depends` section:
   ```cabal
   build-depends: base >=4.7 && <5,
                  random >=1.2
2. **Build the project:**   
   ```cabal   
   cabal build
2. ** Run the executable:**
   ```cabal   
   cabal run
