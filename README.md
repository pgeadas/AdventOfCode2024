## Advent of Code 2024

Welcome to my Advent of Code 2024 solutions in F#!

After taking a four-year break from F#, I decided to jump back in. It's been fun to see how the language has evolved, and I've
enjoyed picking it up again. Some of my solutions might not be the most "functional" style, but I'm pleased with what I've learned
and accomplished regardless. :)

### About the Challenges

Advent of Code is a series of fun coding puzzles you can find [here](https://adventofcode.com/2024). They get trickier as December
goes on, so it's a great way to test and improve your coding skills and algorithms and data structures knowledge.

### How This Project is Set Up

- **Solutions**: Each day's challenge has its own F# script file starting with `DayX`.
- **Input Files**: The `inputs` folder has all the challenge input data named by day.
- **Utilities**: Some shared functions/types are stored in separate modules to use easily across different days, but they surely
  deserve a cleanup (maybe later...).

### How to Use

1. Clone the repo:
   ```bash
   git clone https://github.com/yourusername/Advent2024.git
   cd Advent2024

2. Build the solutions:
   ```bash
   dotnet build
   ```

3. Run the solutions:
   ```bash
   ./Advent2024/bin/Debug/net9.0/Advent2024 <day>
   ```
