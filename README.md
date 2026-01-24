# R Minesweeper
The minesweeper game implemented in R, just because it can be.

## Instructions
1. Build the docker image `docker build -t r-minesweeper .`
2. Launch a container `docker run --rm -p 8100:8100 r-minesweeper`
3. Access the game in your browser at `127.0.0.1:8100`

Alternatively, you can just run the game from your IDE by running `main.R` (make sure the working directory is set to this repository's root)