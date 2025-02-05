#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SIZE 5  // Maze size

// Function prototypes
void initializeMaze(char maze[SIZE][SIZE], int *playerX, int *playerY, int *exitX, int *exitY);
void displayMaze(char maze[SIZE][SIZE]);
int movePlayer(char maze[SIZE][SIZE], int *playerX, int *playerY, char move);
int isExitReached(int playerX, int playerY, int exitX, int exitY);

int main() {
    char maze[SIZE][SIZE];
    int playerX, playerY, exitX, exitY;
    char move;
    int steps = 0;

    initializeMaze(maze, &playerX, &playerY, &exitX, &exitY);
    
    while (1) {
        displayMaze(maze);
        printf("Enter move (W/A/S/D): ");
        scanf(" %c", &move);

        if (movePlayer(maze, &playerX, &playerY, move)) {
            steps++;
            if (isExitReached(playerX, playerY, exitX, exitY)) {
                printf("Congratulations! You reached the exit in %d steps.\n", steps);
                break;
            }
        } else {
            printf("Invalid move. Try again.\n");
        }
    }

    return 0;
}

// Initialize the maze with walls and open paths
void initializeMaze(char maze[SIZE][SIZE], int *playerX, int *playerY, int *exitX, int *exitY) {
    srand(time(0));
    
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            maze[i][j] = (rand() % 4 == 0) ? '#' : '.';  // Randomly place walls
        }
    }

    *playerX = 0;
    *playerY = 0;
    *exitX = SIZE - 1;
    *exitY = SIZE - 1;
    
    maze[*playerX][*playerY] = 'P';  // Player start position
    maze[*exitX][*exitY] = 'E';      // Exit position
}

// Display the maze
void displayMaze(char maze[SIZE][SIZE]) {
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            printf("%c ", maze[i][j]);
        }
        printf("\n");
    }
}

// Move the player based on input
int movePlayer(char maze[SIZE][SIZE], int *playerX, int *playerY, char move) {
    int newX = *playerX, newY = *playerY;

    switch (move) {
        case 'W': case 'w': newX--; break;
        case 'A': case 'a': newY--; break;
        case 'S': case 's': newX++; break;
        case 'D': case 'd': newY++; break;
        default: return 0;
    }

    if (newX >= 0 && newX < SIZE && newY >= 0 && newY < SIZE && maze[newX][newY] != '#') {
        maze[*playerX][*playerY] = '.';  // Clear old position
        *playerX = newX;
        *playerY = newY;
        maze[*playerX][*playerY] = 'P';  // Update new position
        return 1;
    }
    return 0;
}

// Check if the player has reached the exit
int isExitReached(int playerX, int playerY, int exitX, int exitY) {
    return playerX == exitX && playerY == exitY;
}
