# Skärgården

## Goal of the game

The Swedish state has hired you to chart the archipelago. You contracted people to scout out all the islands, but they took off with the money leaving you with only loose clues as to where the islands are. Maybe you can somehow use these hints to finalize the sea charts?

## Quick tutorial

At the start of the game, you will be given a new chart. It will look like this:
![Picture of the initial state of the chart](images/new-game.png "New game")

Your objective is to colour in all the blank tiles, using the hints in the headers and the legend. A finished chart will look something like this:
![Picture of the finished chart](images/completed-game.png "Finished chart")
Notice that each number in the headers match the number of land tiles in the respective row/column, and that each island shape in the legend is represented the correct number of times in the chart.

## Additional details

### Tiles

Tiles come in three types
- Land (Brown)
- Water (Blue)
- Blank (White)

Click the tiles to change their type. A white asterisk on a tile means that it was given as a hint, and that it cannot be changed.

#### Land tiles

Land tiles join together to form islands. An island is a set of land tiles that join together at the sides. Different islands must be completely separated by water, not even their corners can touch.
![Picture of land tiles](images/concepts-tile-land.png "Land tiles circled in red")

#### Water tiles

Water tiles separates the islands.
![Picture of water tiles](images/concepts-tile-water.png "Water tiles circled in red")

#### Blank tiles

Blank tiles indicate that you don't know yet whether the tile should be land or water. A finished chart can't have any blank tile.
![Picture of blank tiles](images/concepts-tile-blank.png "Blank tiles circled in red")

### Headers

There are headers on the top and to the left of the chart. Each header contains the number of land tiles in its respective row or column. In this image, two headers are circled: 
- A column header with the number 0, indicating that the column contains 0 land tiles (i.e. only water)
- A row header with the number 4, indicating that the row contains 4 land tiles (and 6 water tiles).
![Picture of headers with numbers](images/concepts-header-numbers.png "Two headers circled: one column header with 0 land tiles, and one row header with 4 land tiles")

### Legend