# fp-2025

## Lesson notes

Can be viewed [here](https://vipo.github.io/fp-2025/)

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

## Domain: Dishes & Meals DSL

This DSL models the creation and manipulation of dishes.  
Main entities are:

- **Dish**: has a name and calories.  
- **BigDish**: recursive structure that combines two dishes (or other BigDishes).  

### Operations

- `CreateDish` – create a new dish.  
- `RemoveDish` – remove a dish by name.  
- `EatDish` – consume a dish or big dish.  
- `Dump Examples` – show sample commands.  

### Example Commands

```text
Dump Examples
CreateDish (Dish "pasta" 200)
CreateDish (BigDish (Dish "salad" 100) (Dish "soup" 150))
RemoveDish "pasta"
EatDish (Dish "salad" 250)
EatDish (BigDish (BigDish (Dish "salad" 100) (Dish "soup" 150)) (Dish "ice cream" 150))