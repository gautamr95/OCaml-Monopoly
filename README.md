
Group: Sacheth Hegde, Gaurab Bhattacharya, Gautam Ramaswamy

====================================
Monopoly Rules, Clarifications, and Instructions
====================================

----------------------------------
How to Run and Compile:
----------------------------------

Move into the main directory of the project, which includes files such as game_main.ml, and game_utils.ml
Once here, compile the game using the following command (the compilation will take a few seconds):

cs3110 compile -t -p lablgtk2,async,str toplevel.ml


After the game compiles, use the following command to run the game:

cs3110 run toplevel.ml


A GUI interface will pop up. All the interaction and commands will be entered and typed through the small text input, and all directions and information will be in the larger textbox that is above the text input.
To quit the game earlier, either quit using the ‘x’ close button on the top-right of the window or click on the ‘Game” tab on the top left

-------------------------------
Clarifications and Notes
-------------------------------

As mentioned, the game will occur through a text-based interface, and you will have to play the game while using the text bar to enter text. At the bottom of the information bar (the block of text above the input bar), you will see a list of options that mention what options are available. To execute one of the commands, simply type the name of the option. For example, one of the common options is “Trade”. To execute it, please type in “trade”, “Trade”, or any case insensitive permutation and the interaction will continue.
When interacting with the text interface, make sure to READ the entire viewable block of text. Sometimes, when you perform an action, or land on a certain spot, the action that is printed (such as how much money you gained) is ABOVE the list of possible commands. So make sure to look above that to read anything necessary.
As will be mentioned, the players in our game are referenced by an ID, but keep in mind this is 0-indexed, in the spirit of most programming languages. Each character will also have an icon/token associated (which will be Obama, Bernie Sanders, etc.)
This will probably not be an issue, but if the game appears very odd, it may have to do with your screen resolution. Again, it was never an issue for us while testing, but if the screen has odd resolution, the gtk module for the GUI that we used may not display well. Try to enlarge the window.

-----------------
How to Play:
-----------------
The rules are very similar to actual monopoly. There are 4 players and the goal of the game is to make the most money within 30 turns. (The max turn number can be adjusted in game_main.ml)

The game starts with saying how many human players will be in the game. Each player is assigned a number from 0 to 3. If the number of humans is less than 4, then the last players will fill in with AI.

Obama is player 0, John Cena is player 1, Bernie Sanders is player 2 and Gaben is player 3.
Each player must roll the dice every turn. If they are on a property, they can buy the property or leave it. Our game does not execute auctions.

If you land on chance or community chest, you get a random chance or community chest card that has the ability to change the player’s money and all the other player’s money.

Landing on go to jail immediately takes you to jail. If you are in jail, on your next turn, if you roll doubles, you can get out of jail for free. Otherwise you leave jail for $30.

Passing GO gives you $200 dollars.

If you land on a property that is owned by someone else, pay them rent. More expensive properties have more rent.

If you buy all the property of a color, you can build houses on the property to increase the amount of rent that is owed.

Players can trade property as well. On a player’s turn, they can request money and property in exchange for money and property. After creating a request, it is presented to the other player and they have the option to accept.

If at any point you owe money and thus are in debt, at the end of your turn you have the option to sell you houses and trade again to get back to positive money. If you are unable to do so, then your property assets are returned to the bank and obtainable by other players, and said player is out of the game.

The game winner is the one who has the most assets at the end of 30 turns. The total assets is calculated by adding up the value of all property, houses and cash.
